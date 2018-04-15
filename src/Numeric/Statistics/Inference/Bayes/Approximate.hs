{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Numeric.Statistics.Inference.Bayes.Approximate where

-- import Data.Bool (bool)
import Data.Char (toUpper)

import Control.Monad (when, unless, replicateM)
-- import Control.Monad.State
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(..), lift)

import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..))

import Control.Monad.Log (MonadLog(..), WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, PureLoggingT(..), Handler, logInfo, logError)

import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, create, normal, uniformR, bernoulli)


{-|
References:

- Approximate Bayesian computation scheme for parameter inference and model selection in dynamical systems - https://people.eecs.berkeley.edu/~jordan/sail/readings/toni-etal.pdf (ABC-Rejection, ABC-MCMC)
-}


{- | Algorithm 1: Rejection sampling ABC (from Pritchard 1999)

Choose prior function pi()

1. theta* <- pi(theta)
2. x* <- f(x | theta*)
3. if d(x0, x) <= epsilon
          then accept theta*
          else reject theta*
4. goto 1.
-}

-- | Example 1.1 Mean value and rejection rate of the mean parameter of a Gaussian RV

-- | ground truth parameters
thetaMu0, thetaVar0 :: Double
thetaMu0 = 0.5
thetaVar0 = 1

-- |candidate parameters
thetaMu, thetaVar :: Double
thetaMu = 1
thetaVar = 2

-- | Data (fixed)
x0data :: Int -> GenIO -> IO [Double]
x0data n g = samples n (normal thetaMu0 thetaVar0) g


generativeModel :: Prob IO (Double, Double)
generativeModel = do
  thetaMuStar <- uniformR (0, 5)
  x <- normal thetaMuStar thetaVar
  return (thetaMuStar, x)
  
withinBall :: (Ord a, Num a) => a -> a -> a -> Bool
withinBall eps x0 x = abs (x - x0) <= eps

  
abcRejection :: Fractional b => Double -> Int -> GenIO -> IO (Double, b)
abcRejection eps n g = do
  x0s <- x0data n g
  xs <- samples n generativeModel g 
  let
    xtot = zip x0s xs
    xs' = filter (\(x0, (_, x)) -> withinBall eps x0 x) xtot
    thetas = map (\(_, (theta, _)) -> theta) xs'
    nf = length thetas
    rejectionRate = 1 - (fromIntegral nf / fromIntegral n)
  return (mean thetas, rejectionRate)





-- 


-- testAbcMcmc :: Double -> Int -> IO Double
testAbcMcmc eps n = do
  let
    prior mu = normal mu 3
    proposal mu = normal mu 5
    simulator mu sig = normal mu sig
  gen <- create
  --  Data (fixed)
  x0s <- samples n (simulator thetaMu0 thetaVar0) gen
  abcMcmc prior proposal (`simulator` thetaVar) x0s 1000 eps 10 100 gen

  


-- abcMcmc :: (Fractional a, Ord a, PrimMonad m, MonadLog (WithSeverity String) m) =>
--            (Double -> Prob m Double)
--         -> (Double -> Prob m Double)
--         -> (Double -> Prob m a)
--         -> [a]
--         -> Int
--         -> a
--         -> Double
--         -> Int
--         -> Gen (PrimState m)
--         -> m Double
abcMcmc prior proposal simulator x0s n eps theta0 niter g =
  execStateT (replicateM niter $ abcMcmcStep prior proposal simulator x0s n eps g) theta0

{-| Algorithm 2: MCMC-ABC 

ABC-MCMC step:

Prior distribution pi(theta)
Proposal distribution q(.|theta)
Dataset x0

1. theta* <- q( . | theta_i )
2. x* <- f( . | theta* )
3. if d(x*, x0) <= epsilon, goto 4., else goto 6.
4. if Bern(alpha) goto 5. where
   alpha = min(1, pi(theta*)q(thetai|theta*) / (pi(thetai)q(theta*|thetai)) )
5. set theta_i+1 = theta*
   goto 1.
6. set theta_i+1 = theta_i
   goto 1.

References:
- Marjoram 2003
- implemented from https://people.eecs.berkeley.edu/~jordan/sail/readings/toni-etal.pdf
- discussion of ABC-MCMC : http://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2011.01640.x/pdf


-}


-- abcMcmcStep :: (PrimMonad m, MonadLog (WithSeverity String) m, Ord a, Fractional a) =>
--                (Double -> Prob m Double)
--             -> (Double -> Prob m Double)
--             -> (Double -> Prob m a)
--             -> [a]
--             -> Int
--             -> a
--             -> Gen (PrimState m)
--             -> StateT Double m ()
abcMcmcStep prior proposal simulator x0s n eps g = do
  thetai <- get
  -- 1. sample theta* from the proposal distribution
  thetaStar <- lift $ sample (proposal thetai) g
  lift $ infoIO ["theta* = ", show thetaStar]
  -- 2. simulate dataset using theta*
  xStars <- lift $ samples n (simulator thetaStar) g
  let dCurrent = d x0s xStars
  lift $ infoIO ["d(x*, x0) = ", show dCurrent]
  if dCurrent <= eps
    then
      do 
        alpha <- lift $ acceptProb prior proposal thetaStar thetai g
        lift $ infoIO ["alpha = ", show alpha]        
        pa <- lift $ sample (bernoulli alpha) g
        lift $ infoIO ["Bern(alpha) = ", show pa]
        if pa
          then
          do
            put thetaStar
          else put thetai
    else put thetai

infoIO :: [String] -> IO ()    
infoIO ws = putStrLn $ unwords ws

-- info :: MonadLog (WithSeverity String) m => [String] -> m ()
-- info ws = logInfo $ unwords ws


acceptProb :: (Monad m, Ord b, Fractional b) =>
              (t -> Prob m b)   -- ^ Prior
           -> (t -> Prob m b)   -- ^ Proposal
           -> t                 -- ^ Candidate parameter value
           -> t                 -- ^ Current parameter value
           -> Gen (PrimState m) -- ^ Generator
           -> m b
acceptProb p q thetaStar theta gen = do
  a <- sample (p thetaStar) gen
  b <- sample (q thetaStar) gen
  c <- sample (p theta) gen
  d <- sample (q theta) gen
  let alpha = min 1 (a*b/(c*d))
  return alpha
  

-- abcMCMC eps n g = do
--   x0s <- x0data n g
--   xs <- samples n generativeModel g 

d :: (Foldable t, Fractional a) => t a -> t a -> a
d = distL1 mean

-- | L1 distance
distL1 :: Num a => (t -> a) -> t -> t -> a
distL1 f x0s xs = abs (f x0s - f xs)

-- | Mean of a list of real numbers
mean :: (Fractional a, Foldable t) => t a -> a
mean xs = 1 / fromIntegral (length xs) * sum xs  
  
  










-- class RV a where
--   type RVParam a :: *
--   type Variate a :: *
--   sampleRV :: PrimMonad m => RVParam a -> MWC.Prob m (Variate a)





  




--

bracketsUpp :: Show a => a -> String
bracketsUpp p = unwords ["[", map toUpper (show p), "]"]

withSeverity :: (t -> String) -> WithSeverity t -> String
withSeverity k (WithSeverity u a ) = unwords [bracketsUpp u, k a]
