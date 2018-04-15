{-# language FlexibleContexts #-}
module Numeric.Statistics.Inference.Bayes.Approximate where

-- import Data.Bool (bool)

import Control.Monad (when, unless, replicateM)
-- import Control.Monad.State
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(..), lift)

import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..))

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

{-| Algorithm 2: MCMC-ABC (from Marjoram 2003)

Choose prior function pi()

1. theta* <- pi(theta)
2. x* <- f(x | theta*)

-}

testAbcMcmc eps n = do
  let
    prior mu = normal mu 3
    proposal mu = normal mu 5
    simulator mu sig = normal mu sig
  gen <- create
  -- | Data (fixed)
  x0s <- samples n (simulator thetaMu0 thetaVar0) gen
  abcMcmc prior proposal (`simulator` thetaVar) x0s 1000 eps 1 100 gen

  


abcMcmc :: (Fractional a, Ord a, PrimMonad m) =>
           (Double -> Prob m Double)
        -> (Double -> Prob m Double)
        -> (Double -> Prob m a)
        -> [a]
        -> Int
        -> a
        -> Double
        -> Int
        -> Gen (PrimState m)
        -> m Double
abcMcmc prior proposal simulator x0s n eps theta0 niter g =
  execStateT (replicateM niter $ abcMcmcStep prior proposal simulator x0s n eps g) theta0


abcMcmcStep :: (PrimMonad m, Ord a, Fractional a) =>
               (Double -> Prob m Double)
            -> (Double -> Prob m Double)
            -> (Double -> Prob m a)
            -> [a]
            -> Int
            -> a
            -> Gen (PrimState m)
            -> StateT Double m ()
abcMcmcStep prior proposal simulator x0s n eps g = do
  thetai <- get
  thetaStar <- lift $ sample (proposal thetai) g
  -- simulate dataset
  xStars <- lift $ samples n (simulator thetaStar) g
  if d x0s xStars <= eps
    then
      do 
        alpha <- lift $ acceptProb prior proposal thetaStar thetai g
        pa <- lift $ sample (bernoulli alpha) g
        if pa
          then put thetaStar
          else put thetai
    else put thetai


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





  
