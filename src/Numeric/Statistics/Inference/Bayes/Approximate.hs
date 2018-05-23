{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# language TypeApplications #-}
{-# language MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Numeric.Statistics.Inference.Bayes.Approximate where

-- import Data.Bool (bool)
import Data.Char (toUpper)

import Control.Monad (when, unless, replicateM, ap)
-- import Control.Monad.State
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(..), lift)

import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..), PrimState(..))

import Control.Monad.Log (MonadLog(..), WithSeverity(..), Severity(..), LoggingT(..), runLoggingT, PureLoggingT(..), Handler, logInfo, logError)

-- import System.Random.MWC (Variate(..))
import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, create, normal, uniform, uniformR, bernoulli, Variate(..))
import System.Random.MWC.Probability.Transition (Transition(..), mkTransition, runTransition)

import Numeric.Statistics.Utils
import Numeric.Math

import NumHask.Algebra
import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), exp, recip, sum, product)



-- * Utilities

-- aor eps p x0 x =
--   if withinBall eps x0 x then Right (Sample p x) else Left (Sample p x)  


data Sample p a = Sample { sParam :: p, sVal :: a } deriving (Eq, Show)






-- * The Mighty Metropolis-Hastings

mh :: (Ord s, MultiplicativeGroup s, PrimMonad m, Variate s) =>
      Prob m s
   -> (s -> Prob m s)
   -> (s -> Prob m s)
   -> Int
   -> Gen (PrimState m)
   -> m [s]
mh qPrior qCond piPrior n g = do
  x0 <- sample qPrior g
  evalStateT (replicateM n $ mhStep qCond piPrior g) x0

mhStep :: (Ord s, MultiplicativeGroup s, PrimMonad m, Variate s) =>
          (s -> Prob m s)
       -> (s -> Prob m s)
       -> Gen (PrimState m)
       -> StateT s m s
mhStep qCond piPrior g = do
  xim <- get
  xCand <- lift $ sample (qCond xim) g
  alpha <- lift $ metropolis qCond piPrior g xCand xim
  u <- lift $ sample uniform g
  let xi = if u < alpha then xCand else xim
  put xi
  return xi

metropolis :: (Monad m, Ord b, MultiplicativeGroup b) =>
              (t -> Prob m b)
           -> (t -> Prob m b)
           -> Gen (PrimState m)
           -> t
           -> t
           -> m b
metropolis qCond piPrior g xCand xim = do
  qxim <- sample (qCond xCand) g
  pic <- sample (piPrior xCand) g
  qcand <- sample (qCond xim) g
  pixim <- sample (piPrior xim) g
  return $ min one $ qxim * pic / (qcand * pixim)







-- * ABC

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
  
withinBall :: (Ord a, AdditiveGroup a, Signed a) => a -> a -> a -> Bool
withinBall eps x0 x = abs (x - x0) <= eps

  
abcRejection :: (AdditiveGroup b, MultiplicativeGroup b, FromInteger b) =>
                Double
             -> Int
             -> Gen RealWorld
             -> IO (Double, b)
abcRejection eps n g = do
  x0s <- x0data n g
  xs <- samples n generativeModel g 
  let
    xtot = zip x0s xs
    xs' = filter (\(x0, (_, x)) -> withinBall eps x0 x) xtot
    thetas = map (\(_, (theta, _)) -> theta) xs'
    nf = length thetas
    rejectionRate = one - (fromIntegral nf / fromIntegral n)
  return (mean thetas, rejectionRate)


-- testAbcMcmc :: Double -> Int -> IO [Double]
testAbcMcmc eps n = do
  let
    prior mu = normal mu 3
    proposal mu = normal mu 5
    simulator mu sig = normal mu sig
  gen <- create
  --  Data (fixed)
  x0s <- samples n (simulator thetaMu0 thetaVar0) gen
  abcMcmc prior proposal (`simulator` thetaVar) x0s 1000 eps 10 100 gen

  



-- abcMcmc :: (Show a, Ord a, Fractional a) =>
--            (Double -> Prob IO Double)
--         -> (Double -> Prob IO Double)
--         -> (Double -> Prob IO a)
--         -> [a]
--         -> Int
--         -> a
--         -> Double
--         -> Int
--         -> Gen RealWorld
--         -> IO [Double]
abcMcmc prior proposal simulator x0s n eps theta0 niter g =
  fst <$> runStateT (replicateM niter $ abcMcmcStep prior proposal simulator x0s n eps g) theta0

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

-- abcMcmcStep' prior proposal simulator x0s n eps g = undefined
--   where
--     smodel thetai = do  -- stochastic model
--       let thetaStar = proposal thetai
--       xStars <- replicateM n (simulator thetaStar)
--       let dCurrent = d x0s xStars



abcMcmcStep ::
  (Show a, Ord a, FromInteger a, MultiplicativeGroup a, Signed a, AdditiveGroup a) =>
     (Double -> Prob IO Double)
  -> (Double -> Prob IO Double)
  -> (Double -> Prob IO a)
  -> [a]
  -> Int
  -> a
  -> Gen RealWorld
  -> StateT Double IO Double
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
        let theta' = if pa then thetaStar else thetai
        put theta'
        return theta'
    else
      do
        put thetai
        return thetai
         

acceptProb :: (Monad m, Ord b, MultiplicativeGroup b) =>
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
  let alpha = min one (a*b/(c*d))
  return alpha


-- acceptProb' :: (ExpField p, TrigField p, Signed p, Fractional p, Enum p) =>
--                p     -- ^ Gamma approximation order (where necessary)
--             -> PDF p -- ^ Prior
--             -> PDF p -- ^ Proposal
--             -> p     -- ^ Candidate parameter value
--             -> p     -- ^ Current parameter value
--             -> p
-- acceptProb' nmax p q thetaStar theta = min one (a*b/(c*d)) where
--   a = density nmax p thetaStar
--   b = density nmax q thetaStar
--   c = density nmax p theta
--   d = density nmax q theta



-- | Metropolis correction, symmetric proposal distribution
acceptProbSymm :: (Monad m, Ord b, MultiplicativeGroup b) =>
                  (t -> Prob m b)   -- ^ Prior
               -> t                 -- ^ Candidate parameter value (theta*)
               -> t                 -- ^ Current parameter value
               -> Gen (PrimState m)
               -> m b
acceptProbSymm p thetaStar theta gen = do
  a <- sample (p thetaStar) gen
  b <- sample (p theta) gen
  return (a / b)
  -- let alpha = min one (a / b)
  -- return alpha
  



infoIO :: [String] -> IO ()    
infoIO ws = putStrLn $ unwords ws

-- info :: MonadLog (WithSeverity String) m => [String] -> m ()
-- info ws = logInfo $ unwords ws
  




-- | Univariate distributions which are supported over R
data PDF a =
      Normal a a
    | Uniform a a
    | Exponential a
    | Gamma a a
  deriving (Eq, Show)

density :: (ExpField a, TrigField a, Signed a, Fractional a, Enum a) =>
     a -> PDF a -> a -> a
density nmax dens x = case dens of
  Normal mu sig -> normalPdf mu sig x
  Uniform a b -> uniformPdf a b x
  Exponential lambda -> expPdf lambda x
  Gamma k theta -> gammaPdf nmax k theta x




-- | Typeclass based approach for samplign and evaluating PDF

data NormalPdf a = NormalPdf {
    normalMean :: a
  , normalStd :: a }
                           
class CDSampleable m p where
  type CDSample p :: *
  cdSample :: p -> Prob m (CDSample p)

class CDPDF p where
  type CDValue p :: *
  cdPdf :: p -> CDValue p -> CDValue p

instance PrimMonad m => CDSampleable m (NormalPdf Double) where
  type CDSample (NormalPdf Double) = Double
  cdSample (NormalPdf mu sig) = normal mu sig

instance (TrigField a, ExpField a) => CDPDF (NormalPdf a) where
  type CDValue (NormalPdf a) = a
  cdPdf (NormalPdf mu sig) = normalPdf mu sig

--


data N m a = N { nParams :: NormalPdf a, sSample :: Prob m a }

mkNormal :: PrimMonad m => Double -> Double -> N m Double
mkNormal mu sig = N (NormalPdf mu sig) (normal mu sig)






-- class RV a where
--   type RVParam a :: *
--   type Variate a :: *
--   sampleRV :: PrimMonad m => RVParam a -> MWC.Prob m (Variate a)





  




--

bracketsUpp :: Show a => a -> String
bracketsUpp p = unwords ["[", map toUpper (show p), "]"]

withSeverity :: (t -> String) -> WithSeverity t -> String
withSeverity k (WithSeverity u a ) = unwords [bracketsUpp u, k a]




{-|
References:

- Approximate Bayesian computation scheme for parameter inference and model selection in dynamical systems - https://people.eecs.berkeley.edu/~jordan/sail/readings/toni-etal.pdf (ABC-Rejection, ABC-MCMC)
-}
