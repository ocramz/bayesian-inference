module Numeric.Statistics.Sampling.MetropolisHastings where

import Control.Monad (when, unless, replicateM)

import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..), PrimState(..))

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(..), lift)

import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, create, normal, standardNormal, uniform, uniformR, bernoulli, Variate(..))

import NumHask.Algebra
import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), exp, recip, sum, product, sqrt)

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
  return $ min one (qxim * pic / (qcand * pixim))


-- | Metropolis acceptance function in the case of a symmetric proposal
metropolisSymmProposal :: (Monad m, Ord b, MultiplicativeGroup b) =>
                          (t -> Prob m b)
                       -> Gen (PrimState m)
                       -> t
                       -> t
                       -> m b
metropolisSymmProposal piPrior g xCand xim = do 
  pic <- sample (piPrior xCand) g
  pixim <- sample (piPrior xim) g
  return $ min one (pic / pixim)



-- | sanity check: 
--
-- Correlation parameter of two Gaussian r.v.'s
--

-- | We can generate correlated Gaussian r.v. via the Cholesky factorization of the covariance matrix.
--
-- | L X = X '
createCorrelatedData :: PrimMonad m =>
                        Double -- ^ Stddev of X
                     -> Double -- ^ Stddev of Y
                     -> Double -- ^ Correlation of X, Y
                     -> Int -- ^ # of samples
                     -> Gen (PrimState m)
                     -> m ([Double], [Double])
createCorrelatedData sxx syy rho n g = do
  let (a, b, c) = covarCholesky sxx syy rho
  x1s <- samples n standardNormal g
  x2s <- samples n standardNormal g
  let x1s' = (a *) `map` x1s
      x2s' = zipWith (\x1 x2 -> b * x1 + c * x2) x1s x2s
  return (x1s', x2s')

-- | Cholesky factor of a 2x2 covariance matrix
--  
-- [sxx rho] = [a   ] [a  b] = L L'
-- [rho syy]   [b  c] [   c]
covarCholesky :: (ExpField c) => c -> c -> c -> (c, c, c)
covarCholesky sxx syy rho = (a, b, c) where
  a = sqrt sxx
  b = rho / a
  c = sqrt (syy - rho ** (one + one) / sxx)
