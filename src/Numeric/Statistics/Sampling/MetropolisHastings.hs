module Numeric.Statistics.Sampling.MetropolisHastings where

-- import Control.Monad (when, unless, replicateM)
import Control.Monad (replicateM)

-- import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..), PrimState(..))

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (MonadTrans(..), lift)

-- import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, create, normal, standardNormal, uniform, uniformR, bernoulli, Variate)
import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, standardNormal, uniform, uniformR, Variate)
import System.Random.MWC.Probability.Transition -- (Transition(..), mkTransition, runTransition)
import qualified Control.Monad.Log as L

-- import NumHask.Algebra
-- import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), exp, recip, sum, product, sqrt, log)



testMH :: Int
       -> Int
       -> Double
       -> GenIO
       -> IO (Double, Double)
testMH n nBurn rhoTrue g = do
  x12s <- sample (createCorrelatedData' 1 1 rhoTrue n) g
  -- let qProposal rhoi = uniformR (rhoi - 0.02, rhoi + 0.02)
  let qProposal rhoi = log <$> uniformR (rhoi - 0.02, rhoi + 0.02)  
      piPost = postLogProb x12s
      logh _ = pure ()
  -- xdats <- drop nBurn <$> mh' logh uniform qProposal piPost n g
  xdats <- drop nBurn <$> mh' logh uniform qProposal piPost n g  
  return (mean xdats, variance xdats)





-- * The Mighty Metropolis-Hastings

mh' :: (Variate a, Show a, Ord a, Floating a, PrimMonad m) =>
       L.Handler m String
    -> Prob m a
    -> (a -> Prob m a)
    -> (a -> Prob m a)
    -> Int
    -> Gen (PrimState m)
    -> m [a]
mh' lh qPrior qProposal piPost n g = do
  x0 <- sample qPrior g
  evalTransition lh (mhStepLog' qProposal piPost) n x0 g
  

-- | Metropolis-Hastings step in terms of 'Transition'
mhStep' :: (Variate a, Show a, Ord a, Fractional a, PrimMonad m) =>
           (a -> Prob m a)
        -> (a -> Prob m a)
        -> Transition String a m a
mhStep' qProposal piPost = mkTransition smodel transition msgf where
  smodel xim = do 
    xCand <- qProposal xim  -- sample from the proposal distribution
    alpha <- metropolis' qProposal piPost xCand xim -- evaluate acceptance probability with Metropolis rule
    u <- uniform
    pure (xCand, alpha, u)
  transition xim (xCand, alpha, u) = (s', s')
    where
      s' = if u < alpha then xCand else xim
  msgf s _ = show s


-- | Metropolis rule , in applicative form
metropolis' :: (Applicative f, Ord b, Fractional b) =>
               (t -> f b)
            -> (t -> f b)
            -> t
            -> t
            -> f b  
metropolis' qProposal piPrior xCand xim = f <$>
                                          qProposal xCand <*>
                                          piPrior xCand <*>
                                          qProposal xim <*>
                                          piPrior xim
  where    
    f qxim pic qcand pixim = min 1 (qxim * pic / (qcand * pixim))







-- | Metropolis-Hastings step in terms of 'Transition' , log-scale
mhStepLog' :: (Variate a, Show a, Ord a, Floating a,
               PrimMonad m) =>
              (a -> Prob m a)
           -> (a -> Prob m a)
           -> Transition String a m a
mhStepLog' qProposal piPost = mkTransition smodel transition msgf where
  smodel xim = do 
    xCand <- log <$> qProposal xim  -- sample from the proposal distribution
    alpha <- metropolis' qProposal piPost xCand xim -- evaluate acceptance probability with Metropolis rule
    u <- log <$> uniform
    pure (xCand, alpha, u)
  transition xim (xCand, alpha, u) = (s', s')
    where
      s' = if u < alpha then xCand else xim
  msgf s _ = show s    


-- | Metropolis rule , in applicative form , log-scale 
metropolisLog' :: (Applicative f, Ord b, Floating b) =>
                  (t -> f b)
               -> (t -> f b)
               -> t
               -> t
               -> f b  
metropolisLog' qProposal piPrior xCand xim = f <$>
                                          qProposal xCand <*>
                                          piPrior xCand <*>
                                          qProposal xim <*>
                                          piPrior xim
  where
    f qxim pic qcand pixim = min 0 (log qxim + log pic - log qcand - log pixim)  -- NB: log-domain







mh :: (Ord s, Fractional s, PrimMonad m, Variate s) =>
      Prob m s
   -> (s -> Prob m s) -- ^ Proposal distribution
   -> (s -> Prob m s)
   -> Int
   -> Gen (PrimState m)
   -> m [s]
mh qPrior qProposal piPost n g = do
  x0 <- sample qPrior g
  evalStateT (replicateM n $ mhStep qProposal piPost g) x0

mhStep :: (Ord s, Fractional s, Variate s, PrimMonad m) =>
          (s -> Prob m s)
       -> (s -> Prob m s)
       -> Gen (PrimState m)
       -> StateT s m s
mhStep qProposal piPost g = do
  xim <- get
  xCand <- lift $ sample (qProposal xim) g  -- sample from the proposal distribution
  alpha <- lift $ metropolis qProposal piPost g xCand xim -- evaluate acceptance probability with Metropolis rule
  u <- lift $ sample uniform g
  let xi = if u < alpha then xCand else xim
  put xi
  return xi


   
  
-- | Metropolis rule
metropolis :: (Monad m, Ord b, Fractional b) =>
              (t -> Prob m b) -- ^ Proposal distribution
           -> (t -> Prob m b)  
           -> Gen (PrimState m)
           -> t
           -> t
           -> m b
metropolis qProposal piPost g xCand xim = do
  qxim <- sample (qProposal xCand) g
  pic <- sample (piPost xCand) g
  qcand <- sample (qProposal xim) g
  pixim <- sample (piPost xim) g
  return $ min 1 (qxim * pic / (qcand * pixim))


-- | Metropolis acceptance function in the case of a symmetric proposal
metropolisSymmProposal :: (Monad m, Ord b, Fractional b) =>
                          (t -> Prob m b)
                       -> Gen (PrimState m)
                       -> t
                       -> t
                       -> m b
metropolisSymmProposal piPost g xCand xim = do 
  pic <- sample (piPost xCand) g
  pixim <- sample (piPost xim) g
  return $ min 1 (pic / pixim)



-- * sanity check: 
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

createCorrelatedData' :: PrimMonad m =>
                         Double
                      -> Double
                      -> Double
                      -> Int
                      -> Prob m ([Double], [Double])
createCorrelatedData' sxx syy rho n = do
  let (a, b, c) = covarCholesky sxx syy rho
  x1s <- replicateM n standardNormal
  x2s <- replicateM n standardNormal
  let x1s' = (a *) `map` x1s
      x2s' = zipWith (\x1 x2 -> b * x1 + c * x2) x1s x2s
  return (x1s', x2s')  

-- | Cholesky factor of a 2x2 covariance matrix
--  
-- [sxx rho] = [a   ] [a  b] = L L'
-- [rho syy]   [b  c] [   c]
covarCholesky :: (Floating c) => c -> c -> c -> (c, c, c)
covarCholesky sxx syy rho = (a, b, c) where
  a = sqrt sxx
  b = rho / a
  c = sqrt (syy - rho ** 2 / sxx)

-- | Posterior probability
--
-- NB : the result is numerically small so it's best to rewrite in terms of log-probabilities
postProb :: PrimMonad m => Double -> Int -> Gen (PrimState m) -> m Double
postProb rho n g = do
  (x1s, x2s) <- createCorrelatedData 1 1 rho n g
  let lh = product $ zipWith (lhSingle rho) x1s x2s
  pure $ rhoPrior rho * lh

lhSingle :: (Floating a) => a -> a -> a -> a
lhSingle rho xi yi = 1 / (2 * pi * sqrt s) * exp (negate (sqr xi - 2 * rho * xi * yi + sqr yi)/(2 * s)) where
  s = 1 - sqr rho

-- | Posterior Log-probability
-- postLogProb
--   :: PrimMonad m => Double -> Int -> Prob m Double
-- postLogProb rho n = do
--   (x1s, x2s) <- createCorrelatedData' 1 1 rho n
--   let llh = sum $ zipWith (\xi yi -> logLhSingle rho xi yi) x1s x2s
--   pure $ log (rhoPrior rho) + llh

postLogProb :: (Applicative f, Floating a) =>
               ([a], [a])  -- ^ observed data
            -> a           -- ^ candidate parameter value
            -> f a
postLogProb (x1s, x2s) rho = do
  let llh = sum $ zipWith (logLhSingle rho) x1s x2s
  pure $ log (rhoPrior rho) + llh


logLhSingle :: Floating a => a -> a -> a -> a
logLhSingle rho xi yi = log (1 / (2 * pi * sqrt s)) + (negate (sqr xi - 2 * rho * xi * yi + sqr yi)/(2 * s)) where
  s = 1 - sqr rho

-- | Jeffreys' prior on correlation parameter 'rho' of two standard Normal rv's (non-informative prior for covariance matrices)
rhoPrior :: Floating a => a -> a
rhoPrior rho = 1 / (1 - sqr rho) ** (3 / 2)





-- * Helpers


mean :: (Foldable t, Fractional a) =>
        t a
     -> a
mean xs = sum xs / fromIntegral (length xs)

variance :: (Fractional a) =>
            [a]
         -> a
variance xs = mean (zipWith (*) xc xc) where
  xc =  (\x -> x - mean xs) `map` xs



sqr :: Num a => a -> a
sqr x = x * x


