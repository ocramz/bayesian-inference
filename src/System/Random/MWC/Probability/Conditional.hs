{-# language DeriveFunctor #-}
module System.Random.MWC.Probability.Conditional where

import Control.Monad (when, unless, replicateM, ap)
import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..))
import System.Random.MWC.Probability (Prob(..), Gen, GenIO, samples, create, normal, uniformR, bernoulli)

-- | Conditional probability
newtype CProb p m a =
  CProb { sampleCProb :: p -> Gen (PrimState m) -> m a } deriving Functor

instance Applicative m => Applicative (CProb p m) where
  pure x = CProb $ \ _ _ -> pure x
  -- (<*>) = ap  -- doesn't work without Monad (CProb p m)

bindCP :: Monad m => CProb p m a -> (a -> Gen (PrimState m) -> m b) -> CProb p m b
bindCP (CProb fs) k = CProb $ \p g -> do 
  z <- fs p g
  k z g

-- instance Monad m => Applicative (CProb p m) where
--   pure x = CProb $ \ _ _ -> pure x
--   (<*>) = ap
  -- CProb ff <*> CProb fx

-- instance Monad m => Monad (CondProb p m) where

-- instance MonadTrans (CondProb p) where


-- instance Monad m => Applicative (CondProb m p) where
--   pure = CondProb . const . pure
--   (<*>) = ap

-- instance Monad m => Monad (CondProb m p) where
--   return = pure
