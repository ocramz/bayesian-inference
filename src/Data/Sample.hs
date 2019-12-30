{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Sample (Sample, empty, cons, fromList
                   , filter
                   , sample2, sample3) where

import Data.Foldable (Foldable(..))
import Control.Monad.ST (ST, runST)
-- containers
import qualified Data.Sequence as S
-- mwc-probability
import System.Random.MWC.Probability (Gen(..), GenIO, GenST, create, asGenST)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- sampling
import qualified Numeric.Sampling as NS (presample, presampleIO)

import Prelude hiding (filter)


-- | Finite sample, internally represented as a 'S.Seq' (i.e. a finger tree)
newtype Sample a = Sample {
  getSample_ :: S.Seq a
  } deriving (Eq, Functor, Applicative, Foldable)
instance Show a => Show (Sample a) where
  show (Sample xs) = "{" <> unwords (map show $ toList xs) <> "}"

-- | Sample with replacement according to a (nonuniform) probability vector
presample :: (PrimMonad f) =>
             Int -- ^ number of desired samples
          -> [Double] -- ^ probability vector (must be same size as sample)
          -> Sample a
          -> Gen (PrimState f) -- ^ PRNG
          -> f (Maybe [a]) -- ^ returns Nothing iff probability vector size does not match sample size
presample n ps sv gen
  | length sv == length ps = Just <$> NS.presample n ppv gen
  | otherwise = pure Nothing
  where
    ppv = zip ps (toList sv)

sample2 :: a -> a -> Sample a
sample2 a b = a `cons` (b `cons` empty)
{-# INLINE sample2 #-}

sample3 :: a -> a -> a -> Sample a
sample3 a b c = fromList [a, b, c]
{-# INLINE sample3 #-}

-- | Filter a sample according to a predicate
filter :: (a -> Bool) -> Sample a -> Sample a
filter q (Sample s) = Sample $ S.filter q s

-- | Empty sample
empty :: Sample a
empty = Sample S.empty

-- | O(1) Left append
cons :: a -> Sample a -> Sample a
x `cons` s = Sample $ x S.<| getSample_ s
{-# INLINE cons #-}

fromList :: [a] -> Sample a
fromList = Sample . S.fromList



