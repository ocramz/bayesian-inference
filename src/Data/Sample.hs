{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Sample (Sample, empty, cons, fromList
                   , filter
                   , sample2, sample3) where

import Data.Foldable (Foldable(..))
import qualified Data.Sequence as S
import Prelude hiding (filter)


-- | Finite sample, internally represented as a 'S.Seq' (i.e. a finger tree)
newtype Sample a = Sample {
  getSample_ :: S.Seq a
  } deriving (Eq, Functor, Applicative, Foldable)
instance Show a => Show (Sample a) where
  show (Sample xs) = show $ toList xs

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



