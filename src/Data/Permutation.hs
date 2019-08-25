{-# language TypeApplications #-}
{- |
Finite permutations
-}
module Data.Permutation (
  Permutation, permutation, getPermutation, permutations
  -- * Helper functions
  , swaps, swapsEnum
  ) where

import Control.Monad.ST (ST(..), runST)

-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


newtype Permutation a = Permutation {
  _getPermutation :: V.Vector a
                               }

getPermutation :: Permutation a -> V.Vector a
getPermutation = _getPermutation

instance Show a => Show (Permutation a) where
  show (Permutation pv) = show $ V.toList pv

permutations :: [a] -> [Permutation a]
permutations xs = permSwap p0 `map` swaps n  where
  p0 = permutation xs
  n = length xs

permSwap :: Permutation a -> (Int, Int) -> Permutation a
permSwap pp (i1 ,i2) = runST $ do
  let pv = getPermutation pp
  pvm <- V.thaw pv
  VM.swap pvm i1 i2
  Permutation <$> V.freeze pvm

permutation :: [a] -> Permutation a
permutation xs = Permutation $ V.fromList xs

swaps :: Int -> [(Int, Int)]
swaps n = (0, 0) : [(i1, i2) | i1 <- [0 .. n-1], i2 <- [i1 + 1 .. n-1]]  

swapsEnum :: Enum b => b -> b -> [(b, b)]
swapsEnum n amin = [(a, b) | a <- [amin .. pred n], b <- [succ a .. pred n]]



-- -- -- | Hamming distance between two lists
-- -- hamming :: Eq a => [a] -> [a] -> Int
-- -- hamming l r = foldl sumb 0 $ zipWith (/=) l r where
-- --   sumb t b | b = t + 1
-- --            | otherwise = t
