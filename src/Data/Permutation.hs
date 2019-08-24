{-# language TypeApplications #-}
{- |
Finite permutations
-}
module Data.Permutation (Permutation, permutation, getPermutation, swaps, permutations) where

import Control.Monad.ST (ST(..), runST)

-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

newtype Permutation = Permutation {
  _getPermutation :: V.Vector Int
                               }
getPermutation :: Permutation -> V.Vector Int
getPermutation = _getPermutation

instance Show Permutation where
  show (Permutation pv) = show $ V.toList pv

permutations :: Int -> [Permutation]
permutations n = permSwap p0 `map` swaps n  where
  p0 = permutation n

permSwap :: Permutation -> (Int, Int) -> Permutation
permSwap pp (i1 ,i2) = runST $ do
  let pv = getPermutation pp
  pvm <- V.thaw pv
  VM.swap pvm i1 i2
  Permutation <$> V.freeze pvm

swaps :: Int -> [(Int, Int)]
swaps n = [(i1, i2) | i1 <- [0 .. n-1], i2 <- [i1 + 1 .. n-1]]

-- permutationSize :: Permutation -> Int
-- permutationSize = V.length . getPermutation

permutation :: Int -> Permutation
permutation n = Permutation $ V.fromList [0 .. n-1]

-- -- | Hamming distance between two lists
-- hamming :: Eq a => [a] -> [a] -> Int
-- hamming l r = foldl sumb 0 $ zipWith (/=) l r where
--   sumb t b | b = t + 1
--            | otherwise = t
