module Main where

import Data.Permutation (Permutation, permutations, getPermutationList)
import Data.Sample (Sample, fromList, filter, sample2, sample3)
import Data.Ratio (Ratio, (%))

import Prelude hiding (filter)

main :: IO ()
main = putStrLn "hello!"





-- | Ratio of observed outcomes to all possible outcomes for samples of length 3
--
-- >>> pOutcomes (fromList [1,0,1]) (fromList [1,1,0,0])
-- 1 % 8
-- >>> pOutcomes (fromList [1,0,1]) (fromList [1,0,0,0])
-- 3 % 64
pOutcomes :: Sample Int -- ^ observed sample
          -> Sample Int -- ^ hypothesis
          -> Ratio Int
pOutcomes spl hypot = nsps % ntot
  where
    xs = samples3 hypot
    nsps = length $ filter (== spl) xs
    ntot = length xs

-- | All possible samples of length 3
samples3 :: Sample Int -> Sample (Sample Int)
samples3 xs = sample3 <$> xs <*> xs <*> xs




