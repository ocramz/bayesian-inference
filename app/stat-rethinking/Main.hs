module Main where

import Data.Permutation (Permutation, permutations, getPermutationList)
import Data.Sample (Sample, fromList, filter, sample2, sample3)
import Data.Ratio (Ratio, (%))

import Prelude hiding (filter)

main :: IO ()
main = putStrLn "hello!"


-- | Posterior probability of each hypothesis
--
-- >>> posterior (fromList [1,0,1]) [fromList [0,0,0,0], fromList [1,0,0,0], fromList [1,1,0,0], fromList [1,1,1,0], fromList [1,1,1,1]]
-- [0 % 1,3 % 20,2 % 5,9 % 20,0 % 1]
posterior :: Sample Integer -> [Sample Integer] -> [Ratio Integer]
posterior spl hyps = map (% n) ws
  where
    n = sum ws
    ws = nWays spl hyps

-- | Number of ways in which the observed sample could be produced by each of the "hypotheses"
--
-- >>> nWays (fromList [1,0,1]) [fromList [0,0,0,0], fromList [1,0,0,0], fromList [1,1,0,0], fromList [1,1,1,0], fromList [1,1,1,1]]
-- [0,3,8,9,0]
nWays :: Sample Integer -- ^ observed sample
      -> [Sample Integer] -- ^ hypothesis space
      -> [Integer]
nWays spl hyps = map f hyps
  where
    f h = fromIntegral $ length $ filter (== spl) $ samples3 h

-- | All possible samples of length 3
samples3 :: Sample a -> Sample (Sample a)
samples3 xs = sample3 <$> xs <*> xs <*> xs




