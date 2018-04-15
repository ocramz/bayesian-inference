{-# language TypeFamilies #-}
module Numeric.Statistics.Inference.Bayes.Approximate where

import Data.Bool (bool)

-- import GHC.Prim
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad

import System.Random.MWC.Probability (Prob(..), Gen(..), GenIO, samples, create, normal, uniform, uniformR)



{- | Algorithm 1: Rejection sampling ABC (from Pritchard 1999)

Choose prior function pi()

1. theta* <- pi(theta)
2. x* <- f(x | theta*)
3. if d(x0, x) <= epsilon
          then accept theta*
          else reject theta*
4. goto 1.

-}

-- | Example 1.1 Find rejection rate of a mean parameter of a Gaussian RV

-- ground truth parameters
thetaMu0 = 0.5
thetaVar0 = 1
-- candidate parameters
thetaMu = 1
thetaVar = 2

-- | Data (fixed)
x0data :: Int -> GenIO -> IO [Double]
x0data n g = samples n (normal thetaMu0 thetaVar0) g

-- prior :: Prob IO Double
-- prior = uniformR (0, 3)

generativeModel :: Prob IO (Double, Double)
generativeModel = do
  thetaStar <- uniformR (0, 3)
  x <- normal thetaStar 5
  return (thetaStar, x)
  
withinBall :: (Ord a, Num a) => a -> a -> a -> Bool
withinBall eps x0 x = abs (x - x0) <= eps

-- keetp eps x0 (theta, x) =
--   bool (Just theta) Nothing (withinBall eps x0 x)
  
test :: Double -> Int -> GenIO -> IO [(Double, (Double, Double))]
test eps n g = do
  x0s <- x0data n g
  xs <- samples n generativeModel g 
  let
    xtot = zip x0s xs
  pure $ filter (\(x0, (_, x)) -> withinBall eps x0 x) xtot
  
  










-- class RV a where
--   type RVParam a :: *
--   type Variate a :: *
--   sampleRV :: PrimMonad m => RVParam a -> MWC.Prob m (Variate a)





  
