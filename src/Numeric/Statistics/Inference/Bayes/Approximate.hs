{-# language TypeFamilies #-}
module Numeric.Statistics.Inference.Bayes.Approximate where

import Control.Monad.Primitive (PrimMonad(..))

import System.Random.MWC.Probability (Prob(..), Gen(..), samples, create, normal, uniform)



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
x0data :: IO [Double]
x0data = do
  let n = 1000
  g <- create
  samples n (normal thetaMu0 thetaVar0) g
  

  
  










-- class RV a where
--   type RVParam a :: *
--   type Variate a :: *
--   sampleRV :: PrimMonad m => RVParam a -> MWC.Prob m (Variate a)





  
