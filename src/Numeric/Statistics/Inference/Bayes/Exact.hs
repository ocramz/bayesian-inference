{-# language FlexibleContexts #-}
-- {-# language IncoherentInstances #-}
module Numeric.Statistics.Inference.Bayes.Exact where

-- algebraic-graphs
import Algebra.Graph
-- import Algebra.Graph.HigherKinded.Class (Graph(..))

-- massiv
import qualified Data.Massiv.Array as A (Array, all, Comp(..), makeArray, Construct(..), Sz(..))
import Data.Massiv.Array (Index, Ix1(..), D, (..:))


-- -- uniform1 :: (Integral e, Index e) => A.Comp -> e -> Prob e e
-- uniform1 :: (Integral e, Index ix, Integral ix) => A.Comp -> e -> Prob ix e
-- uniform1 s n = Prob ps pv where
--   ps = 0 ..: fromIntegral n
--   pv = A.makeArray s (A.Sz n) (const (1/ fromIntegral n))



data Prob ix e = Prob {
    probSupport :: A.Array D ix e  -- ^ The support of the distribution
  , probVector :: A.Array D ix Double  -- ^ All values must be nonzero
                   } deriving (Eq)

-- validateProb :: (Source r ix Double, Foldable (Array r ix)) =>
--                 Prob r ix e -> Bool
validateProb :: Index ix => Prob ix e -> Bool
validateProb (Prob ps pv) = q1 && q2 where
  q1 = length ps == length pv
  q2 = A.all (> 0) pv
