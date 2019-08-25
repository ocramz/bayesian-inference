{-# language FlexibleContexts #-}
module Numeric.Statistics.Inference.Bayes.Exact.VariableElimination where

-- import GHC.TypeNats 
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Data.Monoid (Sum(..), Product(..))

-- algebraic-graphs
import Algebra.Graph (Graph(..), vertex, edge, overlay, connect)
import qualified Algebra.Graph.Class as GC (Graph(..))
import qualified Algebra.Graph.ToGraph as TG (ToGraph(..))

-- bimap
import qualified Data.Bimap as BM
-- containers
import qualified Data.IntMap as IM
import qualified Data.Set as S
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- massiv
-- import qualified Data.Massiv.Array as A (Array, all, Comp(..), makeArray, Construct(..), Sz(..))
-- import Data.Massiv.Array (Index, Ix1(..), D, (..:), ifoldlWithin', foldlWithin', Lower, Dim(..), Source)
-- permutation
-- import qualified Data.Permute as P (permute, next, elems)

import Prelude hiding (lookup)

import Data.Permutation (Permutation, permutation, permutations, swaps)


student :: Graph Char
student =
  connect c d `overlay`
  connect d g `overlay`
  connect g h `overlay`
  connect g l `overlay`
  connect l j `overlay`
  connect j h `overlay`
  connect s j `overlay`
  connect i g `overlay`
  connect i s
  where
    c = vertex 'c'
    d = vertex 'd'
    g = vertex 'g'
    h = vertex 'h'
    i = vertex 'i'
    j = vertex 'j'
    l = vertex 'l'
    s = vertex 's'

-- hasInScope v f = not $ null $ scope f `S.intersection` S.singleton v

groupFactors :: Ord a => [Factor a] -> [[Factor a]]
groupFactors = groupBy (\f1 f2 -> length (scope f1 `S.intersection` scope f2) > 0)

scopeSize :: Factor a -> Int
scopeSize = length . scope

-- a factor is defined as a set of variables
newtype Factor a = Factor { scope :: S.Set a } deriving (Show)

factorList :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
              g -> [Factor (TG.ToVertex g)]
factorList g = (`moralFactor` g) `map` TG.vertexList g

moralFactor :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
               TG.ToVertex g -> g -> Factor (TG.ToVertex g)
moralFactor v g = Factor $ TG.preSet v g `S.union` S.singleton v




-- bidirectional mapping from rv names to array indices
newtype IxMap ix = IxMap {
  unIxMap :: BM.Bimap ix Dim
  } deriving (Eq, Show)

singleton :: ix -> Dim -> IxMap ix
singleton k ix = IxMap $ BM.singleton k ix

insert :: Ord ix => ix -> Dim -> IxMap ix -> IxMap ix
insert k ix bm = IxMap $ BM.insert k ix $ unIxMap bm

lookup :: (Ord ix, MonadThrow m) => ix -> IxMap ix -> m Dim
lookup k bm = BM.lookup k $ unIxMap bm

lookupR :: (Ord ix, MonadThrow m) => Dim -> IxMap ix -> m ix
lookupR j bm = BM.lookupR j $ unIxMap bm
