{-# language FlexibleContexts #-}
module Numeric.Statistics.Inference.Bayes.Exact.VariableElimination where

-- import GHC.TypeNats 

-- algebraic-graphs
import Algebra.Graph (Graph(..), vertex, edge, overlay, connect)
import qualified Algebra.Graph.Class as GC (Graph(..))
import Algebra.Graph.ToGraph (ToGraph(..))

-- bimap
import qualified Data.Bimap as BM
-- containers
import qualified Data.Set as S
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- massiv
import qualified Data.Massiv.Array as A (Array, all, Comp(..), makeArray, Construct(..), Sz(..))
import Data.Massiv.Array (Index, Ix1(..), D, (..:), ifoldlWithin', foldlWithin', Lower, Dim(..), Source)

import Prelude hiding (lookup)


-- a factor is defined as a set of variables
newtype Factor a = Factor { scope :: S.Set a } deriving (Show)

moralize :: (ToGraph t, Ord (ToVertex t)) =>
            ToVertex t -> t -> Factor (ToVertex t)
moralize v g = Factor $ preSet v g `S.union` S.singleton v


-- mapping from rv names to array indices
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
