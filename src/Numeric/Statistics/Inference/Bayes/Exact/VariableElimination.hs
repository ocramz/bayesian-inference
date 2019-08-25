{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
module Numeric.Statistics.Inference.Bayes.Exact.VariableElimination where

-- import GHC.TypeNats 
import Data.List (groupBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Foldable (foldlM, maximumBy)
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
-- mtl
import Control.Monad.State (MonadState(..))
-- permutation
-- import qualified Data.Permute as P (permute, next, elems)
-- transformers
import Control.Monad.State (State(..), evalState)
-- import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
-- vector
import qualified Data.Vector as V

import Prelude hiding (lookup)

import Data.Permutation (Permutation, permutation, getPermutation, permutations)


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

{- |

λ> permutations <$> TG.topSort student

Just ["iscdgljh","sicdgljh","csidgljh","dscigljh","gscdiljh","lscdgijh","jscdglih","hscdglji","icsdgljh","idcsgljh","igcdsljh","ilcdgsjh","ijcdglsh","ihcdgljs","isdcgljh","isgdcljh","isldgcjh","isjdglch","ishdgljc","iscgdljh","isclgdjh","iscjgldh","ischgljd","iscdlgjh","iscdjlgh","iscdhljg","iscdgjlh","iscdghjl","iscdglhj"]
-}


-- data SumProduct a =
--     SumOver a (Factor a)
--   | Product (S.Set (Factor a))

type VarId = Int
-- newtype Lex m a = Lex { unLex :: StateT VarId m a } deriving (Functor, Applicative, Monad, MonadState VarId)
-- runLex :: Monad m => Lex m a -> m a
-- runLex lx = evalStateT (unLex lx) 0
newtype Lex a = Lex { unLex :: State VarId a } deriving (Functor, Applicative, Monad, MonadState VarId)
runLex :: Lex a -> a
runLex lx = evalState (unLex lx) 0

insert :: a -> IM.IntMap a -> Lex (IM.IntMap a)
insert x mx = do
  k <- get
  let mx' = IM.insert k x mx
  put $ succ k
  pure mx'

fromList :: Foldable t => t a -> Lex (IM.IntMap a)
fromList xs = foldlM (flip insert) IM.empty xs

-- | sequential sum-product elimination of graph factors
-- 
-- >>> elims student "cdihg"
-- fromList [(5,{ 'j' 'l' 's' }),(12,{ 'j' 'l' 's' })]
elims :: (TG.ToGraph g, Foldable t, Ord (TG.ToVertex g)) =>
         g
      -> t (TG.ToVertex g)
      -> IM.IntMap (Factor (TG.ToVertex g))
elims g vs = runLex $ do
  im0 <- factorIM g
  foldlM (flip spe) im0 vs

factorIM :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
            g
         -> Lex (IM.IntMap (Factor (TG.ToVertex g)))
factorIM g = do
  im <- fromList $ TG.vertexList g
  pure $ IM.map (`moralFactor` g) im

-- | Sum-product elimination
spe :: (Ord a) => a -> IM.IntMap (Factor a) -> Lex (IM.IntMap (Factor a))
spe z pphi = insert tau pphiC where
  pphi' = factorsContaining z pphi
  pphiC = pphi `IM.difference` pphi'
  tau = eliminate z pphi'

forgetIndices :: Ord a => IM.IntMap a -> S.Set a
forgetIndices = S.fromList . map snd . IM.toList 

-- | Factors containing a given variable
factorsContaining :: Ord a => a -> IM.IntMap (Factor a) -> IM.IntMap (Factor a)
factorsContaining v = IM.filter (hasInScope v)

eliminate :: (Foldable t, Ord a) => a -> t (Factor a) -> Factor a
eliminate v fs = sumOver v $ intermediateFactor fs

-- | Intermediate factor (formally the set union of the given factor scopes)
-- called `psi_i` in {Koller Friedman, Algorithm 9.1, p. 298}
intermediateFactor :: (Foldable t, Ord a) => t (Factor a) -> Factor a
intermediateFactor fs = Factor $ foldl insf f0 fs where
  f0 = S.empty
  insf acc f = acc `S.union` scope f

-- | "Marginalize" a factor over a variable ; in this case this just means: filter it out
sumOver :: Eq a => a -> Factor a -> Factor a
sumOver v f = Factor $ S.filter (/= v) $ scope f 

-- | Does the factor have a given variable in scope?
hasInScope :: Ord a => a -> Factor a -> Bool
hasInScope v f = not $ null $ scope f `S.intersection` S.singleton v

moralFactor :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
               TG.ToVertex g -> g -> Factor (TG.ToVertex g)
moralFactor v g = Factor $ TG.preSet v g `S.union` S.singleton v

-- a factor is defined as a set of variables
newtype Factor a = Factor { scope :: S.Set a } deriving (Eq, Ord, Foldable)
instance Show a => Show (Factor a) where
  show (Factor fs) = unwords $ ["{"] ++ show `map` S.toList fs ++ ["}"]

-- | Induced width
width :: (Foldable t, Functor t) => t (Factor a) -> Int
width = maximum . fmap length






-- -- bidirectional mapping from rv names to array indices
-- newtype IxMap ix = IxMap {
--   unIxMap :: BM.Bimap ix Dim
--   } deriving (Eq, Show)

-- singleton :: ix -> Dim -> IxMap ix
-- singleton k ix = IxMap $ BM.singleton k ix

-- insert :: Ord ix => ix -> Dim -> IxMap ix -> IxMap ix
-- insert k ix bm = IxMap $ BM.insert k ix $ unIxMap bm

-- lookup :: (Ord ix, MonadThrow m) => ix -> IxMap ix -> m Dim
-- lookup k bm = BM.lookup k $ unIxMap bm

-- lookupR :: (Ord ix, MonadThrow m) => Dim -> IxMap ix -> m ix
-- lookupR j bm = BM.lookupR j $ unIxMap bm
