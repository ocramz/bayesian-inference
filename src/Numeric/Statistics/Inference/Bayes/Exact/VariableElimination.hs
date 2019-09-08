{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFoldable #-}
{-# language LambdaCase #-}
module Numeric.Statistics.Inference.Bayes.Exact.VariableElimination where

-- import GHC.TypeNats 
import Data.List (groupBy, sort, sortBy, intercalate, intersperse)
import Data.Ord (comparing)
import Data.Foldable (foldlM, maximumBy, minimumBy)
-- import Data.Monoid (Sum(..), Product(..))

-- algebraic-graphs
import Algebra.Graph (Graph(..), vertex, edge, overlay, connect)
import qualified Algebra.Graph.Class as GC (Graph(..))
import qualified Algebra.Graph.ToGraph as TG (ToGraph(..))
-- bimap
import qualified Data.Bimap as BM
-- containers
import qualified Data.IntMap as IM
import qualified Data.Map as M (Map, empty, fromList, lookup, insert, toList)
import qualified Data.Set as S (Set, empty, singleton, union, intersection, filter, toList, member, insert, lookupGE, lookupLE, fromList)
import Data.Set ((\\))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- massiv
-- import qualified Data.Massiv.Array as A (Array, all, Comp(..), makeArray, Construct(..), Sz(..))
-- import Data.Massiv.Array (Index, Ix1(..), D, (..:), ifoldlWithin', foldlWithin', Lower, Dim(..), Source)
-- mtl
import Control.Monad.State (MonadState(..), gets)
-- permutation
-- import qualified Data.Permute as P (permute, next, elems)
-- transformers
import Control.Monad.State (State(..), runState, evalState, execState)
import Control.Monad.Trans.State (StateT(..), runStateT, evalStateT)
-- vector
import qualified Data.Vector as V

import Data.Graph.Examples (student)
import Data.Permutation (Permutation, permutation, getPermutation, permutations)

import Prelude hiding (lookup)

minimumMaxDegOrdering :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
                         g -> TG.ToVertex g -> Permutation (TG.ToVertex g)
minimumMaxDegOrdering g v =
  minimumBy compareOrderings $ permutations vs where
    vs = verticesWithout g $ S.singleton v
    compareOrderings vp1 vp2 =
      compare (maxFS $ getPermutation vp1) (maxFS $ getPermutation vp2)
    maxFS vss = maxFactorSize $ snd $ runLex (elims g vss)

-- | All vertices in the graph but a given subset
verticesWithout :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
                   g -> S.Set (TG.ToVertex g) -> [TG.ToVertex g]
verticesWithout g vs = S.toList $ TG.vertexSet g \\ vs


-- data SumProduct a =
--     SumOver a (Factor a)
--   | Product (S.Set (Factor a))

type VarId = Int
data Temp = Temp { varId :: VarId, maxFactorSize :: Int } deriving (Eq, Show)
-- newtype Lex m a = Lex { unLex :: StateT VarId m a } deriving (Functor, Applicative, Monad, MonadState VarId)
-- runLex :: Monad m => Lex m a -> m a
-- runLex lx = evalStateT (unLex lx) 0
newtype Lex a = Lex { unLex :: State Temp a } deriving (Functor, Applicative, Monad, MonadState Temp)

initTemp :: Temp
initTemp = Temp 0 0

-- | Run a 'Lex' computation and return its result along with the final 'Temp' value
runLex :: Lex a -> (a, Temp)
runLex lx = runState (unLex lx) initTemp

-- | Insert a new factor into scope and update the maximum size of scopes seen so far
insertFactor :: Factor a -> IM.IntMap (Factor a) -> Lex (IM.IntMap (Factor a))
insertFactor x mx = do
  Temp k mfs <- get
  let mx' = IM.insert k x mx
      sz = factorSize x
  put $ Temp (succ k) (max sz mfs)
  pure mx'

factorSize :: Factor a -> Int
factorSize = length

fromList :: Foldable t => t (Factor a) -> Lex (IM.IntMap (Factor a))
fromList xs = foldlM (flip insertFactor) IM.empty xs



-- | sequential sum-product elimination of graph factors, given a vertex elimination order
-- 
-- >>> runLex $ elims student "cdihg"
-- (fromList [(4,{ 'j' , 'l' , 's' }),(5,{ 'j' , 'l' , 's' })],Temp {varId = 5, maxFactorSize = 3})
elims :: (TG.ToGraph g, Foldable t, Ord (TG.ToVertex g)) =>
         g
      -> t (TG.ToVertex g)
      -> Lex (IM.IntMap (Factor (TG.ToVertex g)))
elims g vs = do
  let im0 = factorIM g
  foldlM (flip sumProductVE) im0 vs


factorIM :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
            g
         -> IM.IntMap (Factor (TG.ToVertex g))
factorIM g = IM.map (`moralFactor` g) im where
  im = IM.fromList $ zip [0..] (TG.vertexList g) 


-- | Sum-product elimination
sumProductVE :: (Ord a) => a -> IM.IntMap (Factor a) -> Lex (IM.IntMap (Factor a))
sumProductVE z pphi = insertFactor tau pphiC
  where
    pphi' = factorsContaining z pphi
    pphiC = pphi `IM.difference` pphi'
    tau = eliminate z pphi'

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
hasInScope v f = v `S.member` scope f 

factorList :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
              g -> [Factor (TG.ToVertex g)]
factorList g = (`moralFactor` g) `map` TG.vertexList g

moralFactor :: (TG.ToGraph g, Ord (TG.ToVertex g)) =>
               TG.ToVertex g -> g -> Factor (TG.ToVertex g)
moralFactor v g = Factor $ TG.preSet v g `S.union` S.singleton v



condition :: (Ord x, Foldable t, Eq e) =>
             t (x, e) -> Factor x -> Factor (Clamp x e)
condition cvs = Factor . S.fromList . map f . M.toList . condition_ cvs
  where
    f (x, je) = x := je

condition_ :: (Foldable t, Ord x) => t (x, e) -> Factor x -> M.Map x (Maybe e)
condition_ cvs f = foldl insf M.empty $ scope f
  where
    cs = clampVars cvs
    insf acc x = M.insert x (M.lookup x cs) acc

clampVars :: (Foldable t, Ord x) => t (x, e) -> M.Map x e
clampVars = foldl (flip clamp) M.empty
  where
    clamp (x, e) = M.insert x e 

-- | Notation for a potentially clamped (i.e. conditioned) variable
data Clamp x e = x := Maybe e
instance (Show x, Show e) => Show (Clamp x e) where
  show = \case
    x := Just je -> unwords ["(", show x, ":=", show je, ")"]
    x := Nothing -> show x
instance (Eq x, Eq e) => Eq (Clamp x e) where
  (x1 := v1) == (x2 := v2) = x1 == x2 && v1 == v2
instance (Ord x, Eq e) => Ord (Clamp x e) where
  (x1 := _) <= (x2 := _) = x1 <= x2


-- | a factor is defined here as a set of variables
newtype Factor a = Factor { scope :: S.Set a } deriving (Eq, Ord, Foldable)
instance Show a => Show (Factor a) where
  show (Factor fs) = unwords $ ["{"] ++ intersperse "," (show `map` S.toList fs) ++ ["}"]

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
