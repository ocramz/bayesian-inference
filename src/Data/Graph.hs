{-# language LambdaCase #-}
module Data.Graph where

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S


data G a =
    Node a
  | Edge (G a) (G a)
  deriving (Eq, Show)

node :: a -> G a
node = Node

edge :: G a -> G a -> G a
edge = Edge

-- data G a = G {
--     gNodes :: IM.IntMap a
--   , gEdges :: M.Map a (IM.IntMap a)
--              } deriving (Eq, Show)

-- -- empty :: G a
-- -- empty = G S.empty M.empty

-- connect a b (G gnodes gedges) = undefined where
--    gnodes' = IM.fromList [a, b] `IM.intersection` gnodes
--    -- gedges' = M.alter f 
-- --   gedges' = M.alter f a gedges where
-- --     f = \case
-- --       Just n -> S.intersection n gedges


