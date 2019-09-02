module Data.Graph.Examples where

-- algebraic-graphs
import Algebra.Graph (Graph(..), vertex, overlay, connect)
-- import qualified Algebra.Graph.Class as GC (Graph(..))
-- import qualified Algebra.Graph.ToGraph as TG (ToGraph(..))

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
