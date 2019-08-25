{-# language OverloadedStrings #-}
module Main where

import Numeric.Statistics.Inference.Bayes.Exact.VariableElimination (student)

import Algebra.Graph.Export (Doc, literal, render, export)
import Algebra.Graph (Graph)

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)


main :: IO ()
main = T.writeFile "graph.example.student.dot" $
           T.pack $ render $ withDigraph "student" $ export vDoc eDoc student

-- | Render a vertex
vDoc :: Char -> Doc String
vDoc x   = literal [x] <> ";\n"

-- | Render an edge
eDoc :: Char -> Char -> Doc String
eDoc x y = literal [x] <> " -> " <> literal [y] <> ";\n"

-- | Graphviz-related
withDigraph :: String -> Doc String -> Doc String
withDigraph name body =
  literal (unwords ["digraph", name, "{\n"]) <>
  body <>
  literal "}\n"
