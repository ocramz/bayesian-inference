{-# language OverloadedStrings #-}
module Main where

import Numeric.Statistics.Inference.Bayes.Exact.VariableElimination (student)

import Algebra.Graph.Export (Doc, literal, render, export)
import Algebra.Graph (Graph)

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)


-- vDoc x   = literal (show x) <> "\n"
-- eDoc x y = literal (show x) <> " -> " <> literal (show y) <> "\n"

vDoc x   = literal [x] <> ";\n"
eDoc x y = literal [x] <> " -> " <> literal [y] <> ";\n"

withDigraph :: String -> Doc String -> Doc String
withDigraph name body =
  literal (unwords ["digraph", name, "{\n"]) <>
  body <>
  literal "}\n"

main :: IO ()
main = T.writeFile "graph.example.student.dot" $
           T.pack $ render $ withDigraph "student" $ export vDoc eDoc student
