{-# language OverloadedStrings #-}
module Main where

import Data.Graph.Examples (student)

import Algebra.Graph.Export (Doc, literal, render)
import Algebra.Graph.Export.Dot (Style(..), defaultStyle, export, Attribute(..))
import Algebra.Graph (Graph)

import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)


main :: IO ()
main = T.writeFile "graph.example.student.dot" $
  T.pack $ render $ export style student

style :: Style Char (Doc String)
style = s{
      graphName = "student"
    -- , graphAttributes = ["splines" := "line"]
      }
  where
    s = defaultStyle vDoc

-- | Render a vertex
vDoc :: Char -> Doc String
vDoc x   = literal [x] 





