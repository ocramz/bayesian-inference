module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Numeric/Statistics/Inference/Bayes/Exact/VariableElimination.hs"

        ]
