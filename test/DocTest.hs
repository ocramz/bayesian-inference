module Main where

import Test.DocTest (doctest)


main = doctest [
  "test/Bayes/Exact/VariableElimination.hs"

        ]
