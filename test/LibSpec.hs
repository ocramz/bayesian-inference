module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Numeric.Statistics.Utils
import Numeric.Statistics.Inference.Bayes.Approximate
import Numeric.Math

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Numeric.Math" $ do
    it "laguerreE evaluates Laguerre polynomial L_2 in 2" $ 
      (laguerreE 2 2 :: Double) `shouldBe` (- 1)
    it "laguerre evaluates Laguerre polynomial L_2 in 2" $ 
      (laguerre 2 2 :: Double) `shouldBe` (- 1)      
    -- it "works" $ do
    --   True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x


-- | Logistic map. Parameter bounds `0 < r < 4`, `0 < x0 < 1`, chaotic behaviour for r > 3.57 
logisticMap :: (Fractional a, Ord a) => Int -> a -> a -> Either String [a]
logisticMap n r x0
  | r <= 0 || r >= 4 = Left "0 < r < 4"
  | x0 <= 0 || x0 >= 1 = Left "0 < x0 < 1"
  | otherwise = Right $ take n $ iterate (lm r) x0
  where
    lm r xn = r * xn * (1 - xn)
