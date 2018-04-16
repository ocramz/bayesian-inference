module Main where

import Test.Hspec
import Test.Hspec.QuickCheck


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
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
