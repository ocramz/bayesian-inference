{-# language DeriveDataTypeable #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad (unless)
import Control.Exception (Exception(..))
import Data.Typeable
import Control.Monad.Catch (MonadThrow(..), throwM)

import Numeric.Statistics.Utils
import Numeric.Statistics.Inference.Bayes.Approximate
import Numeric.Math


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Numeric.Math" $ do
    it "laguerre 2 2 == laguerreE 2 2 : evaluates Laguerre polynomial L_2 in 2" $ 
      (laguerreE 2 2 :: Double) `shouldBeAbout` laguerre 2 2
    it "laguerre 3 2 == laguerreE 3 2 : evaluates Laguerre polynomial L_3 in 2" $ 
      (laguerreE 3 2 :: Double) `shouldBeAbout` laguerre 3 2  


-- | Logistic map. Parameter bounds `0 < r < 4`, `0 < x0 < 1`, chaotic behaviour for r > 3.57 
logisticMap :: (Ord a, Num a) =>
     Int -> a -> a -> Either String [a]
logisticMap n r x0
  | r <= 0 || r >= 4 = Left "0 < r < 4"
  | x0 <= 0 || x0 >= 1 = Left "0 < x0 < 1"
  | otherwise = Right $ take n $ iterate (lm r) x0
  where
    lm r xn = r * xn * (1 - xn)




-- | Hspec + NumHask

shouldBeAbout :: (Show a, Epsilon a, Ord a) => a -> a -> IO ()
shouldBeAbout actual expected = 
  unless (actual `aboutEqual` expected) $
    throwM $ NotAboutEqual ("expected: " ++ show expected ++ "\n but got: " ++ show actual)

-- | Floating point exceptions
data FPE = NotAboutEqual String deriving (Eq, Show, Typeable)
instance Exception FPE


class Num a => Epsilon a where
  epsilon :: a

aboutEqual :: (Epsilon a, Ord a) => a -> a -> Bool
aboutEqual x y = abs (x - y) <= epsilon

instance Epsilon Double where
  epsilon = 1e-12

instance Epsilon Float where
  epsilon = 1e-6
