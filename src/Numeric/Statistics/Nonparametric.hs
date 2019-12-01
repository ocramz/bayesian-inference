{-# language DeriveFunctor, DeriveFoldable #-}
module Numeric.Statistics.Nonparametric where

import Control.Monad (foldM)
import Data.Monoid (Sum(..))

-- containers
import qualified Data.IntMap as IM
-- mwc-probability
import Control.Monad (replicateM)
import System.Random.MWC.Probability (Prob, create, sample, samples, Gen, GenIO, GenST, categorical)
-- primitive
import Control.Monad.Primitive (PrimState(..), PrimMonad(..))



py :: (PrimMonad f) =>
      Double
   -> Double
   -> Int
   -> Gen (PrimState f)
   -> f (Tables (Sum Integer))
py a b n gen = go initial (n - 1) where
  go acc 0 = pure acc
  go acc i = do
    acc' <- sample (pitmanYor a b acc) gen
    go acc' (i - 1)


pitmanYor :: (PrimMonad m, Integral a) =>
             Double -- a \in [0, 1]
          -> Double -- b > 0
          -> Tables (Sum a)
          -> Prob m (Tables (Sum a))
pitmanYor a b zs = do
  zn1 <- categorical (pms <> [pm1])
  pure $ insert zn1 zs
  where
    m = fromIntegral $ uniques zs
    n = fromIntegral $ numCustomers zs
    d = n + b
    pm1 = (m * a + b) / d
    pms = map (\x -> (fromIntegral (getSum x) - a) / d) $ customers zs




newtype Tables c = Tables {
  getTables :: IM.IntMap c } deriving (Eq, Show, Functor, Foldable)

initial :: Tables (Sum Integer)
initial = insert 0 $ Tables IM.empty

insert :: Num a => IM.Key -> Tables (Sum a) -> Tables (Sum a)
insert k (Tables ts) = Tables $ IM.insertWith (<>) k (Sum 1) ts

fromList :: (Foldable t, Num a) =>
            t IM.Key
         -> Tables (Sum a)
         -> Tables (Sum a)
fromList xs z = foldl (flip insert) z xs

uniques :: Tables a -> Int
uniques (Tables ts) = length ts

customers :: Tables c -> [c]
customers = map snd . IM.toList . getTables

numCustomers :: (Foldable t, Functor t, Num a) => t (Sum a) -> a
numCustomers = sum . fmap getSum

-- no bound check
kthTableUnsafe :: Num a => IM.Key -> Tables (Sum a) -> a
kthTableUnsafe k (Tables ts) = maybe 0 getSum (IM.lookup k ts)

kthTable :: Num a => Int -> Tables (Sum a) -> Maybe a
kthTable k ts
  | k >= 0 = Just $ kthTableUnsafe k ts
  | otherwise = Nothing 
