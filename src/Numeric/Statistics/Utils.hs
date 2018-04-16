module Numeric.Statistics.Utils where

import NumHask.Algebra
import Prelude hiding (Num(..), sum, fromIntegral, (/), (*))


-- | d uu vv = abs (mean uu - mean vv)
d :: (FromInteger a, MultiplicativeGroup a, Foldable t, Signed a, AdditiveGroup a) =>
     t a -> t a -> a
d = distL1 mean

-- | L1 distance
distL1 :: (Signed a, AdditiveGroup a) => (t -> a) -> t -> t -> a
distL1 f x0s xs = abs (f x0s - f xs)

-- | Mean of a list of real numbers
mean :: (Foldable t, MultiplicativeGroup a, FromInteger a, Additive a) => t a -> a
mean xs = one / fromIntegral (length xs) * sum xs  

-- | Autocorrelation as a function of the lag
acLag :: (FromInteger b, AdditiveGroup b, MultiplicativeGroup b) =>
     Int -> [b] -> Either String b
acLag lag ts
  | lag > nn = Left "lag cannot be > length of the data"
  | otherwise = Right $ tsNum / tsDen
  where
    nn = length ts
    mu = mean ts
    ts0 = shiftData mu ts
    tsNum = inner (take (nn - lag) ts0) (drop lag ts0)
    tsDen = inner ts0 ts0

-- | Inner product (Real)
inner :: (MultiplicativeGroup a, Additive a) => [a] -> [a] -> a
inner u v = sum $ zipWith (*) u v    

-- | Shift a list of numbers by a constant
shiftData :: AdditiveGroup b => b -> [b] -> [b]
shiftData xMu = map (\x -> x - xMu)

-- | Subtract the sample mean from a list of numbers
centerData :: (AdditiveGroup b, MultiplicativeGroup b, FromInteger b) =>
     [b] -> [b]
centerData xs = shiftData (mean xs) xs
