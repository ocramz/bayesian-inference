module Numeric.Statistics.Utils where


-- | d uu vv = abs (mean uu - mean vv)
d :: (Foldable t, Fractional a) => t a -> t a -> a
d = distL1 mean

-- | L1 distance
distL1 :: (Num a) => (t -> a) -> t -> t -> a
distL1 f x0s xs = abs (f x0s - f xs)

-- | Mean of a list of real numbers
mean :: (Foldable t, Fractional a) => t a -> a
mean xs = recip $ fromIntegral (length xs) * sum xs  

-- | Autocorrelation as a function of the lag
acLag :: (Fractional b) => Int -> [b] -> Either String b
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
inner :: (Num a) => [a] -> [a] -> a
inner u v = sum $ zipWith (*) u v    

-- | Shift a list of numbers by a constant
shiftData :: Num b => b -> [b] -> [b]
shiftData xMu = map (\x -> x - xMu)

-- | Subtract the sample mean from a list of numbers
centerData :: (Fractional b) => [b] -> [b]
centerData xs = shiftData (mean xs) xs
