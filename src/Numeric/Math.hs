module Numeric.Math where

-- | Incomplete Gamma function
--

-- | Upper incomplete Gamma



gammaIU
  :: (ExpField a, Enum a, Eq a, Fractional a) => a -> a -> a -> a
gammaIU nmax a x =
  (x ** a) * exp (negate x) * sum [ gLaguerre n a x / n + 1 | n <- [0 .. nmax]]

gamma :: (Fractional a, Eq a, Enum a, ExpField a) => a -> a -> a
gamma nmax a = gammaIU nmax a 0.1  

gamma10 :: (Fractional a, Eq a, Enum a, ExpField a) => a -> a
gamma10 = gamma 10


-- | Generalized Laguerre polynomials

gLaguerre :: (Eq p, Fractional p, MultiplicativeGroup p, AdditiveGroup p) =>
             p   -- ^ Polynomial order (Natural number)
          -> p   -- ^ Parameter (if == 0, we get regular Laguerre polynomials)
          -> p   -- ^ Evaluation point
          -> p
gLaguerre nn alpha x = case nn of
  0 -> 1
  1 -> 1 + alpha - x
  k -> let
    a = (2 * (k - 1) + 1 + alpha - x) * gLaguerre (k - 1) alpha x
    b = (k + alpha) * gLaguerre (k - 2) alpha x 
    in (a - b) / k 

laguerre :: (Eq p, Fractional p, MultiplicativeGroup p, AdditiveGroup p) =>
            p   -- ^ Polynomial order
         -> p   -- ^ Evaluation point
         -> p
laguerre nn = gLaguerre nn 0    



factorial :: (AdditiveGroup p, Multiplicative p, Eq p) => p -> p
factorial nn = recur nn where
  recur n
    | n == zero = one
    | n == one = one
    | otherwise = n * recur (n - one)

binomialCoeff :: (AdditiveGroup a, Eq a, MultiplicativeGroup a) =>
     a -> a -> a
binomialCoeff n k = factorial n / (factorial k * factorial (n - k))
  

-- | Laguerre polynomials, exact formulation 
laguerreE :: (Enum a, ExpField a, Eq a) => a -> a -> a
laguerreE n x =
  sum [ (x ** k) * binomialCoeff n k * (negate one ** k) / factorial k | k <- [zero .. n]]
