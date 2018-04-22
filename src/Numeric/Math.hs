module Numeric.Math where

import NumHask.Algebra
import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), exp, recip, sum, product)

-- | Incomplete Gamma function
--

-- | Upper incomplete Gamma function 
gammaIU
  :: (ExpField a, Enum a, Eq a) => a -> a -> a -> a
gammaIU nmax a x =
  (x ** a) * exp (negate x) * sum [ gLaguerre n a x / (n + one) | n <- [zero .. nmax]]

gamma :: (Eq a, Enum a, ExpField a) => a -> a -> a
gamma nmax a = gammaIU nmax a zero

gamma10 :: (Fractional a, Eq a, Enum a, ExpField a) => a -> a
gamma10 = gamma 10


-- | Generalized Laguerre polynomials
gLaguerre :: (Eq p, MultiplicativeGroup p, AdditiveGroup p) =>
             p -- ^ Polynomial order (Natural number)
          -> p -- ^ Parameter (if == 0, we get regular Laguerre polynomials)
          -> p -- ^ Evaluation point
          -> p
gLaguerre nn alpha x = recur nn where
  recur k
    | k == zero = one
    | k == one = one + alpha - x
    | otherwise = let
        km = k - one        -- k - 1 
        km2 = k - one - one -- k - 2
        kk = k + k          -- 2 k
        a = (kk - one + alpha - x) * gLaguerre km alpha x
        b = (k + alpha - one) * gLaguerre km2 alpha x
        in (a - b) / k

laguerre :: (Eq p, MultiplicativeGroup p, AdditiveGroup p) =>
            p   -- ^ Polynomial order
         -> p   -- ^ Evaluation point
         -> p
laguerre nn = gLaguerre nn zero  


-- | Laguerre polynomials, exact formulation 
laguerreE :: (Enum a, ExpField a, Eq a) => a -> a -> a
laguerreE n x =
  sum [ (x ** k) * binomialCoeff n k * (negate one ** k) / factorial k | k <- [zero .. n]]

  



factorial :: (AdditiveGroup p, Multiplicative p, Eq p) => p -> p
factorial nn = recur nn where
  recur n
    | n == zero = one
    | n == one = one
    | otherwise = n * recur (n - one)

binomialCoeff :: (AdditiveGroup a, Eq a, MultiplicativeGroup a) =>
     a -> a -> a
binomialCoeff n k = factorial n / (factorial k * factorial (n - k))
  






