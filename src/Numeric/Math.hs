module Numeric.Math where

import NumHask.Algebra
import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), exp, recip, sum, product)



-- | Gamma function, expressed as a Weierstrass form
gammaW :: (Fractional p, Enum p, ExpField p) => p -> p -> p
gammaW nmax z = recip a where
  a = z * exp (emc * z) * prods
  prods = product [(one + z/r) * exp (negate z / r) | r <- [one .. nmax]]
  -- Euler-Mascheroni constant
  emc = 0.5772156649015328606065120900824024310421593359399235988057672348848677267776646


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
  






