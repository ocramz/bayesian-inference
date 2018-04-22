module Numeric.Math (
  -- * Distributions
  -- ** Uniform
  uniformPdf
  -- ** Exponential
  , expPdf
  -- ** Normal 
  , normalPdf , stdNormalPdf
  -- ** Gamma
  , gammaPdf
    -- * Gamma function 
  , gammaW
  -- * Laguerre polynomials
  , gLaguerre, laguerre, laguerreE
  -- * Utilities
  , binomialCoeff  
  , factorial

                    ) where

import NumHask.Algebra
import Prelude hiding (Num(..), fromIntegral, (/), (*), pi, (**), (^^), sqrt, exp, recip, sum, product)


-- | Uniform distribution over [a, b]
uniformPdf :: (Ord p, MultiplicativeInvertible p, Signed p,
      AdditiveGroup p) =>
              p -- ^ First endpoint
           -> p -- ^ Second endpoint
           -> p -- ^ Evaluation point
           -> p
uniformPdf a b x | x >= a && x <= b = recip z
                 | otherwise = zero where
                     z = abs (b - a)

-- | Normal distribution
normalPdf :: (ExpField a, TrigField a) =>
             a -- ^ Mean
          -> a -- ^ Standard deviation
          -> a -- ^ Evaluation point
          -> a
normalPdf mu sig x = recip z * exp (negate (one / two) * ((x - mu)/sig) ** two)
  where
    two = one + one
    z = sqrt (two * pi) * sig

-- | Standard normal distribution (zero mean, unit variance)
stdNormalPdf :: (ExpField a, TrigField a) => a -> a
stdNormalPdf = normalPdf zero one    

-- | Exponential probability density function
expPdf :: (Ord p, ExpField p) =>
          p -- ^ Rate parameter
       -> p -- ^ Evaluation point
       -> p
expPdf lambda x | x >= zero = lambda * exp (negate lambda * x)
                | otherwise = zero


-- | Gamma probability density function, shape-scale parametrization 
gammaPdf :: (Fractional a, Enum a, ExpField a) =>
            a -- ^ Approximation order of the Gamma function (Natural number)
         -> a -- ^ Shape parameter
         -> a -- ^ Scale parameter
         -> a -- ^ Evaluation point
         -> a
gammaPdf nmax k theta x =
  recip (gammaW nmax k * (theta ** k)) * x ** (k - one) * exp (negate x / theta)



-- | Gamma function, expressed as a Weierstrass form
--
-- NB : Not sure about the order of convergence, set the approximation order > 1e3
--
-- Reference : http://mathworld.wolfram.com/GammaFunction.html
gammaW :: (Fractional p, Enum p, ExpField p) =>
          p  -- ^ Approximation order (Natural number)
       -> p  -- ^ Evaluation point
       -> p
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
--
-- Uses the binomial coefficient
laguerreE :: (Enum a, ExpField a, Eq a, Ord a) =>
             a -- ^ Polynomial order (Natural number)
          -> a -- ^ Evaluation point
          -> a
laguerreE n x =
  sum [ (x ** k) * binomialCoeff n k * (negate one ** k) / factorial k | k <- [zero .. n]]


-- | Factorial function
factorial :: (AdditiveGroup p, Multiplicative p, Eq p, Ord p) =>
             p  -- ^ Non-negative integer
          -> p
factorial nn = recur nn where
  recur n
    | n < zero = error "Argument must be a non-negative integer"
    | n == zero = one
    | n == one = one
    | otherwise = n * recur (n - one)

-- | Binomial coefficient ("n over k")
--
-- n ! / (k ! (n - k) !)
binomialCoeff :: (AdditiveGroup a, Eq a, MultiplicativeGroup a, Ord a) =>
                 a -- ^ Natural number
              -> a -- ^ Natural number
              -> a
binomialCoeff n k = factorial n / (factorial k * factorial (n - k))
  






