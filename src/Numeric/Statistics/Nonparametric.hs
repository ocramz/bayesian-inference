{-# language DeriveFunctor, DeriveFoldable #-}
module Numeric.Statistics.Nonparametric (crp) where

-- import Control.Monad (foldM)
import Data.Monoid (Sum(..))

-- containers
import qualified Data.IntMap as IM
-- mwc-probability
import System.Random.MWC.Probability (Prob, create, sample, samples, Gen, categorical, multinomial)
import System.Random.MWC.Probability (gamma, normal)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))






-- | Chinese Restaurant Process, based on Griffiths, Ghahramani 2011 
crp :: PrimMonad m =>
       Double -- ^ Concentration parameter (> 1)
    -> Int -- ^ Total number of customers 
    -> Prob m [Integer]
crp a n = do
  ts <- go crpInitial 1
  pure $ map getSum $ customers ts
  where
    go acc i
      | i == n = pure acc
      | otherwise = do
          acc' <- crpSingle i acc a
          go acc' (i + 1)

-- | CRP presentation based on Griffiths, Ghahramani
crpSingle :: (PrimMonad m, Integral b) =>
             Int
          -> CRPTables (Sum b)
          -> Double
          -> Prob m (CRPTables (Sum b))
crpSingle i zs a = do
  zn1 <- categorical probs
  pure $ crpInsert zn1 zs
  where
    probs = pms <> [pm1]
    mks = getSum <$> customers zs -- # of customers sitting at each table
    pms = map (\m -> fromIntegral m / (fromIntegral i - 1 + a)) mks
    pm1 = a / (fromIntegral i - 1 + a)

-- | Tables at the Chinese Restaurant
newtype CRPTables c = CRP {
  getCRPTables :: IM.IntMap c } deriving (Eq, Show, Functor, Foldable)

-- | Initial state of the CRP : one customer sitting at table #0
crpInitial :: CRPTables (Sum Integer)
crpInitial = crpInsert 0 $ CRP IM.empty

-- | Seat one customer at table 'k'
crpInsert :: Num a => IM.Key -> CRPTables (Sum a) -> CRPTables (Sum a)
crpInsert k (CRP ts) = CRP $ IM.insertWith (<>) k (Sum 1) ts

-- -- | Number of tables
-- uniques :: CRPTables a -> Int
-- uniques (CRP ts) = length ts

-- | Number of customers sitting at each table
customers :: CRPTables c -> [c]
customers = map snd . IM.toList . getCRPTables


--


-- | Sampler for a Normal-Gamma random variate
--
-- https://en.wikipedia.org/wiki/Normal-gamma_distribution
normalGamma :: PrimMonad m =>
               Double -- ^ a > 0 
            -> Double -- ^ b > 0
            -> Double -- ^ mu
            -> Double -- ^ lambda > 0 
            -> Prob m Double
normalGamma a b mu lambda = do
  tau <- gamma a b
  let stdd = sqrt $ 1 / (lambda * tau)
  normal mu stdd
