module Solsys.RFactor (RFactor, toRFactor) where

import Primes.Factorization
import Primes.Pix
import Primes.Logint

data RFactor = 
    RFactor { value        :: Integer
            , primefactors :: [Integer]
            , nthprime     :: Integer
            } deriving Show

toRFactor :: Integer -> RFactor
toRFactor i = RFactor v f n
    where
    v = i
    f = factorize i 
    n = if abovePixThreshold i then logint i else pix i

nthPrimeIsAccurate :: RFactor -> Bool
nthPrimeIsAccurate (RFactor v _ _) = abovePixThreshold v

abovePixThreshold :: Integer -> Bool
abovePixThreshold i = i > (10 ^ 13)
