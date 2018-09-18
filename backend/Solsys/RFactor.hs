{- RFactor is a flat structure which stores the prime facorization of a given
 - number and the number of primes preceding it.
 -}
module Solsys.RFactor (RFactor (..), toRFactor) where

import Primes.Factorization
import Primes.Pix
import Primes.Logint

import Data.List

data RFactor = 
    RFactor { -- Original value that was factored
              value        :: Integer
            , -- Prime factors of the number, excluding 1 and itself
              primefactors :: [(Integer, Integer)]
            , -- Number of primes before the number, not including itself
              nthprime     :: Integer
            } deriving Show

-- Constructing an RFactor out of an Integer

toRFactor :: Integer -> RFactor
toRFactor i = RFactor v f n
    where
    v = i
    f = collectPowers $ exceptPrimes $ factorize i 
    n = (if abovePixThreshold i then logint i else pix i) - (if length f > 0 then 0 else 1)

-- If the list of factors is of length 1, the factored number is prime and can
-- be excluded from the list of prime factors
exceptPrimes :: [a] -> [a]
exceptPrimes [prime] = []
exceptPrimes xs = xs

-- Collect factors, returned as a flat list, into tuples containing the factor
-- and the number of times it occurred
collectPowers :: [Integer] -> [(Integer, Integer)]
collectPowers = map compress . group
    where
    compress :: [Integer] -> (Integer, Integer)
    compress g@(x:_) = (x, toInteger $ length g)

-- Determining if the number should be calculated using Logarithmic Integral
-- instead of Pi(x)

nthPrimeIsAccurate :: RFactor -> Bool
nthPrimeIsAccurate (RFactor v _ _) = abovePixThreshold v

abovePixThreshold :: Integer -> Bool
abovePixThreshold i = i > pixThreshold

pixThreshold :: Integer
pixThreshold = 10 ^ 13
