module Solsys.Planets (Planet (..), PlanetType (Positive, Negative, Neutral), rootPlanet, toPlanet, planetTypeToInt) where

import Primes.Factorization
import Primes.Pix
import Primes.Logint

import Control.Concurrent.Async
import System.IO.Unsafe
import Data.List

data PlanetType = Positive | Negative | Neutral deriving Show

planetTypeToInt :: PlanetType -> Int
planetTypeToInt Positive = 1
planetTypeToInt Neutral  = 0
planetTypeToInt Negative = -1

data Planet = Planet
    { planetType :: PlanetType
    , children :: [Planet] 
    } deriving Show

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

rootPlanet :: Integer -> Planet
rootPlanet x | x == 0    = Planet Neutral []
             | x < 0     = toPlanet Negative (abs x)
             | otherwise = toPlanet Positive x

toPlanet :: PlanetType -> Integer -> Planet
toPlanet t 1 = Planet t []
toPlanet t power = unsafePerformIO $ do
    -- Get factors and their pi(x) values
    let fs = frequencies . factorize $ power
    pis <- mapConcurrently (return . conditionalPix . fst) fs

    -- Build the positives as triples (value, power, pix)
    let unbuiltPositives = zipWith (\(a,b) c -> (a,b,c)) fs pis
    -- Build the neutrals as triples with value (0, power, 0)
    let unbuiltChildren = insertNeutrals 0 unbuiltPositives

    -- Turn the triples into pairs of (PlanetType, power)
    let builtChildren = map buildPlanet unbuiltChildren
    -- Mapping toPlanet with curried data constructors over the pairs
    children <- mapConcurrently (return . uncurry toPlanet) builtChildren

    -- Return everything correctly
    return $ Planet t children

insertNeutrals :: Integer -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
insertNeutrals prevP [] = []
insertNeutrals prevP (a@(value, power, p):xs) 
    | p - prevP == 1 = a:(insertNeutrals p xs)
    | otherwise = (0, p - prevP - 1, 0):a:(insertNeutrals p xs)

buildPlanet :: (Integer, Integer, Integer) -> (PlanetType, Integer)
buildPlanet (0, p, 0) = (Neutral, p)
buildPlanet (_, p, _) = (Positive, p)

-- Collect factors, returned as a flat list, into tuples containing the factor
-- and the number of times it occurred
frequencies :: Eq a => [a] -> [(a, Integer)]
frequencies = map compress . group
    where
    compress g@(x:_) = (x, toInteger $ length g)

-- If the list of factors is of length 1, the factored number is prime and can
-- be excluded from the list of prime factors
exceptPrimes :: [a] -> [a]
exceptPrimes [prime] = []
exceptPrimes xs = xs

-- Determining if the number should be calculated using Logarithmic Integral
-- instead of Pi(x)
conditionalPix :: Integer -> Integer
conditionalPix ii = if abovePixThreshold ii then logint ii else pix ii

abovePixThreshold :: Integer -> Bool
abovePixThreshold i = i > pixThreshold

pixThreshold :: Integer
pixThreshold = 10 ^ 13
