module Main where

import Primes.Factor
import Primes.Primecount
import Primes.Logint

import System.Environment (getArgs)
import Data.Char          (isDigit)

isInteger :: String -> Bool
--isInteger = not . null . (reads :: ReadS Integer)
isInteger = all isDigit

main :: IO ()
main = do
    args <- getArgs
    if and [ not $ null args
           , isInteger (args !! 0)
           ]
    then do
        let n = read (args !! 0)
        putStrLn $ "Number to process: " ++ show n
        let f = factor n
        putStrLn $ "Factors: " ++ show f
        let p = pix    n
        putStrLn $ "Pi(x): " ++ show p
        let l = logint n
        putStrLn $ "Li(x): " ++ show l

        return ()
    else do
        putStrLn "No number supplied."
        return ()
