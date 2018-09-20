module Main where

import Solsys.Planets
import Solsys.Printer.JSON (convert)
import Primes.Logint

import System.Environment (getArgs)
import Data.Char          (isDigit)
import System.IO          (hPutStrLn, stderr)
import System.Random      (randomRIO)

isInteger :: String -> Bool
isInteger = not . null . (reads :: ReadS Integer)

main :: IO ()
main = do
    args <- getArgs
    if and [ not $ null args
           , isInteger (args !! 0)
           ]
    then do
        let n = read (args !! 0)
        let p = rootPlanet n
        putStrLn $ show $ convert p

        return ()
    else do
        r <- randomRIO (10 ^ 49, 10 ^ 50) :: IO Integer
        sign <- randomRIO (0, 1) >>= return . ([-1,1] !!) :: IO Integer

        let n = r * sign
        hPutStrLn stderr $ "No number supplied. Generating a random number ... " ++ show n
        let p = rootPlanet n
        putStrLn $ show $ convert p

        return ()
