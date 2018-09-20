module Main where

import Solsys.Planets
import Solsys.Printer.JSON (convert)
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
        let p = rootPlanet n
        putStrLn $ show $ convert p

        return ()
    else do
        putStrLn "No number supplied."
        return ()
