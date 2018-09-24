module Main where

import Solsys.Planets
import qualified Solsys.Printer.JSON as J
import qualified Solsys.Printer.Whitespace as W
import Primes.Logint

import System.Environment (getArgs)
import System.Exit        (exitSuccess, exitFailure)
import Data.Char          (isDigit)
import System.IO          (hPutStrLn, stderr)
import System.Random      (randomRIO)

isInteger :: String -> Bool
isInteger = not . null . (reads :: ReadS Integer)

main :: IO ()
main = do
    args <- getArgs
    n <- if and [ not $ null args
               , isInteger (args !! 0)
               ]
        then do
            let n = read (args !! 0)
            return n
        else do
            r <- randomRIO (10 ^ 49, 10 ^ 50) :: IO Integer
            sign <- randomRIO (0, 1) >>= return . ([-1,1] !!) :: IO Integer

            let n = r * sign
            hPutStrLn stderr $ "No number supplied. Generating a random number ... " ++ show n
            return n

    let toString = if and [ length args > 1
                   , args !! 1 == "W"
                   ]
                   then W.convert
                   else show . J.convert

    let p = rootPlanet n

    putStrLn $ toString p
    return ()
