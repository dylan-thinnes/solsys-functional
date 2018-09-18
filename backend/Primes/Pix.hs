{-# LANGUAGE ForeignFunctionInterface #-}

module Primes.Pix (pix) where

import Data.Char (isDigit)
import Foreign.C
import System.IO.Unsafe

foreign import ccall "pi_extern" pi_primecount :: CString -> CString

pix :: Integer -> Integer
pix i = unsafePerformIO $ do
    cs <- newCString $ show i
    prehs <- peekCString $ pi_primecount cs
    let hs = takeWhile isDigit prehs
    let res = read hs :: Integer
    return res
