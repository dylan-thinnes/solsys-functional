{-# LANGUAGE ForeignFunctionInterface #-}

module Primes.Primecount (pix) where

import Foreign.C
import System.IO.Unsafe

foreign import ccall "pi_extern" pi_primecount :: CString -> CString

pix :: Integer -> Integer
pix i = unsafePerformIO $ do
    cs <- newCString $ show i
    hs <- peekCString $ pi_primecount cs
    let res = read hs :: Integer
    return res
