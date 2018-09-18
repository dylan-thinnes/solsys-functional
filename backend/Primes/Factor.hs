{-# LANGUAGE ForeignFunctionInterface #-}

module Primes.Factor (factor) where

import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "test.h factor_integer" factor_msieve :: CString -> CString

factor :: Integer -> [Integer]
factor i = unsafePerformIO $ do
    cs <- newCString $ show i
    hs <- peekCString $ factor_msieve cs
    let factors = read hs :: [Integer]
    return factors
