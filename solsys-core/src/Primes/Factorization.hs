{-# LANGUAGE ForeignFunctionInterface #-}

module Primes.Factorization (factorize) where

import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "factor_integer" factor_msieve :: CString -> CString

factorize :: Integer -> [Integer]
factorize i = unsafePerformIO $ do
    cs <- newCString $ show i
    hs <- peekCString $ factor_msieve cs
    let factors = read hs :: [Integer]
    return factors
