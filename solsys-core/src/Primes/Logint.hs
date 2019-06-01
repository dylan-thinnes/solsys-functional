{-# LANGUAGE ForeignFunctionInterface #-}

module Primes.Logint (logint) where

import Data.Char (isDigit)
import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "logint" logint_c :: CString -> CString

logint :: Integer -> Integer
logint i = unsafePerformIO $ do
    cs <- newCString $ show i
    prehs <- peekCString $ logint_c cs
    let hs = takeWhile isDigit prehs
    let res = read hs :: Integer
    return res
