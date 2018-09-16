{-# LANGUAGE ForeignFunctionInterface #-}

module Factor (factor) where

import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "test.h factor_integer" factor_msieve :: CString -> IO CString

factor :: Integer -> IO [Integer]
factor i = do
    cs <- newCString $ show i
    hs <- (factor_msieve cs >>= peekCString)
    let factors = read hs :: [Integer]
    return factors
