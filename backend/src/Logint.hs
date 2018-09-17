{-# LANGUAGE ForeignFunctionInterface #-}

module Logint (logint) where

import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "logint" logint_c :: CString -> IO CString

logint :: Integer -> IO Integer
logint i = unsafePerformIO $ do
    cs <- newCString $ show i
    hs <- (logint_c cs >>= peekCString)
    let res = read hs :: Integer
    return res
