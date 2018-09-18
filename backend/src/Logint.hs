{-# LANGUAGE ForeignFunctionInterface #-}

module Logint (logint) where

import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "logint" logint_c :: CString -> CString

logint :: Integer -> Integer
logint i = unsafePerformIO $ do
    cs <- newCString $ show i
    hs <- peekCString $ logint_c cs
    let res = read hs :: Integer
    return res
