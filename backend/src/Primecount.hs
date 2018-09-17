{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import System.Environment (getArgs)

foreign import ccall "pi_extern" pi_primecount :: CString -> IO CString

pix :: Integer -> IO Integer
pix i = do
    cs <- newCString $ show i
    hs <- (pi_primecount cs >>= peekCString)
    let res = read hs :: Integer
    return res
