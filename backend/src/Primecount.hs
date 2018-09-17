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

main :: IO ()
main = do
    args <- getArgs
    if length args > 0 then do
        let i = read $ args !! 0 :: Integer
        res <- pix i 
        print res
    else return ()
