{-# LANGUAGE OverloadedStrings #-}
module Server where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp

import Control.Monad (join)
import Text.Read (readMaybe)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS

import Solsys.Planets
import qualified Solsys.Printer.JSON as J

main :: IO ()
main = run 3000 app

app :: Application
app req respond = do
    putStrLn "Request received!"
    let number :: Maybe Integer
        number = do
            result <- join $ lookup "number" $ queryString req
            readMaybe . unpack $ result
    case number of 
        Nothing 
         -> do
             putStrLn "Faulty request received."
             respond $ responseLBS status400 defaultHeaders "{}\n"
        Just n  
         -> do
             putStrLn $ "Profile for: " ++ show n
             respond $ responseLBS status200 defaultHeaders
                (BS.pack . (++"\n") . show . J.convert . rootPlanet $ n)

defaultHeaders = [("Content-Type", "application/json")
                 ,("Access-Control-Allow-Origin", "*")
                 ,("Access-Control-Allow-Headers", "*")
                 ]
