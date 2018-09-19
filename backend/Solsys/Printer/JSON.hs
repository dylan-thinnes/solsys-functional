-- Pretty printing of Solsys modules to different formats
module Solsys.Printer.JSON where

import Solsys.Planets

import Data.Maybe (fromMaybe)
import Data.List (intersperse)

data JSON = JO [(String, JSON)]
          | JA [JSON]
          | JS String
          | JI Integer
          | Null

getAttr :: JSON -> String -> JSON
getAttr (JO o) s = fromMaybe Null $ lookup s o
getAttr _ _ = Null

getIndex :: JSON -> Int -> JSON
getIndex (JA xs) ii = xs !! ii
getIndex _ _ = Null

planetTypeToJSON :: PlanetType -> JSON
planetTypeToJSON t = JS $ show t

planetToJSON :: Planet -> JSON
planetToJSON (Planet t cs) = 
    JO [("type", planetTypeToJSON t)
       ,("children", JA $ map planetToJSON cs)
       ]

instance Show JSON where
    show (JO kvs) = "{" ++ (concat . intersperse "," . map showKv $ kvs) ++ "}"
                    where
                    showKv (k,v) = "\"" ++ k ++ "\":" ++ show v
    show (JA vs)  = "[" ++ (concat . intersperse "," . map show $ vs) ++ "]"
    show (JS s) = show s
    show (JI i) = show i
