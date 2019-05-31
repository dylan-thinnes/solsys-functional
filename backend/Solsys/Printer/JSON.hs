{-# LANGUAGE RecordWildCards #-}
-- Pretty printing of Solsys modules to JSON format
module Solsys.Printer.JSON (convert) where

import Solsys.Planets

import Data.Maybe (fromMaybe)
import Data.List (intersperse, nubBy)

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

mergeObjects :: JSON -> JSON -> JSON
mergeObjects (JO o1) (JO o2) = JO $ nubBy (\x y -> fst x == fst y) $ o1 ++ o2
mergeObjects (JO o1) _       = JO o1
mergeObjects _       (JO o2) = JO o2
mergeObjects _       _       = Null

planetTypeToJSON :: PlanetType -> JSON
planetTypeToJSON t = JS $ show t

planetToJSON :: Planet -> JSON
planetToJSON (Planet t cs) = 
    JO [("type", planetTypeToJSON t)
       ,("children", JA $ map planetToJSON cs)
       ]

convert = planetToJSON

instance Show JSON where
    show (JO kvs) = "{" ++ (concat . intersperse "," . map showKv $ kvs) ++ "}"
                    where
                    showKv (k,v) = "\"" ++ k ++ "\":" ++ show v
    show (JA vs)  = "[" ++ (concat . intersperse "," . map show $ vs) ++ "]"
    show (JS s) = show s
    show (JI i) = show i

data ScaleWidth = ScaleWidth
    { scale :: Double
    , width :: Double
    , planet :: Planet
    , subannotations :: [OrbitRadius]
    } deriving Show

data OrbitRadius = OrbitRadius
    { orbitRadius :: Double
    , scaleWidth :: ScaleWidth
    } deriving Show

type Annotation = OrbitRadius

scaling, spacing :: Double
scaling = 0.9
spacing = 0.1

annotate :: Planet -> Annotation
annotate = OrbitRadius 0 . f 1 
    where
    f :: Double -> Planet -> ScaleWidth
    f scale planet@(Planet{..})
        = ScaleWidth scale finalWidth planet $ reverse subannotations
        where
        scaleWidthChildren = map (f $ scale * scaling) children
        initialWidth = 1 + 2 * fromIntegral (length children) * spacing
        (subannotations, _, finalWidth) 
            = foldl assignOrbitRadii ([], 0.5, initialWidth) scaleWidthChildren
        assignOrbitRadii (siblings, radius, total) planet
            = (OrbitRadius (scale * (radius + spacing + scaling * width planet / 2)) planet : siblings
              ,radius + spacing + scaling * width planet
              ,2 * scaling * width planet + total
              )
