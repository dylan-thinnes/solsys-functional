{- Pretty printing of Solsys modules to JSON format
E.g. 15/7 is equal to
positive
    neutral
    positive
    positive
    negative
Which is shortened to
+
 0
 +
 +
 -
-}

module Solsys.Printer.Whitespace (convert) where

import Solsys.Planets

planetTypeToShorthand :: PlanetType -> String
planetTypeToShorthand Positive = "+"
planetTypeToShorthand Negative = "-"
planetTypeToShorthand Neutral  = "0"

toWhitespaceLines :: Planet -> [String]
toWhitespaceLines (Planet t cs) = (planetTypeToShorthand t) : map ((" "++)) (concatMap toWhitespaceLines cs)

toWhitespace :: Planet -> String
toWhitespace = init . unlines . toWhitespaceLines

convert = toWhitespace
