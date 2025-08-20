module Errors where

import LexGrammar   as LG

prettyerr = "\ESC[31mERROR\ESC[0m"
prettywarn = "\ESC[33mWARNING\ESC[0m"

err :: LG.Posn -> String -> String
err pos msg = show pos ++ ": " ++ prettyerr ++ ": " ++ msg

warn :: LG.Posn -> String -> String
warn pos msg = show pos ++ ": " ++ prettywarn ++ ": " ++ msg