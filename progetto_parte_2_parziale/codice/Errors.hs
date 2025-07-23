module Errors where

import LexGrammar (Posn)

prettyerr = "\ESC[31mERROR\ESC[0m"
prettywarn = "\ESC[33mWARNING\ESC[0m"

err :: Posn -> String -> String
err pos msg = show pos ++ " " ++ prettyerr ++ ": " ++ msg

warn :: Posn -> String -> String
warn pos msg = show pos ++ " " ++ prettywarn ++ ": " ++ msg