-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import qualified Data.Map.Lazy as Map
import qualified AbsSyn as AS
import qualified LexGrammar as LG
import StatSem.Exp (annExp)

-- TODO: main annotation function