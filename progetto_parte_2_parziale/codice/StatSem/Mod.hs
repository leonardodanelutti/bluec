-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import StatSem.Exp (annExp)
import StatSem.Utils

-- TYPES
-- Void annotations after parsing.
type Declarations = AS.Declarations ()
-- Annotated expressions after static semantics analysis.
type AnnDeclarations = AS.Declarations Ann

-- TODO: Main annotation function.