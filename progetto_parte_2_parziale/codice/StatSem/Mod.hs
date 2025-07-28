-- Static semantics analysis module for the compiler.

module StatSem.Mod where

import qualified AbsSyn         as AS
import StatSem.Utils
-- The line below gives a warning because unused for the moment, but it is needed to
-- compile the Exp module.
import StatSem.Exp

-- TYPES
-- Void annotations after parsing.
type Declarations = AS.Declarations ()
-- Annotated expressions after static semantics analysis.
type AnnDeclarations = AS.Declarations Ann

-- TODO: Main annotation function.