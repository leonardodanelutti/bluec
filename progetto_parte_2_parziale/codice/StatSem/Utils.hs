-- Utilities for static semantics analysis.
module StatSem.Utils where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import qualified Types          as T

-- Environment with different namespaces for variables, functions, and constants.
--
-- Routine calls without parameters and variables can be indistinguishable if have the
-- same name and same type. In those cases it will be necessary to use the parentheses
-- to disambiguate between a variable and a function call.
data Env = Env {
    symbols :: Map.Map AS.Ident SymbolEntry,
    routines :: Map.Map AS.Ident (LG.Posn, [AS.Param], [T.TypePos])
}

data SymbolEntry
    = Var LG.Posn T.TypePos
    | Const AS.Literal

-- Datatype for static semantics analysis module annotations (on the AST).
data Ann
    = ExpAnn {
        expType :: [T.Type],
        expVal :: CTValue
        -- TODO: other fileds
    }
    | IdentAnn {
        identType :: [T.Type],
        identVal :: CTValue,
        identCategory :: IdentCategory
    }

data IdentCategory
    = VarCategory
    | ConstCategory
    | RoutineCategory
    | ErrorCategory

-- data DownwardAnn
--     = ExpAnn
--     | DownwardIdentAnn {
--         expectedType :: AS.Type
--     }

-- Values known at compile time.
data CTValue
    = CTVInteger Int
    | CTVChar Char
    | CTVString String
    | CTVBoolean Bool
    | CTVReal Double
    | CTVUnknown -- Represents a value that is not known at compile time.

-- -- Get the number of types of an annotation.
-- getAnnTypeCount :: Ann -> Int
-- getAnnTypeCount (ExpAnn { expType = types }) = length types
-- getAnnTypeCount (IdentAnn { identType = types }) = length types
