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
        expType :: T.Type,
        ctVal :: Maybe CTValue
        -- TODO: other fileds
    }
    | IdentAnn {
        expType :: T.Type,
        ctVal :: Maybe CTValue,
        identCategory :: IdentCategory
    }
    | RoutAnn {
        expTypes :: [T.Type]
    }

data IdentCategory
    = VarCategory
    | ConstCategory
    | ErrorCategory

data CountedType
    = SingleType
    | MultipleTypes

-- Type getter.
getType :: Ann -> CountedType
getType (ExpAnn { expType = t }) = SingleType t
getType (IdentAnn { expType = t }) = SingleType t
getType (RoutAnn { expTypes = ts }) | length ts == 1 = SingleType (head ts)
                                    | otherwise = MultipleTypes ts

-- Type setter.
setType :: Ann -> CountedType -> Ann
setType e@(ExpAnn {}) (SingleType t) = e { expType = t }
-- Multiple types are not supported for ExpAnn.
setType e@(IdentAnn {}) (SingleType t) = e { expType = t }
-- Multiple types are not supported for IdentAnn.
setType e@(RoutAnn {}) (MultipleTypes ts) = e { expTypes = ts }
-- Single type is not supported for RoutAnn.

-- Get the compile-time value from an annotation.
getCTValue :: Ann -> Maybe CTValue
getCTValue (ExpAnn { ctVal = val }) = val
getCTValue (IdentAnn { ctVal = val }) = val
getCTValue _ = Nothing

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

-- Compile time casting.
ctCast :: Maybe CTValue -> T.Type -> Maybe CTValue
ctCast (Just (CTVInteger v)) toType = case toType of
    T.TInteger -> Just (CTVInteger v)
    T.TReal    -> Just (CTVReal (fromIntegral v))
    T.TBoolean -> Just (CTVBoolean (v /= 0))
    _          -> Nothing
ctCast (Just (CTVChar v)) toType = case toType of
    T.TChar    -> Just (CTVChar v)
    T.TString  -> Just (CTVString [v]) -- FIXME: vogliamo mettere la compatibilità tra Char e String?
    T.TBoolean -> Just (CTVBoolean (v /= '\0'))
    T.TInteger -> Just (CTVInteger (fromEnum v))
    T.TReal    -> Just (CTVReal (fromIntegral (fromEnum v)))
    _          -> Nothing
ctCast (Just (CTVString v)) T.TString = Just (CTVString v)
ctCast (Just (CTVBoolean v)) T.TBoolean = Just (CTVBoolean v)
ctCast (Just (CTVReal v)) T.TReal = Just (CTVReal v)
ctCast _ _ = Nothing
