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
        ctVal :: Maybe CTValue,
        identCategoryWT :: IdentCategory
    }
    | RoutAnn {
        expTypes :: [T.Type]
    }

data IdentCategory
    = VarCategory T.Type
    | ConstCategory T.Type
    | RoutineCategory [T.Type]
    | ErrorCategory T.Type

data CountedType
    = SingleType T.Type
    | MultipleTypes [T.Type]

getType :: Ann -> CountedType
getType (ExpAnn { expType = t }) = SingleType t
getType (IdentAnn { identCategoryWT = VarCategory t }) = SingleType t
getType (IdentAnn { identCategoryWT = ConstCategory t }) = SingleType t
getType (IdentAnn { identCategoryWT = RoutineCategory ts }) | length ts == 1 = SingleType (head ts)
                                                            | otherwise = MultipleTypes ts
getType (IdentAnn { identCategoryWT = ErrorCategory t }) = SingleType t
getType (RoutAnn { expTypes = ts }) | length ts == 1 = SingleType (head ts)
                                    | otherwise = MultipleTypes ts

setType :: Ann -> CountedType -> Ann
setType e@(ExpAnn { expType = _ }) (SingleType t) = e { expType = t }
-- Multiple types are not supported for ExpAnn.
setType e@(IdentAnn { identCategoryWT = VarCategory _ }) (SingleType t) =
    e { identCategoryWT = VarCategory t }
setType e@(IdentAnn { identCategoryWT = ConstCategory _ }) (SingleType t) =
    e { identCategoryWT = ConstCategory t }
setType e@(IdentAnn { identCategoryWT = RoutineCategory _ }) (MultipleTypes ts) =
    e { identCategoryWT = RoutineCategory ts }
setType e@(IdentAnn { identCategoryWT = ErrorCategory _ }) (SingleType t) =
    e { identCategoryWT = ErrorCategory t }
setType e@(RoutAnn { expTypes = _ }) (MultipleTypes ts) =
    e { expTypes = ts }

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

-- -- Get the number of types of an annotation.
-- getAnnTypeCount :: Ann -> Int
-- getAnnTypeCount (ExpAnn { expType = types }) = length types
-- getAnnTypeCount (IdentAnn { identType = types }) = length types
