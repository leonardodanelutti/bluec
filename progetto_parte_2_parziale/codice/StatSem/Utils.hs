-- Utilities for static semantics analysis.
module StatSem.Utils where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import qualified Types          as T
import qualified Errors         as E

-- Environment with different namespaces for variables, functions, and constants.
--
-- Routine calls without parameters and variables can be indistinguishable if have the
-- same name and same type. In those cases it will be necessary to use the parentheses
-- to disambiguate between a variable and a function call.
data Env = Env {
        symbols :: Map.Map AS.Ident SymbolEntry,
        routines :: Map.Map AS.Ident RoutEntry
    }
    deriving (Show)

data SymbolEntry
    = Var LG.Posn T.TypePos
    | Const AS.Literal
    deriving (Show)

data RoutEntry
    = Fun LG.Posn [AS.Param] T.TypePos
    | Proc LG.Posn [AS.Param]
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-- Merge two environments. In case of clashes, the second environment overrides the first.
mergeEnv :: Env -> Env -> Env
mergeEnv (Env s1 r1) (Env s2 r2) = Env (Map.union s2 s1) (Map.union r2 r1)

envFromSymbolBindings :: [(AS.Ident, SymbolEntry)] -> Env
envFromSymbolBindings bindings = Env (Map.fromList bindings) Map.empty

envFromRoutBindings :: [(AS.Ident, RoutEntry)] -> Env
envFromRoutBindings bindings = Env Map.empty (Map.fromList bindings)

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
    | DeclAnn {
        bindings :: Env, -- Local environment
        errs :: [String]
    }
    deriving (Show)

-- Category of an identifier.
--
-- An ident that is found to be a routine is annotated with the ExpAnn type instead (so it
-- does not appear in this enum).
data IdentCategory
    = VarCategory
    | ConstCategory
    | ErrorCategory
    deriving (Show)

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
    deriving (Show)

ctvToLiteral :: AS.Exp Ann -> Either AS.Literal String
ctvToLiteral exp = case getCTValue (AS.expAnnot exp) of
    Just (CTVInteger v) -> Left $ AS.LitInt v pos
    Just (CTVChar v)    -> Left $ AS.LitChar v pos
    Just (CTVString v)  -> Left $ AS.LitString v pos
    Just (CTVBoolean v) -> Left $ AS.LitBool v pos
    Just (CTVReal v)    -> Left $ AS.LitDouble v pos
    Nothing             -> Right $ E.err pos $ show exp ++ " is not a constant expression."
    where
        pos = AS.expPos exp

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
