-- Utilities for static semantics analysis.
module StatSem.Utils where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import qualified Types          as T
import qualified Errors         as E
import qualified PrettyPrinter  as PP

-- Environment with different namespaces for variables, functions, and constants.
--
-- Routine calls without parameters and variables can be indistinguishable if have the
-- same name and same type. In those cases it will be necessary to use the parentheses
-- to disambiguate between a variable and a function call.
data Env = Env {
        symbols :: Map.Map AS.Ident SymbolEntry,
        routines :: Map.Map AS.Ident RoutEntry,
        keywords :: Map.Map String KeywordEntry
    }
    deriving (Show)

data SymbolEntry
    -- Posn refers of the whole declaration (meaning that different symbols can have the
    -- same Posn if they are declared together).
    = Var LG.Posn T.TypePos (Maybe AS.Mode) -- TODO: a cosa serve Posn per var? Perchè TypePos e non Type?
    | Const LG.Posn AS.Literal
    deriving (Show)

data RoutEntry -- TODO: serve la posizione?
    = Fun LG.Posn [AS.Param] T.TypePos
    | Proc LG.Posn [AS.Param]
    deriving (Show)

data KeywordEntry
    -- If present specifies the return type. Cases:
    -- - It is not present in the environment: return keyword is not permitted.
    -- - It is present and bool is True: at least one return keyword is necessary.
    -- - It is present and bool is False: return keyword is not necessary, but permitted.
    = Return (Maybe T.Type)
    | Continue
    deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty

-- Merge two environments. In case of clashes, the second environment overrides the first.
mergeEnv :: Env -> Env -> Env
mergeEnv (Env s1 r1 k1) (Env s2 r2 k2) = Env (Map.union s2 s1) (Map.union r2 r1) (Map.union k2 k1)

envFromSymbolBindings :: [(AS.Ident, SymbolEntry)] -> Env
envFromSymbolBindings bindings = Env (Map.fromList bindings) Map.empty Map.empty

envFromRoutBindings :: [(AS.Ident, RoutEntry)] -> Env
envFromRoutBindings bindings = Env Map.empty (Map.fromList bindings) Map.empty

addContinue :: Env -> Env
addContinue env = env { keywords = Map.insert "continue" Continue (keywords env) }

addReturn :: Env -> Maybe T.Type -> Env
addReturn env mType = env { keywords = Map.insert "return" (Return mType) (keywords env) }

-- Datatype for static semantics analysis module annotations (on the AST).
data Ann
    = ExpAnn {
        expType :: T.Type,
        ctVal :: Maybe CTValue
        -- TODO: other fileds
    }
    | FunCallAnn {
        expType :: T.Type,
        params :: [AS.Param] -- Parameters passed to the function call
    }
    | IdentAnn {
        expType :: T.Type,
        ctVal :: Maybe CTValue,
        identCategory :: IdentCategory,
        mode :: Maybe AS.Mode,
        -- The position of the whole declaration (different symbols can have the same
        -- position if they are declared together).
        -- Nothing if the identifier is not found in the environment.
        declPos :: Maybe LG.Posn
    }
    | DeclAnn {
        bindings :: Env, -- Local environment
        errs :: [String]
    }
    | StmtAnn {
        -- Errors of the tree rooted by the node with this annotation.
        errs :: [String],
        -- True if for each flow of control inside the node there is a return statement.
        alwaysReturn :: Bool
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
    | CTVRef CTValue
    -- | CTVArray [CTValue]
    deriving (Show)

ctvToLiteral :: AS.Exp Ann -> Either AS.Literal [String]
ctvToLiteral exp = case getCTValue (AS.expAnnot exp) of
    Just (CTVInteger v) -> Left $ AS.LitInt v pos
    Just (CTVChar v)    -> Left $ AS.LitChar v pos
    Just (CTVString v)  -> Left $ AS.LitString v pos
    Just (CTVBoolean v) -> Left $ AS.LitBool v pos
    Just (CTVReal v)    -> Left $ AS.LitDouble v pos
    Just (CTVRef v)     -> Right [E.err pos $ "constants cannot be references: " ++ show v ++ "."]
    Nothing             -> case AS.expAnnot exp of
        ExpAnn { expType = T.TError { T.msgs = msgs } } -> Right msgs
        IdentAnn { expType = T.TError { T.msgs = msgs } } -> Right msgs
        _ -> Right [E.err pos $ PP.expPrinter exp ++ " is not a constant expression."]
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


-- Apply a binary operation to two compile-time values.
-- It assumes that values are already casted to the correct types.
applyBinOp :: AS.BinOp -> CTValue -> CTValue -> CTValue
applyBinOp AS.And (CTVBoolean v1) (CTVBoolean v2) = CTVBoolean (v1 && v2)
applyBinOp AS.Or (CTVBoolean v1) (CTVBoolean v2) = CTVBoolean (v1 || v2)
applyBinOp AS.Add (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 + v2)
applyBinOp AS.Add (CTVReal v1) (CTVReal v2) = CTVReal (v1 + v2)
applyBinOp AS.Sub (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 - v2)
applyBinOp AS.Sub (CTVReal v1) (CTVReal v2) = CTVReal (v1 - v2)
applyBinOp AS.Mul (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 * v2)
applyBinOp AS.Mul (CTVReal v1) (CTVReal v2) = CTVReal (v1 * v2)
applyBinOp AS.Div (CTVReal v1) (CTVReal v2) = CTVReal (v1 / v2)
applyBinOp AS.DivInt (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 `div` v2)
applyBinOp AS.Mod (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 `mod` v2)
applyBinOp AS.Pow (CTVInteger v1) (CTVInteger v2) = CTVInteger (v1 ^ v2)
applyBinOp AS.Pow (CTVReal v1) (CTVReal v2) = CTVReal (v1 ** v2)
applyBinOp _ _ _ = error "Invalid arguments for compile-time binary operation"

-- Apply a relational operation to two compile-time values.
-- It assumes that values are already casted to the correct types.
applyRelOp :: AS.RelOp -> CTValue -> CTValue -> Bool
applyRelOp f (CTVInteger v1) (CTVInteger v2) = funRelOp f v1 v2
applyRelOp f (CTVReal v1) (CTVReal v2) = funRelOp f v1 v2
applyRelOp f (CTVChar v1) (CTVChar v2) = funRelOp f v1 v2
applyRelOp f (CTVString v1) (CTVString v2) = funRelOp f v1 v2
applyRelOp f (CTVBoolean v1) (CTVBoolean v2) = funRelOp f v1 v2
applyRelOp f (CTVRef v1) (CTVRef v2) = applyRelOp f v1 v2
applyRelOp _ _ _ = error "Invalid arguments for compile-time relational operation"

funRelOp AS.Eq = (==)
funRelOp AS.Neq = (/=)
funRelOp AS.Lt = (<)
funRelOp AS.Le = (<=)
funRelOp AS.Gt = (>)
funRelOp AS.Ge = (>=)