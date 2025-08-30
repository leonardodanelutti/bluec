-- Utilities for static semantics analysis.
module StatSem.Utils where

import qualified Data.Map.Lazy  as Map
import qualified Data.Maybe     as Maybe
import qualified AbsSyn         as AS
import qualified LexGrammar     as LG
import qualified Types          as T
import qualified Errors         as E
import qualified PrettyPrinter  as PP

-----------------
-- ENVIRONMENT --
-----------------

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

-- Variable or constant bindings.
data SymbolEntry
    -- Posn refers of the whole declaration (meaning that different symbols can have the
    -- same Posn if they are declared together).
    = Var LG.Posn T.TypePos (Maybe AS.Mode)
    | Const LG.Posn AS.Literal
    deriving (Show)

-- Function or procedure bindings.
data RoutEntry
    -- - Position of definition;
    -- - Parameters;
    -- - Return type;
    -- - When it throws.
    = Fun LG.Posn [AS.Param] T.TypePos When
    | Proc LG.Posn [AS.Param] When
    deriving (Show)

-- Position of a `RoutEntry`.
pos :: RoutEntry -> LG.Posn
pos (Fun p _ _ _) = p
pos (Proc p _ _)  = p

-- Keyword bindings.
data KeywordEntry
    -- Allowed inside routines to allow returning:
    -- - `Return (Just ty)` is present inside a function with returning type `ty`;
    -- - `Return Nothing` is present inside procedures.
    = Return (Maybe T.Type)
    -- Allowed inside loops to allow skipping to the next iteration.
    | Continue
    -- Allowed inside loops to allow breaking out of the loop.
    | Break
    -- Inserted in the environment of unreachable statements.
    | Unreachable
    -- Insert inside expression environment to require it to be a left-hand side
    -- expression.
    | LExp
    deriving (Show)

-- Construct an empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty

-- Pre-defined routines.
preludeRoutines :: [(AS.Ident, RoutEntry)]
preludeRoutines = [
        ((AS.Ident "writeInt" LG.defaultPosn), Proc LG.defaultPosn [AS.Param AS.Mval (AS.Ident "value" LG.defaultPosn) (T.TypePos T.TInteger LG.defaultPosn) LG.defaultPosn] Never),
        ((AS.Ident "writeReal" LG.defaultPosn), Proc LG.defaultPosn [AS.Param AS.Mval (AS.Ident "value" LG.defaultPosn) (T.TypePos T.TReal LG.defaultPosn) LG.defaultPosn] Never),
        ((AS.Ident "writeChar" LG.defaultPosn), Proc LG.defaultPosn [AS.Param AS.Mval (AS.Ident "value" LG.defaultPosn) (T.TypePos T.TChar LG.defaultPosn) LG.defaultPosn] Never),
        ((AS.Ident "writeString" LG.defaultPosn), Proc LG.defaultPosn [AS.Param AS.Mval (AS.Ident "value" LG.defaultPosn) (T.TypePos T.TString LG.defaultPosn) LG.defaultPosn] Never),
        ((AS.Ident "readInt" LG.defaultPosn), Fun LG.defaultPosn [] (T.TypePos T.TInteger LG.defaultPosn) Never),
        ((AS.Ident "readReal" LG.defaultPosn), Fun LG.defaultPosn [] (T.TypePos T.TReal LG.defaultPosn) Never),
        ((AS.Ident "readChar" LG.defaultPosn), Fun LG.defaultPosn [] (T.TypePos T.TChar LG.defaultPosn) Never),
        ((AS.Ident "readString" LG.defaultPosn), Fun LG.defaultPosn [] (T.TypePos T.TString LG.defaultPosn) Never)
    ]

-- Construct an environment with the pre-defined routines.
preludeEnv :: Env
preludeEnv = Env Map.empty (Map.fromList preludeRoutines) Map.empty

-- Merge two environments.
--
-- In case of symbol clashes, the second environment overrides the first.
-- In case of routine clashes, an error is added to the list.
-- In case of keyword clashes, the second environment overrides the first.
mergeEnv :: Env -> Env -> (Env, [String])
mergeEnv (Env s1 r1 k1) (Env s2 r2 k2) = (Env s r k, errs)
    where
        s = Map.union s2 s1
        r = Map.union r2 r1
        k = Map.union k2 k1
        errs = Map.foldrWithKey f [] r2
        f key value accErr = case Map.lookup key r1 of
            Just _  -> accErr ++ [E.err (pos value) "A routine with the same name `" ++ PP.identPrinter key ++ "` is already defined at " ++ show (pos $ Maybe.fromJust $ Map.lookup key r1) ++ "."]
            _       -> accErr

-- Creates an environment from symbol bindings.
envFromSymbolBindings :: [(AS.Ident, SymbolEntry)] -> Env
envFromSymbolBindings bindings = Env (Map.fromList bindings) Map.empty Map.empty

-- Creates an environment from routine bindings.
envFromRoutBindings :: [(AS.Ident, RoutEntry)] -> Env
envFromRoutBindings bindings = Env Map.empty (Map.fromList bindings) Map.empty

-- Adds a `continue` keyword to the environment, used inside loops.
addContinue :: Env -> Env
addContinue env = env { keywords = Map.insert "continue" Continue (keywords env) }

-- Adds a `break` keyword to the environment, used inside loops.
addBreak :: Env -> Env
addBreak env = env { keywords = Map.insert "break" Break (keywords env) }

-- Adds a `return` keyword to the environment, used inside routines.
addReturn :: Env -> Maybe T.Type -> Env
addReturn env mType = env { keywords = Map.insert "return" (Return mType) (keywords env) }

-- Sets the `unreachable` keyword in the environment, used inside unreachable statements.
setUnreachable :: Env -> Env
setUnreachable env = env { keywords = Map.insert "unreachable" Unreachable (keywords env) }

-- Query if the statement is unreachable.
isUnreachable :: Env -> Bool
isUnreachable env = Map.member "unreachable" (keywords env)

-- Require the expression to be of left-hand side type.
requireLExp :: Env -> Env
requireLExp env = env { keywords = Map.insert "LExp" LExp (keywords env) }

-- Query if the expression must be of left-hand side type.
mustBeLExp :: Env -> Bool
mustBeLExp env = Map.member "LExp" (keywords env)

-- Remove the left-hand side requirement.
doNotRequireLExp :: Env -> Env
doNotRequireLExp env = env { keywords = Map.delete "LExp" (keywords env) }

-----------------
-- ANNOTATIONS --
-----------------

-- Datatype for static semantics analysis module annotations (on the AST).
data Ann
    ----------------------------
    -- EXPRESSION ANNOTATIONS --
    ----------------------------
    = ExpAnn {
        expType :: T.Type,
        ctVal :: Maybe CTValue
    }
    | ExpFunCallAnn {
        expType :: T.Type,
        -- Parameters passed to the function call.
        params :: Maybe [AS.Param],
        -- The position of the whole declaration (different symbols can have the same
        -- position if they are declared together).
        -- Nothing if the identifier is not found in the environment.
        declPos :: Maybe LG.Posn,
        throws :: When
    }
    | ExpIdentAnn {
        expType :: T.Type,
        ctVal :: Maybe CTValue,
        identCategory :: IdentCategory,
        mode :: Maybe AS.Mode,
        -- The position of the whole declaration (different symbols can have the same
        -- position if they are declared together).
        -- Nothing if the identifier is not found in the environment.
        declPos :: Maybe LG.Posn
    }
    -----------------------------
    -- DECLARATION ANNOTATIONS --
    -----------------------------
    | DeclAnn {
        bindings :: Env, -- Local environment
        errs :: [String]
    }
    ---------------------------
    -- STATEMENT ANNOTATIONS --
    ---------------------------
    | StmtAnn {
        -- Errors of the tree rooted by the node with this annotation.
        errs :: [String],
        -- True if the node is unreachable (e.g. after a return statement).
        unreachable :: Bool,
        throws :: When
    }
    | StmtRoutCallAnn {
         -- Errors of the tree rooted by the node with this annotation.
        errs :: [String],
        -- True if the node is unreachable (e.g. after a return statement).
        unreachable :: Bool,
        -- Parameters passed to the function call. Nothing if the identifier is not found
        -- in the environment.
        params :: Maybe [AS.Param],
        -- The position of the whole declaration (different symbols can have the same
        -- position if they are declared together).
        -- Nothing if the identifier is not found in the environment.
        declPos :: Maybe LG.Posn,
        -- The return type of the routine. 
        -- Just ty if it is a function returning type `ty`, 
        -- Nothing if it is a procedure or the identifier is not found in the environment.
        returnType :: Maybe T.Type,
        throws :: When
    }
    deriving (Show)

data When
    = Never
    | Sometimes
    | Always
    deriving (Show, Eq)

annThrows :: Ann -> When
annThrows (ExpFunCallAnn { throws = when }) = when
annThrows (StmtAnn { throws = when }) = when
annThrows (StmtRoutCallAnn { throws = when }) = when
annThrows _ = Never

-- Category of an identifier.
--
-- An ident that is found to be a routine is annotated with the ExpAnn type instead (so it
-- does not appear in this enum).
data IdentCategory
    = VarCategory
    | ConstCategory
    | ErrorCategory
    deriving (Show)

-- Add an error to the annotation.
addErr :: Ann -> String -> Ann
addErr ann msg = case ann of
    DeclAnn { errs = errs } -> ann { errs = msg : errs }
    StmtAnn { errs = errs } -> ann { errs = msg : errs }
    StmtRoutCallAnn { errs = errs } -> ann { errs = msg : errs }
    exp -> exp { expType = newExpType }
        where
            ty = expType exp
            newExpType = case ty of
                T.TError { T.msgs = msgs } -> T.TError { T.msgs = msg : msgs }
                _ -> T.TError { T.msgs = [msg] }

-------------------------
-- COMPILE TIME VALUES --
-------------------------

-- Get the compile-time value from an annotation.
getCTValue :: Ann -> Maybe CTValue
getCTValue (ExpAnn { ctVal = val }) = val
getCTValue (ExpIdentAnn { ctVal = val }) = val
getCTValue _ = Nothing

-- Values known at compile time.
data CTValue
    = CTVInteger Int
    | CTVChar Char
    | CTVString String
    | CTVBoolean Bool
    | CTVReal Double
    deriving (Show)

-- Convert a compile-time value to a literal.
ctvToLiteral :: AS.Exp Ann -> Either AS.Literal [String]
ctvToLiteral exp = case getCTValue (AS.expAnnot exp) of
    Just (CTVInteger v) -> Left $ AS.LitInt v pos
    Just (CTVChar v)    -> Left $ AS.LitChar v pos
    Just (CTVString v)  -> Left $ AS.LitString v pos
    Just (CTVBoolean v) -> Left $ AS.LitBool v pos
    Just (CTVReal v)    -> Left $ AS.LitDouble v pos
    Nothing             -> case AS.expAnnot exp of
        ExpAnn { expType = T.TError { T.msgs = msgs } } -> Right msgs
        ExpIdentAnn { expType = T.TError { T.msgs = msgs } } -> Right msgs
        _ -> Right [E.err pos "\"" ++ (PP.expPrinter 0 exp) ++ "\" is not a constant expression."]
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

-- Apply a binary operation to two compile-time values with short-cut
applyBinOpSC :: AS.BinOp -> Maybe CTValue -> Maybe CTValue -> Maybe CTValue
applyBinOpSC AS.And (Just (CTVBoolean False)) _ = Just (CTVBoolean False)
applyBinOpSC AS.And _ (Just (CTVBoolean False)) = Just (CTVBoolean False)
applyBinOpSC AS.Or (Just (CTVBoolean True)) _ = Just (CTVBoolean True)
applyBinOpSC AS.Or _ (Just (CTVBoolean True)) = Just (CTVBoolean True)
applyBinOpSC op (Just v1) (Just v2) = Just (applyBinOp op v1 v2)
applyBinOpSC _ _ _ = Nothing

-- Apply a relational operation to two compile-time values.
-- It assumes that values are already casted to the correct types.
applyRelOp :: AS.RelOp -> CTValue -> CTValue -> Bool
applyRelOp f (CTVInteger v1) (CTVInteger v2) = funRelOp f v1 v2
applyRelOp f (CTVReal v1) (CTVReal v2) = funRelOp f v1 v2
applyRelOp f (CTVChar v1) (CTVChar v2) = funRelOp f v1 v2
applyRelOp f (CTVString v1) (CTVString v2) = funRelOp f v1 v2
applyRelOp f (CTVBoolean v1) (CTVBoolean v2) = funRelOp f v1 v2
applyRelOp _ _ _ = error "Invalid arguments for compile-time relational operation"

-- Function to apply a relational operation.
funRelOp AS.Eq = (==)
funRelOp AS.Neq = (/=)
funRelOp AS.Lt = (<)
funRelOp AS.Le = (<=)
funRelOp AS.Gt = (>)
funRelOp AS.Ge = (>=)
