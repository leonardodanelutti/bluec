-- Implementation of the annotation rules for STATEMENTS.
module StatSem.Stmt where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified StatSem.Exp    as SSE
import qualified Errors         as E
import qualified Types          as T
import qualified PrettyPrinter  as PP
import StatSem.Utils

-- TYPES
-- Void annotations after parsing.
type Stmt = AS.Statement ()
-- Annotated statements after static semantics analysis.
type AnnStmt = AS.Statement Ann

-- Annotate a statement node.
annStmt :: Env -> Stmt -> AnnStmt
annStmt env stmt@AS.StmtAss{} = annStmtAss env stmt
annStmt env stmt@AS.StmtRoutCall{} = annStmtRoutCall env stmt
annStmt _ _ = error "TODO: Implement other statement types"
-- ...

-- Annotate an assignment statement.
annStmtAss :: Env -> Stmt -> AnnStmt
annStmtAss env AS.StmtAss {
    AS.left = left,
    AS.right = right,
    AS.pos = pos
} = AS.StmtAss {
    AS.left = annLeft,
    AS.right = castedRight,
    AS.annot = ann,
    AS.pos = pos
} where
    annLeft = map (SSE.annExp env) left
    annRight = map (SSE.annExp env) right
    numberErr = if length left /= length right
                then [E.err pos "The number of identifiers does not match number of values."]
                else []
    -- TODO: check that the left is a LExp?
    -- Get expected types from the left expressions.
    leftTypes = map (expType . AS.expAnnot) annLeft
    -- Cast the right expressions to the expected types (when the expected type are non-errors).
    castedRight = zipWith (\a -> \t -> case t of
        T.TError _ -> a -- If the expected type is an error, keep the original expression.
        _ -> SSE.cast a t
        ) annRight leftTypes
    -- Get errors.
    rightErrs = T.getErrs $ map (expType . AS.expAnnot) castedRight
    leftErrs = T.getErrs $ map (expType . AS.expAnnot) annLeft
    errs = leftErrs ++ rightErrs ++ numberErr -- Errors are printed in the order of evaluation (l-value is evaluated before r-value).
    -- Create the annotation.
    ann = StmtAnn {
        errs = errs
    }

-- Annotate a routine call statement. SSE.annRoutCall
annStmtRoutCall :: Env -> Stmt -> AnnStmt
annStmtRoutCall env AS.StmtRoutCall {
    AS.call = rc@AS.RoutineCall {
        AS.routCallName = ident,
        AS.args = args,
        AS.routCallPos = pos
    }
} = AS.StmtRoutCall {
    AS.call = AS.RoutineCall {
        AS.routCallName = ident,
        AS.args = castedArgs,
        AS.routCallPos = pos
    },
    AS.annot = ann
} where
    -- Annotate the arguments.
    annArgs = map (SSE.annExp env) args
    -- Params.
    (params, lookupErrs) = case Map.lookup ident (routines env) of
        Just (Fun _ params _) -> (params, [E.err pos "Unused return of the function call " ++ PP.routineCallPrinter rc ++ "."]) -- TODO: maybe make it a warning?
        Just (Proc _ params) -> (params, [])
        Nothing -> ([], [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."])
    -- Cast the arguments to the expected types.
    (castedArgs, numberErr) = if length params /= length annArgs
                then (annArgs, [E.err pos $ "Wrong number of arguments in routine call: " ++ PP.routineCallPrinter rc ++ ". Expected " ++ show (length params) ++ " but got " ++ show (length annArgs) ++ "."])
                else (SSE.castParamTypes params annArgs, [])
    -- Argument errors.
    argErrs = T.getErrs $ map (expType . AS.expAnnot) castedArgs
    -- Create the annotation.
    ann = StmtAnn {
        errs = lookupErrs ++ argErrs ++ numberErr
    }