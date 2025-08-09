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
annStmt env stmt@AS.StmtReturn{} = annStmtReturn env stmt
annStmt env stmt@AS.StmtReturnEmpty{} = annStmtReturnEmpty env stmt
annStmt env stmt@AS.StmtContinue{} = annStmtContinue env stmt
annStmt env stmt@AS.StmtIf{} = annStmtIf env stmt
annStmt env stmt@AS.StmtIfElse{} = annStmtIfElse env stmt
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
        errs = errs,
        alwaysReturn = False
    }
annStmtAss _ _ = error "Unreachable: `annStmtAss` called with non-`StmtAss` expression."

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
        errs = lookupErrs ++ argErrs ++ numberErr,
        alwaysReturn = False
    }
annStmtRoutCall _ _ = error "Unreachable: `annStmtRoutCall` called with non-`StmtRoutCall` expression."

annStmtReturn :: Env -> Stmt -> AnnStmt
annStmtReturn env AS.StmtReturn {
    AS.valReturn = ret,
    AS.pos = pos
} = AS.StmtReturn {
    AS.valReturn = annRet,
    AS.annot = ann,
    AS.pos = pos
} where
    -- Annotate the return value.
    annRet = SSE.annExp env ret
    -- Search for the return keyword in the environment.
    returnEntry = Map.lookup "return" (keywords env)
    -- Check if the return keyword is present.
    (returnType, returnErr) = case returnEntry of
        Just (Return (Just t)) -> (Just t, [])
        Just (Return Nothing) -> (Nothing, [E.err pos "Procedures cannot return a value."]) -- TODO: forse non si capisce bene l'errore
        _ -> (Nothing, [E.err pos "Return keyword is not permitted outside routines."])
    -- Cast the return value to the expected type.
    castedRet = case returnType of
        Just t -> SSE.cast annRet t
        Nothing -> annRet   -- If there was an error in the retrival of the return type,
                            -- keep the original expression.
    expErrs = T.getErrs $ [expType (AS.expAnnot castedRet)]
    -- Create the annotation.
    ann = StmtAnn {
        errs = returnErr ++ expErrs,
        alwaysReturn = True
    }
annStmtReturn _ _ = error "Unreachable: `annStmtReturn` called with non-`StmtReturn` expression."

annStmtReturnEmpty :: Env -> Stmt -> AnnStmt
annStmtReturnEmpty env AS.StmtReturnEmpty {
    AS.pos = pos
} = AS.StmtReturnEmpty {
    AS.annot = ann,
    AS.pos = pos
} where
    -- Search for the return keyword in the environment.
    returnEntry = Map.lookup "return" (keywords env)
    -- Check if the return keyword is present.
    returnErr = case returnEntry of
        Just (Return (Just t)) -> [E.err pos "Returning an empty value, but " ++ show t ++ " expected."]
        Just (Return Nothing) -> []
        _ -> [E.err pos "Return keyword is not permitted outside routines."]
    -- Create the annotation.
    ann = StmtAnn {
        errs = returnErr,
        alwaysReturn = True
    }
annStmtReturnEmpty _ _ = error "Unreachable: `annStmtReturnEmpty` called with non-`StmtReturnEmpty` expression."

annStmtContinue :: Env -> Stmt -> AnnStmt
annStmtContinue env AS.StmtContinue {
    AS.pos = pos
} = AS.StmtContinue {
    AS.annot = ann,
    AS.pos = pos
} where
    -- Check if the continue keyword is present.
    continueErr = case Map.lookup "continue" (keywords env) of
        Just Continue -> []
        _ -> [E.err pos "Continue keyword is not permitted outside loops."]
    -- Create the annotation.
    ann = StmtAnn {
        errs = continueErr,
        alwaysReturn = False
    }
annStmtContinue _ _ = error "Unreachable: `annStmtContinue` called with non-`StmtContinue` expression."

annStmtIf :: Env -> Stmt -> AnnStmt
annStmtIf env AS.StmtIf {
    AS.cond = cond,
    AS.body = body,
    AS.pos = pos
} = AS.StmtIf {
    AS.cond = castedCond,
    AS.body = annBody,
    AS.annot = ann,
    AS.pos = pos
} where
    -- Annotate the condition.
    annCond = SSE.annExp env cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs $ [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- Annotate the body.
    annBody = map (annStmt env) body
    -- Body always returns?
    bodyAlwaysReturn = any (alwaysReturn . AS.annot) annBody -- TODO: from the return we can output a warning if there are unreachable statements.
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- If the condition has a compile-time value, we can determine if the statement always returns.
    thisAlwaysReturn = case condCTVal of
        Just (CTVBoolean v) -> v && bodyAlwaysReturn -- TODO: maybe warn that the `If` statement is useless because the condition is always true or false.
        Nothing -> False
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        alwaysReturn = thisAlwaysReturn
    }
annStmtIf _ _ = error "Unreachable: `annStmtIf` called with non-`StmtIf` expression."

annStmtIfElse :: Env -> Stmt -> AnnStmt
annStmtIfElse env AS.StmtIfElse {
    AS.cond = cond,
    AS.bodyT = bodyT,
    AS.bodyF = bodyF,
    AS.pos = pos
} = AS.StmtIfElse {
    AS.cond = castedCond,
    AS.bodyT = annBodyT,
    AS.bodyF = annBodyF,
    AS.annot = ann,
    AS.pos = pos
} where
    -- Annotate the condition.
    annCond = SSE.annExp env cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs $ [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- Annotate the true branch.
    annBodyT = map (annStmt env) bodyT
    -- Annotate the false branch.
    annBodyF = map (annStmt env) bodyF
    -- True branch always returns?
    bodyTAlwaysReturn = any (alwaysReturn . AS.annot) annBodyT
    -- False branch always returns?
    bodyFAlwaysReturn = any (alwaysReturn . AS.annot) annBodyF
    -- Body errors.
    bodyTErrs = concatMap (errs . AS.annot) annBodyT
    bodyFErrs = concatMap (errs . AS.annot) annBodyF
    -- If the condition has a compile-time value, we can determine if the statement always returns.
    thisAlwaysReturn = case condCTVal of
        Just (CTVBoolean v) -> (v && bodyTAlwaysReturn) || ((not v) && bodyFAlwaysReturn) -- TODO: warn that the `If` or `Else` are unreachable if the condition is constant.
        Nothing -> bodyTAlwaysReturn && bodyFAlwaysReturn
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyTErrs ++ bodyFErrs,
        alwaysReturn = thisAlwaysReturn
    }
annStmtIfElse _ _ = error "Unreachable: `annStmtIfElse` called with non-`StmtIfElse` expression."

annStmtLoop :: Env -> Stmt -> AnnStmt
-- TODO:
annStmtLoop _ _ = error "Unreachable: `annStmtLoop` called with non-`StmtLoop` expression."