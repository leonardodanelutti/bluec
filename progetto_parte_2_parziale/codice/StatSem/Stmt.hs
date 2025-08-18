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

-- Annotate list of statements.
--
-- Returns:
-- - a list of annotated statements;
-- - a bool indicating if it always exits;
-- - a bool indicating if it always returns.
annStmts :: Env -> [Stmt] -> ([AnnStmt], Bool, Bool)
-- Note that when we check if xsReturns we do not have to check whether x does not exit
-- because the only way it exits is with a return statement.
annStmts env (x:xs) = (xAnn : xsAnn, xExits || xsExits, xReturns || xsReturns)
    where (xsAnn, xsExits, xsReturns) = annStmts env xs
          xReturns = alwaysReturn (AS.annot xAnn)
          xExits = alwaysExits (AS.annot xAnn)
          xAnn = annStmt env x
annStmts _ [] = ([], False, False)

-- From a list of annotated statements take only the first that are reachable.
takeWhileReachable :: [AnnStmt] -> [AnnStmt]
takeWhileReachable = takeWhile (not . unreachable . AS.annot)

-- Annotate a statement node.
annStmt :: Env -> Stmt -> AnnStmt
annStmt env stmt@AS.StmtAss{} = annStmtAss env stmt
annStmt env stmt@AS.StmtRoutCall{} = annStmtRoutCall env stmt
annStmt env stmt@AS.StmtReturn{} = annStmtReturn env stmt
annStmt env stmt@AS.StmtReturnEmpty{} = annStmtReturnEmpty env stmt
annStmt env stmt@AS.StmtContinue{} = annStmtContinue env stmt
annStmt env stmt@AS.StmtIf{} = annStmtIf env stmt
annStmt env stmt@AS.StmtIfElse{} = annStmtIfElse env stmt
annStmt env stmt@AS.StmtLoop{} = annStmtLoop env stmt

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
    -- Annotate the left and right expressions.
    annLeft = map (SSE.annExp env) left
    annRight = map (SSE.annExp env) right
    -- Check that the left and right expressions have the same number of elements.
    numberErr = if length left /= length right
                then [E.err pos "The number of identifiers does not match number of values."]
                else []
    -- Check whether the left expressions aren't constants.
    leftConstErr = concat $ map (\a -> case getCTValue (AS.expAnnot a) of -- TODO: is this enough to be sure that the left is not a constant?
        Just _ -> [E.err (AS.expPos a) "Cannot assign a value to a constant. Suggested fix: make " ++ PP.expPrinter a ++ " a variable."]
        _ -> []) annLeft
    -- TODO: check that the left is a LExp? Forse è sufficiente checkare che non abbia valore costante perchè tanto si preoccupa il parser di controllare che sia una LExp?
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
    errs = leftErrs ++ rightErrs ++ numberErr ++ leftConstErr -- Errors are printed in the order of evaluation (l-value is evaluated before r-value).
    -- Create the annotation.
    ann = StmtAnn {
        errs = errs,
        alwaysReturn = False,
        alwaysExits = False,
        unreachable = isUnreachable env
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
    (params, lookupErrs, declPos, isFunction) = case Map.lookup ident (routines env) of
        Just (Fun declPos params _) -> (Just params, [E.err pos "Unused return of the function call " ++ PP.routineCallPrinter rc ++ "."], Just declPos, Just True) -- TODO: maybe make it a warning?
        Just (Proc declPos params) -> (Just params, [], Just declPos, Just False)
        Nothing -> (Nothing, [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."], Nothing, Nothing)
    -- Cast the arguments to the expected types.
    (castedArgs, numberErr) = case params of
        Nothing -> (annArgs, []) -- No numberErr because the identifier was not found.
        Just innerParams | length innerParams /= length annArgs -> (annArgs, [E.err pos $ "Wrong number of arguments in routine call: " ++ PP.routineCallPrinter rc ++ ". Expected " ++ show (length params) ++ " but got " ++ show (length annArgs) ++ "."])
        Just innerParams -> (SSE.castParamTypes innerParams annArgs, [])
    -- Argument errors.
    argErrs = T.getErrs $ map (expType . AS.expAnnot) castedArgs
    -- Create the annotation.
    ann = StmtRoutCallAnn {
        errs = lookupErrs ++ argErrs ++ numberErr,
        alwaysReturn = False,
        alwaysExits = False,
        unreachable = isUnreachable env,
        params = params,
        declPos = declPos,
        isFunction
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
        alwaysReturn = True,
        alwaysExits = True,
        unreachable = isUnreachable env
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
        alwaysReturn = True,
        alwaysExits = True,
        unreachable = isUnreachable env
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
        alwaysReturn = False,
        alwaysExits = True,
        unreachable = isUnreachable env
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
    (annBody, bodyAlwaysExits, bodyAlwaysReturn) = annStmts env body -- TODO: from the return we can output a warning if there are unreachable statements.
    -- Body always returns?
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- If the condition has a compile-time value, we can determine if the statement always returns.
    (thisAlwaysReturn, thisAlwaysExits) = case condCTVal of
        Just (CTVBoolean v) -> (v && bodyAlwaysReturn, v && bodyAlwaysExits) -- TODO: maybe warn that the `If` statement is useless because the condition is always true or false.
        Nothing -> (False, False)
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        alwaysReturn = thisAlwaysReturn,
        alwaysExits = thisAlwaysExits,
        unreachable = isUnreachable env
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
    (annBodyT, bodyTAlwaysExits, bodyTAlwaysReturn) = annStmts env bodyT
    -- Annotate the false branch.
    (annBodyF, bodyFAlwaysExits, bodyFAlwaysReturn) = annStmts env bodyF
    -- Body errors.
    bodyTErrs = concatMap (errs . AS.annot) annBodyT
    bodyFErrs = concatMap (errs . AS.annot) annBodyF
    -- If the condition has a compile-time value, we can determine if the statement always returns.
    (thisAlwaysReturn, thisAlwaysExits) = case condCTVal of
        Just (CTVBoolean v) -> ((v && bodyTAlwaysReturn) || ((not v) && bodyFAlwaysReturn), (v && bodyTAlwaysExits) || ((not v) && bodyFAlwaysExits)) -- TODO: warn that the `If` or `Else` are unreachable if the condition is constant.
        Nothing -> (bodyTAlwaysReturn && bodyFAlwaysReturn, bodyTAlwaysExits && bodyFAlwaysExits)
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyTErrs ++ bodyFErrs,
        alwaysReturn = thisAlwaysReturn,
        alwaysExits = thisAlwaysExits,
        unreachable = isUnreachable env
    }
annStmtIfElse _ _ = error "Unreachable: `annStmtIfElse` called with non-`StmtIfElse` expression."

-- Annotate a loop statement.
annStmtLoop :: Env -> Stmt -> AnnStmt
annStmtLoop env AS.StmtLoop {
    AS.body = body,
    AS.exit = exit,
    AS.pos = pos
} = AS.StmtLoop {
    AS.body = annBody,
    AS.exit = annExit,
    AS.annot = ann,
    AS.pos = pos
} where
    -- Add the `continue` keyword to the environment.
    env' = addContinue env
    -- Annotate the body.
    (annBody, bodyAlwaysExits, bodyAlwaysReturn) = annStmts env' body
    -- Get the errors from the body.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Annotate the exit.
    (annExit, exitAlwaysReturn) = annLoopExits env' exit
    -- Get the errors from the exit.
    exitErrs = concatMap (errs . AS.loopExitAnnot) annExit
    -- Get only the reachable statements.
    -- reachableStmts = takeWhileReachable exitResults
    -- -- Get only the statements before the first exit whose condition is always false (i.e. take while conditions are true).
    -- neverExits = map fst $ takeWhile (\(_, condCTValBool) -> case condCTValBool of
    --     Just False -> True
    --     _ -> False) reachableStmts
    -- Check if the exit always returns.
    -- exitAlwaysReturn = any (alwaysReturn . AS.loopExitAnnot) neverExits
    -- Create the annotation.
    ann = StmtAnn {
        errs = bodyErrs ++ exitErrs,
        alwaysReturn = bodyAlwaysReturn || (exitAlwaysReturn && not bodyAlwaysExits),
        alwaysExits = False,
        unreachable = isUnreachable env
    }
annStmtLoop _ _ = error "Unreachable: `annStmtLoop` called with non-`StmtLoop` expression."

-- Annotate an exit statements list.
--
-- Returns:
-- - the annotated exit statements;
-- - a bool indicating if the list always returns.
annLoopExits :: Env -> [AS.Exit ()] -> ([AS.Exit Ann], Bool)
annLoopExits env (x@AS.LoopExit{}:xs) = (xAnn : xsAnn, thisAlwaysReturn)
    where (env', thisAlwaysReturn) = case xExits of
            -- If x always exits, the next elements are unreachable. So the list in input
            -- never returns.
            Just True -> (setUnreachable env, False)
            -- If the condition is always false, the body is always reached. So the list
            -- in input always returns if the current exit does, or if the rest of the
            -- list does.
            Just False -> (env, alwaysReturn (AS.loopExitAnnot xAnn) || xsReturns)
            -- If the condition is variable, the body is not guaranteed to be executed. So
            -- it is not unreachable but we cannot determine if alwaysReturns.
            Nothing -> (env, False)
          (xAnn, xExits) = annLoopExit env x
          (xsAnn, xsReturns) = annLoopExits env' xs
annLoopExits _ [] = ([], False) -- If there are no exits, the list never returns.

-- Annotate an exit statement.
--
-- Returns:
-- - the annotated exit statement;
-- - a bool indicating if it always exits (even with a continue or a return)
annLoopExit :: Env -> AS.Exit () -> (AS.Exit Ann, Maybe Bool)
annLoopExit env AS.LoopExit {
    AS.loopCond = cond,
    AS.loopBody = body,
    AS.loopExitPos = pos
} = (AS.LoopExit {
    AS.loopCond = castedCond,
    AS.loopBody = annBody,
    AS.loopExitAnnot = ann,
    AS.loopExitPos = pos
}, alwaysExits) where
    -- Annotate the condition.
    annCond = SSE.annExp env cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- If the condition is always true the body is unreachable.
    env' = case condCTVal of
        Just (CTVBoolean True) -> setUnreachable env
        _ -> env
    -- Annotate the body.
    (annBody, bodyExits, bodyAlwaysReturn) = annStmts env' body
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Always returns?
    (condCTValBool, thisAlwaysReturn) = case condCTVal of
            Just (CTVBoolean v) -> (Just v, (not v) && bodyAlwaysReturn)
            Nothing -> (Nothing, False) -- We cannot determine if the exit is false at least once in order to exit the loop.
            _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        alwaysReturn = thisAlwaysReturn,
        alwaysExits = case alwaysExits of
            Just True -> True
            _ -> False,
        unreachable = isUnreachable env
    }
    -- Determine if this statement always exits.
    alwaysExits = case (bodyExits, condCTValBool) of
        (True, _) -> Just True
        (_, x) -> x