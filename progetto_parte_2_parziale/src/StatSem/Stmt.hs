-- Implementation of the annotation rules for STATEMENTS.
module StatSem.Stmt where

import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified StatSem.Exp    as SSE
import qualified Errors         as E
import qualified Types          as T
import qualified PrettyPrinter  as PP
import qualified LexGrammar     as LG
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
-- - the environment after the annotation of all stmts;
-- - whether it can exit without returning from a loop (i.e. there's a break);
-- - when it throws.
annStmts :: Env -> [Stmt] -> ([AnnStmt], Env, Bool, When)
annStmts env (x:xs) = (xAnn : xsAnn, env'', canExit, whenThrows)
    where (xAnn, env', xCanExit) = annStmt env x
          (xsAnn, env'', xsCanExit, xsThrows) = annStmts env' xs
          canExit = (xCanExit || xsCanExit) && not (isUnreachable env)
          xThrows = throws . AS.annot $ xAnn
          -- Throws:
          -- - Never if the body's reachable statements' throws field is always `Never`
          --   (or if there are no reachable statement);
          -- - Always if it exists a body's always reached statement whose throws field is
          --   `Always`;
          -- - Sometimes otherwise.
          whenThrows = case (isUnreachable env, xThrows, xsThrows) of
              (True, _, _) -> Never
              (_, Always, _) -> Always
              (_, _, Always) | xCanExit -> Sometimes
              (_, _, Always) -> Always
              (_, Sometimes, _) -> Sometimes
              (_, _, Sometimes) -> Sometimes
              _ -> Never
          --env' = if alwaysReturn (AS.annot xAnn) then setUnreachable env else env -- works because in a normal list of statements a stmt cannot always exit without returning
annStmts env [] = ([], env, False, Never)

-- Utility to unpack a nested tuple.
threeTuple :: ((a, b), c) -> (a, b, c)
threeTuple ((x, y), z) = (x, y, z)

-- Annotate a statement node.
--
-- Returns:
-- - an annotated statement;
-- - the environment after the annotation: the environment is propagated because
--   statements can make the next ones unreachable;
-- - whether it can exit without returning from a loop (i.e. there's a break).
annStmt :: Env -> Stmt -> (AnnStmt, Env, Bool)
annStmt env stmt = (stmtAnn, env'', canExit) where
    (stmtAnn, env', canExit) = case stmt of
        AS.StmtAss{} -> threeTuple (annStmtAss env stmt, False)
        AS.StmtRoutCall{} -> threeTuple (annStmtRoutCall env stmt, False)
        AS.StmtReturn{} -> (annStmtReturn env stmt, setUnreachable env, False)
        AS.StmtReturnEmpty{} -> (annStmtReturnEmpty env stmt, setUnreachable env, False)
        AS.StmtContinue{} -> (annStmtContinue env stmt, setUnreachable env, False)
        AS.StmtBreak{} -> (annStmtBreak env stmt, setUnreachable env, True)
        AS.StmtIf{} -> annStmtIf env stmt
        AS.StmtIfElse{} -> annStmtIfElse env stmt
        AS.StmtLoop{} -> threeTuple (annStmtLoop env stmt, False)
        AS.StmtForEach{} -> annStmtForEach env stmt
        AS.StmtTryCatch{} -> annStmtTryCatch env stmt
        AS.StmtThrow{} -> (annStmtThrow env stmt, setUnreachable env, False)
        --_ -> error "Unreachable: `annStmt` called with unknown statement."
    ann = AS.annot stmtAnn
    env'' = if throws ann == Always then setUnreachable env' else env'

-- Annotate an assignment statement.
annStmtAss :: Env -> Stmt -> (AnnStmt, Env)
annStmtAss env AS.StmtAss {
    AS.left = left,
    AS.right = right,
    AS.pos = pos
} = (AS.StmtAss {
    AS.left = annLeft,
    AS.right = castedRight,
    AS.annot = ann,
    AS.pos = pos
}, env') where
    -- Annotate the left and right expressions.
    annLeft = map (SSE.annExp Nothing (requireLExp env)) left
    annRight = zipWith (\r ty -> SSE.annExp (Just ty) (doNotRequireLExp env) r) right leftTypes
    -- Check if the left expression is read-only.
    readOnlyErrs = [E.err pos "Expression `" ++ PP.expPrinter 0 i ++ "` is read-only. Suggestion: change the parameter passing modality." | i <- annLeft, readOnly $ AS.expAnnot i]
    -- Right throws:
    -- - Never if the right expressions always `Never` throw (or if there are no right
    --   expressions);
    -- - Always if it exists a right expression whose throws field is `Always`;
    -- - Sometimes otherwise.
    rightThrows = if all (== Never) (map (annThrows . AS.expAnnot) annRight)
        then Never
        else if any (== Always) (map (annThrows . AS.expAnnot) annRight)
        then Always
        else Sometimes
    -- Check that the left and right expressions have the same number of elements.
    numberErr = [E.err pos "The number of identifiers does not match number of values." | length left /= length right]
    -- Get expected types from the left expressions.
    leftTypes = map (expType . AS.expAnnot) annLeft
    -- Cast the right expressions to the expected types (when the expected type are non-errors).
    castedRight = zipWith (\ a t -> case t of
        T.TError _ -> a -- If the expected type is an error, keep the original expression.
        _ -> SSE.cast (doNotRequireLExp env) a t
        ) annRight leftTypes
    -- Get errors.
    rightErrs = T.getErrs $ map (expType . AS.expAnnot) castedRight
    leftErrs = T.getErrs $ map (expType . AS.expAnnot) annLeft
    errs = leftErrs ++ rightErrs ++ numberErr -- -- Errors are printed in the order of evaluation (l-value is evaluated before r-value).
    -- Create the annotation.
    ann = StmtAnn {
        errs = errs ++ readOnlyErrs,
        unreachable = isUnreachable env,
        throws = rightThrows
    }
    -- If the right exp always throws the environment becomes unreachable.
    env' = if rightThrows == Always then setUnreachable env else env
annStmtAss _ _ = error "Unreachable: `annStmtAss` called with non-`StmtAss` expression."

-- Annotate a routine call statement.
annStmtRoutCall :: Env -> Stmt -> (AnnStmt, Env)
annStmtRoutCall env AS.StmtRoutCall {
    AS.call = rc@AS.RoutineCall {
        AS.routCallName = ident,
        AS.args = args,
        AS.routCallPos = pos
    }
} = (AS.StmtRoutCall {
    AS.call = AS.RoutineCall {
        AS.routCallName = ident,
        AS.args = castedArgs,
        AS.routCallPos = pos
    },
    AS.annot = ann
}, env') where
    -- Params.
    (params, lookupErrs, declPos, returnType, thisThrows) = case Map.lookup ident (routines env) of
        Just (Fun declPos params ty thisThrows) -> (Just params, [], Just declPos, Just $ T.type_ ty, thisThrows) -- Future improvement: warn "Unused return of the function call " ++ PP.routineCallPrinter rc ++ "."
        Just (Proc declPos params thisThrows) -> (Just params, [], Just declPos, Nothing, thisThrows)
        Nothing -> (Nothing, [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."], Nothing, Nothing, Never)
    -- Cast the arguments to the expected types.
    (castedArgs, numberErr) = case params of
        Nothing -> (map (SSE.annExp Nothing (doNotRequireLExp env)) args, []) -- No numberErr because the identifier was not found.
        Just innerParams | length innerParams /= length args -> (map (SSE.annExp Nothing (doNotRequireLExp env)) args, [E.err pos $ "Wrong number of arguments in routine call: " ++ PP.routineCallPrinter 0 rc ++ ". Expected " ++ show (length params) ++ " but got " ++ show (length args) ++ "."])
        Just innerParams -> (SSE.annAndCastRoutArgs env args innerParams, [])
    -- Argument errors.
    argErrs = T.getErrs $ map (expType . AS.expAnnot) castedArgs
    -- Create the annotation.
    ann = StmtRoutCallAnn {
        errs = lookupErrs ++ argErrs ++ numberErr,
        unreachable = isUnreachable env,
        params,
        declPos,
        returnType,
        throws = thisThrows
    }
    -- If the routine always throws the environment becomes unreachable.
    env' = if thisThrows == Always then setUnreachable env else env
annStmtRoutCall _ _ = error "Unreachable: `annStmtRoutCall` called with non-`StmtRoutCall` expression."

-- Annotate a return statement.
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
    annRet = SSE.annExp returnType (doNotRequireLExp env) ret
    -- Search for the return keyword in the environment.
    returnEntry = Map.lookup "return" (keywords env)
    -- Check if the return keyword is present.
    (returnType, returnErr) = case returnEntry of
        Just (Return (Just t)) -> (Just t, [])
        Just (Return Nothing) -> (Nothing, [E.err pos "Procedures (i.e. routines without return value) cannot return a value."])
        _ -> (Nothing, [E.err pos "Return keyword is not permitted outside routines."])
    -- Cast the return value to the expected type.
    castedRet = case returnType of
        Just t -> SSE.cast (doNotRequireLExp env) annRet t
        Nothing -> annRet   -- If there was an error in the retrival of the return type,
                            -- keep the original expression.
    expErrs = T.getErrs [expType (AS.expAnnot castedRet)]
    -- Create the annotation.
    ann = StmtAnn {
        errs = returnErr ++ expErrs,
        unreachable = isUnreachable env,
        throws = Never
    }
annStmtReturn _ _ = error "Unreachable: `annStmtReturn` called with non-`StmtReturn` expression."

-- Annotate a return statement without a value.
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
        unreachable = isUnreachable env,
        throws = Never
    }
annStmtReturnEmpty _ _ = error "Unreachable: `annStmtReturnEmpty` called with non-`StmtReturnEmpty` expression."

-- Annotate a continue statement.
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
        unreachable = isUnreachable env,
        throws = Never
    }
annStmtContinue _ _ = error "Unreachable: `annStmtContinue` called with non-`StmtContinue` expression."

-- Annotate a break statement.
annStmtBreak :: Env -> Stmt -> AnnStmt
annStmtBreak env AS.StmtBreak {
    AS.pos = pos
} = AS.StmtBreak {
    AS.annot = ann,
    AS.pos = pos
} where
    -- Check if the break keyword is present.
    breakErr = case Map.lookup "break" (keywords env) of
        Just Break -> []
        _ -> [E.err pos "Break keyword is not permitted outside loops."]
    -- Create the annotation.
    ann = StmtAnn {
        errs = breakErr,
        unreachable = isUnreachable env,
        throws = Never
    }
annStmtBreak _ _ = error "Unreachable: `annStmtBreak` called with non-`StmtBreak` expression."

-- Annotate an if statement.
annStmtIf :: Env -> Stmt -> (AnnStmt, Env, Bool)
annStmtIf env AS.StmtIf {
    AS.cond = cond,
    AS.body = body,
    AS.pos = pos
} = (AS.StmtIf {
    AS.cond = castedCond,
    AS.body = annBody,
    AS.annot = ann,
    AS.pos = pos
}, env''', thisCanExit) where
    -- Annotate the condition.
    annCond = SSE.annExp (Just T.TBoolean) (doNotRequireLExp env) cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast (doNotRequireLExp env) annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- Set the environment for the body.
    env' = case condCTVal of
        Just (CTVBoolean False) -> setUnreachable env
        _ -> env
    -- Annotate the body.
    (annBody, env'', bodyCanExit, bodyThrows) = annStmts env' body
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Body throws:
    -- - Never if the body's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a body's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableBody = filter (not . unreachable . AS.annot) annBody
    -- bodyThrows = if all (== Never) (map (throws . AS.annot) reachableBody)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBody)
    --     then Always
    --     else Sometimes
    -- If the body is always executed the propagate the body environment.
    env''' = case condCTVal of
        Just (CTVBoolean True) -> env''
        _ -> env'
    -- When the if statement throws.
    thisThrows = case condCTVal of
        Just (CTVBoolean True) -> bodyThrows
        Just (CTVBoolean False) -> Never
        Nothing -> if bodyThrows /= Never then Sometimes else Never
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- When the if statement can exit.
    thisCanExit = case condCTVal of
        Just (CTVBoolean False) -> False
        _ -> bodyCanExit
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        unreachable = isUnreachable env,
        throws = thisThrows
    }
annStmtIf _ _ = error "Unreachable: `annStmtIf` called with non-`StmtIf` expression."

-- Annotate an if-then-else statement.
annStmtIfElse :: Env -> Stmt -> (AnnStmt, Env, Bool)
annStmtIfElse env AS.StmtIfElse {
    AS.cond = cond,
    AS.bodyT = bodyT,
    AS.bodyF = bodyF,
    AS.pos = pos
} = (AS.StmtIfElse {
    AS.cond = castedCond,
    AS.bodyT = annBodyT,
    AS.bodyF = annBodyF,
    AS.annot = ann,
    AS.pos = pos
}, env'', thisCanExit) where
    -- Annotate the condition.
    annCond = SSE.annExp (Just T.TBoolean) (doNotRequireLExp env) cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast (doNotRequireLExp env) annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- Set the environment for the body.
    envT = case condCTVal of
        Just (CTVBoolean False) -> setUnreachable env
        _ -> env
    envF = case condCTVal of
        Just (CTVBoolean True) -> setUnreachable env
        _ -> env
    -- Annotate the true branch.
    (annBodyT, envT', bodyTCanExit, bodyTThrows) = annStmts envT bodyT
    -- Annotate the false branch.
    (annBodyF, envF', bodyFCanExit, bodyFThrows) = annStmts envF bodyF
    -- Body errors.
    bodyTErrs = concatMap (errs . AS.annot) annBodyT
    bodyFErrs = concatMap (errs . AS.annot) annBodyF
    -- Body throws:
    -- - Never if the body's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a body's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableBodyT = filter (not . unreachable . AS.annot) annBodyT
    -- bodyTThrows = if all (== Never) (map (throws . AS.annot) reachableBodyT)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBodyT)
    --     then Always
    --     else Sometimes
    -- reachableBodyF = filter (not . unreachable . AS.annot) annBodyF
    -- bodyFThrows = if all (== Never) (map (throws . AS.annot) reachableBodyF)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBodyF)
    --     then Always
    --     else Sometimes
    -- If the condition has a compile-time value, we can propagate the respective
    -- environments.
    env'' = case condCTVal of
        Just (CTVBoolean True) -> envT'
        Just (CTVBoolean False) -> envF'
        Nothing | isUnreachable envF' && isUnreachable envT' -> setUnreachable env
        Nothing -> env
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- When the if-then-else statement throws.
    thisThrows = case condCTVal of
        Just (CTVBoolean True) -> bodyTThrows
        Just (CTVBoolean False) -> bodyFThrows
        Nothing -> if (bodyTThrows == Always) && (bodyFThrows == Always) then Always
            else if (bodyTThrows == Never) && (bodyFThrows == Never) then Never
            else Sometimes
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- When the if-then-else statement can exit.
    thisCanExit = case condCTVal of
        Just (CTVBoolean True) -> bodyTCanExit
        Just (CTVBoolean False) -> bodyFCanExit
        _ -> bodyTCanExit || bodyFCanExit
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyTErrs ++ bodyFErrs,
        unreachable = isUnreachable env,
        throws = thisThrows
    }
annStmtIfElse _ _ = error "Unreachable: `annStmtIfElse` called with non-`StmtIfElse` expression."

-- Annotate a loop statement.
annStmtLoop :: Env -> Stmt -> (AnnStmt, Env)
annStmtLoop env AS.StmtLoop {
    AS.body = body,
    AS.exit = exit,
    AS.pos = pos
} = (AS.StmtLoop {
    AS.body = annBody,
    AS.exit = annExit,
    AS.annot = ann,
    AS.pos = pos
}, env'''') where
    -- Add the `continue` and `break` keywords to the environment.
    env' = addBreak $ addContinue env
    -- Annotate the body.
    (annBody, env'', bodyCanExit, bodyThrows) = annStmts env' body
    -- Get the errors from the body.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Body throws:
    -- - Never if the body's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a body's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableBody = filter (not . unreachable . AS.annot) annBody
    -- alwaysReachedBody = 
    -- bodyThrows = if all (== Never) (map (throws . AS.annot) reachableBody)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBody)
    --     then Always
    --     else Sometimes
    -- Annotate the exit.
    (annExit, _, exitsCanExit, exitThrows) = annLoopExits env'' exit
    -- If the loop statement can exit.
    canExit = bodyCanExit || exitsCanExit
    -- If the loop cannot exit the environment after the loop is unreachable.
    env'''' = if canExit then env else setUnreachable env
    -- Get the errors from the exit.
    exitErrs = concatMap (errs . AS.loopExitAnnot) annExit
    -- Create the annotation.
    ann = StmtAnn {
        errs = bodyErrs ++ exitErrs,
        unreachable = isUnreachable env,
        throws = if bodyThrows == Always || exitThrows == Always then Always
                 else if bodyThrows == Never && exitThrows == Never then Never
                 else Sometimes
    }
annStmtLoop _ _ = error "Unreachable: `annStmtLoop` called with non-`StmtLoop` expression."

-- Annotate an exit statements list.
--
-- Returns:
-- - the annotated exit statements;
-- - the updated environment;
-- - a bool indicating if the list can exit;
-- - when the list of loop exits throws.
annLoopExits :: Env -> [AS.Exit ()] -> ([AS.Exit Ann], Env, Bool, When)
annLoopExits env (x@AS.LoopExit{}:xs) = (xAnn : xsAnn, env'', canExit, thisThrows)
    where
        (xAnn, env', xcanExit) = annLoopExit env x
        xThrows = throws . AS.loopExitAnnot $ xAnn
        (xsAnn, env'', xscanExit, xsThrows) = annLoopExits env' xs
        canExit = (xcanExit || xscanExit) && not (isUnreachable env)
        thisThrows = if xThrows == Never && xsThrows == Never
            then Never
            else if xcanExit
            then Sometimes
            else if xThrows == Always || xsThrows == Always
            then Always
            else Sometimes
annLoopExits env [] = ([], env, False, Never) -- If there are no exits, the list never returns, cannot exit, and never throws.

-- Annotate an exit statement.
--
-- Returns:
-- - the annotated exit statement;
-- - the updated environment;
-- - a bool indicating if it can exit.
annLoopExit :: Env -> AS.Exit () -> (AS.Exit Ann, Env, Bool)
annLoopExit env AS.LoopExit {
    AS.loopCond = cond,
    AS.loopBody = body,
    AS.loopExitPos = pos
} = (AS.LoopExit {
    AS.loopCond = castedCond,
    AS.loopBody = annBody,
    AS.loopExitAnnot = ann,
    AS.loopExitPos = pos
}, env'', canExit) where
    -- Annotate the condition.
    annCond = SSE.annExp (Just T.TBoolean) (doNotRequireLExp env) cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast (doNotRequireLExp env) annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- If the condition is always true the body is unreachable.
    (env', canExit) = case condCTVal of
        Just (CTVBoolean True) -> (setUnreachable env, True)
        Just (CTVBoolean False) -> (env, bodyCanExit)
        Nothing -> (env, True)
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Annotate the body.
    (annBody, env'', bodyCanExit, bodyThrows) = annStmts env' body
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Body throws:
    -- - Never if the body's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a body's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableBody = filter (not . unreachable . AS.annot) annBody
    -- bodyThrows = if all (== Never) (map (throws . AS.annot) reachableBody)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBody)
    --     then Always
    --     else Sometimes
    -- When the exit throws.
    thisThrows = case condCTVal of
        Just (CTVBoolean True) -> Never
        Just (CTVBoolean False) -> bodyThrows
        Nothing | bodyThrows == Never -> Never
        Nothing -> Sometimes
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        unreachable = isUnreachable env,
        throws = thisThrows
    }

-- Annotate a for-each statement.
--
-- Note: `continue` and `break` keyword aren't permitted inside foreach (unless it is
-- nested inside a loop).
annStmtForEach :: Env -> Stmt -> (AnnStmt, Env, Bool)
annStmtForEach env AS.StmtForEach {
    AS.varIterName = iterName,
    AS.arrayExp = arrayExp,
    AS.body = body,
    AS.pos = pos
} = (AS.StmtForEach {
    AS.varIterName = iterName,
    AS.arrayExp = annArrayExp,
    AS.body = annBody,
    AS.annot = ann,
    AS.pos = pos
}, env''', bodyCanExit) where
    -- Annotate the array expression.
    annArrayExp = SSE.annExp Nothing (requireLExp env) arrayExp
    -- Check if the array expression is read only.
    arrayReadOnly = readOnly (AS.expAnnot annArrayExp)
    -- Get the errors from the array expression.
    arrayPropagatedErrs = T.getErrs [expType (AS.expAnnot annArrayExp)]
    -- Check that it is an array; if so check if its elements' type can be casted to the
    -- iterator type; if so update the environment accordingly.
    (env', arrayNewErr) = case expType $ AS.expAnnot annArrayExp of
        T.TArray { T.elementsType = elementsType } -> mergeEnv env (envFromSymbolBindings [(iterName, (Var pos (T.TypePos elementsType LG.defaultPosn) (Just AS.Mref) arrayReadOnly))])
        T.TError {} -> (env, [])
        _ -> (env, [E.err (AS.expPos annArrayExp) "Array expression expected."]) -- No update of the environment.
    -- Annotate the body.
    (annBody, env'', bodyCanExit, bodyThrows) = annStmts env' body
    -- Body throws:
    -- - Never if the body's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a body's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableBody = filter (not . unreachable . AS.annot) annBody
    -- bodyThrows = if all (== Never) (map (throws . AS.annot) reachableBody)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableBody)
    --     then Always
    --     else Sometimes
    -- Array size is always >= 1, so it is always executed at least an iteration of the
    -- body -> so the unreachable keyword in the after-body environment is propagated.
    env''' = if isUnreachable env'' then setUnreachable env else env
    -- Get the errors from the body.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Create the annotation.
    ann = StmtAnn {
        errs = arrayPropagatedErrs ++ arrayNewErr ++ bodyErrs,
        unreachable = isUnreachable env,
        throws = bodyThrows
    }
annStmtForEach _ _ = error "Unreachable: `annStmtForEach` called with non-`StmtForEach` expression."

-- Annotate a try catch statement.
annStmtTryCatch :: Env -> Stmt -> (AnnStmt, Env, Bool)
annStmtTryCatch env AS.StmtTryCatch {
    AS.try = try,
    AS.catch = catch,
    AS.pos = pos
} = (AS.StmtTryCatch {
    AS.try = annTry,
    AS.catch = annCatch,
    AS.annot = ann,
    AS.pos = pos
}, env''', canExit) where
    -- Annotate the try statement.
    (annTry, env', tryCanExit, tryThrows) = annStmts env try
    -- Get the errors from the try statement.
    tryAnnot = map AS.annot annTry
    tryErrs = concatMap errs tryAnnot
    -- Try throws:
    -- - Never if the try's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a try's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableTry = filter (not . unreachable . AS.annot) annTry
    -- tryThrows = if all (== Never) (map (throws . AS.annot) reachableTry)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableTry)
    --     then Always
    --     else Sometimes
    -- Annotate the catch statement.
    -- If try never throws then catch is unreachable.
    (annCatch, env'', catchCanExit, catchThrows) = if tryThrows == Never then
        annStmts (setUnreachable env) catch
    else
        annStmts env catch
    -- Get the errors from the catch statement.
    catchAnnot = map AS.annot annCatch
    catchErrs = concatMap errs catchAnnot
    -- Catch throws:
    -- - Never if the catch's reachable statements' throws field is always `Never` (or if
    --   there are no reachable statement);
    -- - Always if it exists a catch's reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableCatch = filter (not . unreachable . AS.annot) annCatch
    -- catchThrows = if all (== Never) (map (throws . AS.annot) reachableCatch)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableCatch)
    --     then Always
    --     else Sometimes
    -- If the try-catch statement can exit.
    canExit = tryCanExit || catchCanExit
    -- Create the annotation.
    ann = StmtAnn {
        errs = tryErrs ++ catchErrs,
        unreachable = isUnreachable env,
        throws = catchThrows
    }
    -- After the try-catch statement, the environment is unreachable if both catch and try
    -- are unreachable.
    env''' = if isUnreachable env' && isUnreachable env'' then setUnreachable env else env
annStmtTryCatch _ _ = error "Unreachable: `annStmtTryCatch` called with non-`StmtTryCatch` expression."

-- Annotate a throw statement.
annStmtThrow :: Env -> Stmt -> AnnStmt
annStmtThrow env AS.StmtThrow {
    AS.pos = pos
} = AS.StmtThrow {
    AS.annot = ann,
    AS.pos = pos
} where
    ann = StmtAnn {
        errs = [],
        unreachable = isUnreachable env,
        throws = Always
    }
annStmtThrow _ _ = error "Unreachable: `annStmtThrow` called with non-`StmtThrow` expression."
