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
-- - a bool indicating if it can exits;
-- - a bool indicating if it always returns.
annStmts :: Env -> [Stmt] -> ([AnnStmt], Env)
annStmts env (x:xs) = (xAnn : xsAnn, env'')
    where (xAnn, env') = annStmt env x
          (xsAnn, env'') = annStmts env' xs
          --env' = if alwaysReturn (AS.annot xAnn) then setUnreachable env else env -- works because in a normal list of statements a stmt cannot always exit without returning
annStmts env [] = ([], env)

-- Annotate a statement node.
-- The environment is propagated because statements can make the next ones unreachable.
annStmt :: Env -> Stmt -> (AnnStmt, Env)
annStmt env stmt@AS.StmtAss{} = (annStmtAss env stmt, env)
annStmt env stmt@AS.StmtRoutCall{} = (annStmtRoutCall env stmt, env)
annStmt env stmt@AS.StmtReturn{} = (annStmtReturn env stmt, setUnreachable env)
annStmt env stmt@AS.StmtReturnEmpty{} = (annStmtReturnEmpty env stmt, setUnreachable env)
annStmt env stmt@AS.StmtContinue{} = (annStmtContinue env stmt, setUnreachable env)
annStmt env stmt@AS.StmtIf{} = annStmtIf env stmt
annStmt env stmt@AS.StmtIfElse{} = annStmtIfElse env stmt
annStmt env stmt@AS.StmtLoop{} = annStmtLoop env stmt
annStmt env stmt@AS.StmtCase{} = annStmtCase env stmt

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
    annLeft = map (SSE.annExp (requireLExp env)) left
    annRight = map (SSE.annExp (doNotRequireLExp env)) right
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
        errs = errs,
        unreachable = isUnreachable env
    }
annStmtAss _ _ = error "Unreachable: `annStmtAss` called with non-`StmtAss` expression."

-- Annotate a routine call statement.
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
    -- Params.
    (params, lookupErrs, declPos, returnType) = case Map.lookup ident (routines env) of
        Just (Fun declPos params ty) -> (Just params, [], Just declPos, Just $ T.type_ ty ) -- Future improvement: warn "Unused return of the function call " ++ PP.routineCallPrinter rc ++ "."
        Just (Proc declPos params) -> (Just params, [], Just declPos, Nothing)
        Nothing -> (Nothing, [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."], Nothing, Nothing)
    -- Cast the arguments to the expected types.
    (castedArgs, numberErr) = case params of
        Nothing -> (map (SSE.annExp (doNotRequireLExp env)) args, []) -- No numberErr because the identifier was not found.
        Just innerParams | length innerParams /= length args -> (map (SSE.annExp (doNotRequireLExp env)) args, [E.err pos $ "Wrong number of arguments in routine call: " ++ PP.routineCallPrinter rc ++ ". Expected " ++ show (length params) ++ " but got " ++ show (length args) ++ "."])
        Just innerParams -> (SSE.annAndCastRoutArgs env args innerParams, [])
    -- Argument errors.
    argErrs = T.getErrs $ map (expType . AS.expAnnot) castedArgs
    -- Create the annotation.
    ann = StmtRoutCallAnn {
        errs = lookupErrs ++ argErrs ++ numberErr,
        unreachable = isUnreachable env,
        params,
        declPos,
        returnType
    }
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
    annRet = SSE.annExp (doNotRequireLExp env) ret
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
        unreachable = isUnreachable env
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
        unreachable = isUnreachable env
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
        unreachable = isUnreachable env
    }
annStmtContinue _ _ = error "Unreachable: `annStmtContinue` called with non-`StmtContinue` expression."

-- Annotate an if statement.
annStmtIf :: Env -> Stmt -> (AnnStmt, Env)
annStmtIf env AS.StmtIf {
    AS.cond = cond,
    AS.body = body,
    AS.pos = pos
} = (AS.StmtIf {
    AS.cond = castedCond,
    AS.body = annBody,
    AS.annot = ann,
    AS.pos = pos
}, env''') where
    -- Annotate the condition.
    annCond = SSE.annExp (doNotRequireLExp env) cond
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
    (annBody, env'') = annStmts env' body
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- If the body is always executed the propagate the body environment.
    env''' = case condCTVal of
        Just (CTVBoolean True) -> env''
        _ -> env'
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        unreachable = isUnreachable env
    }
annStmtIf _ _ = error "Unreachable: `annStmtIf` called with non-`StmtIf` expression."

-- Annotate an if-else statement.
annStmtIfElse :: Env -> Stmt -> (AnnStmt, Env)
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
}, env'') where
    -- Annotate the condition.
    annCond = SSE.annExp (doNotRequireLExp env) cond
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
    (annBodyT, envT') = annStmts envT bodyT
    -- Annotate the false branch.
    (annBodyF, envF') = annStmts envF bodyF
    -- Body errors.
    bodyTErrs = concatMap (errs . AS.annot) annBodyT
    bodyFErrs = concatMap (errs . AS.annot) annBodyF
    -- If the condition has a compile-time value, we can propagate the respective
    -- environments.
    env'' = case condCTVal of
        Just (CTVBoolean True) -> envT'
        Just (CTVBoolean False) -> envF'
        Nothing | isUnreachable envF' && isUnreachable envT' -> setUnreachable env
        Nothing -> env
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyTErrs ++ bodyFErrs,
        unreachable = isUnreachable env
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
    -- Add the `continue` keyword to the environment.
    env' = addContinue env
    -- Annotate the body.
    (annBody, env'') = annStmts env' body
    -- Get the errors from the body.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Annotate the exit.
    (annExit, _, canExit) = annLoopExits env'' exit
    -- If the loop cannot exit the environment after the loop is unreachable.
    env'''' = if canExit then env else setUnreachable env
    -- Get the errors from the exit.
    exitErrs = concatMap (errs . AS.loopExitAnnot) annExit
    -- Create the annotation.
    ann = StmtAnn {
        errs = bodyErrs ++ exitErrs,
        unreachable = isUnreachable env
    }
annStmtLoop _ _ = error "Unreachable: `annStmtLoop` called with non-`StmtLoop` expression."

-- Annotate an exit statements list.
--
-- Returns:
-- - the annotated exit statements;
-- - the updated environment;
-- - a bool indicating if the list can exit.
annLoopExits :: Env -> [AS.Exit ()] -> ([AS.Exit Ann], Env, Bool)
annLoopExits env (x@AS.LoopExit{}:xs) = (xAnn : xsAnn, env'', canExit)
    where
        (xAnn, env', xcanExit) = annLoopExit env x
        (xsAnn, env'', xscanExit) = annLoopExits env' xs
        canExit = xcanExit || xscanExit
annLoopExits env [] = ([], env, False) -- If there are no exits, the list never returns and cannot exit.

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
    annCond = SSE.annExp (doNotRequireLExp env) cond
    -- Cast the condition to a boolean type.
    castedCond = SSE.cast (doNotRequireLExp env) annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- If the condition is always true the body is unreachable.
    (env', canExit) = case condCTVal of
        Just (CTVBoolean True) -> (setUnreachable env, True)
        Just (CTVBoolean False) -> (env, False)
        Nothing -> (env, True)
        _ -> error "Unreachable: `castedCond` is casted to a boolean type, so it cannot be a constant of another type."
    -- Annotate the body.
    (annBody, env'') = annStmts env' body
    -- Body errors.
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Create the annotation.
    ann = StmtAnn {
        errs = condErrs ++ bodyErrs,
        unreachable = isUnreachable env
    }


-- Annotate a case statement.
annStmtCase :: Env -> Stmt -> AnnStmt
annStmtCase env AS.StmtCase {
    AS.scrutinee = scrutinee,
    AS.cases = branches,
    AS.def = def,
    AS.pos = pos
} = AS.StmtCase {
    AS.scrutinee = annScrutinee,
    AS.cases = annBranches,
    AS.def = annDef,
    AS.annot = ann,
    AS.pos = pos
} where
    -- Annotate the scrutinee.
    annScrutinee = SSE.annExp (doNotRequireLExp env) scrutinee
    -- Scrutinee compile-time value.
    scrutineeCTVal = getCTValue (AS.expAnnot annScrutinee)
    -- Annotate the branches.
    (annBranches, annDef, thisCanExit, thisAlwaysReturn) = annCaseBranches env branches def scrutineeCTVal
    -- Errors.
    branchErrs = concatMap (errs . AS.branchAnnot) annBranches
    defErrs = case annDef of
        Just annDefUnwrap -> concatMap (errs . AS.annot) annDefUnwrap
        Nothing -> []
    -- Annotation.
    ann = StmtAnn {
        errs = branchErrs ++ defErrs,
        alwaysReturn = thisAlwaysReturn,
        canExit = thisCanExit,
        unreachable = isUnreachable env
    }
annStmtCase _ _ = error "Unreachable: `annStmtCase` called with non-`StmtCase` expression."

-- Annotate a list of case branches.
--
-- Returns:
-- - the annotated case branches;
-- - the annotated default branch;
-- - whether it can exit;
-- - whether it always returns.
annCaseBranches :: Env -> [AS.CaseBranch ()] -> Maybe [Stmt] -> Maybe CTValue -> ([AS.CaseBranch Ann], Maybe [AnnStmt], Bool, Bool)
annCaseBranches _ [] Nothing _ = ([], Nothing, False, False)
annCaseBranches env [] (Just defStmts) _ = ([], Just annDef, bodyCanExit, bodyAlwaysReturn) where
    (annDef, bodyCanExit, bodyAlwaysReturn, _) = annStmts env defStmts
annCaseBranches env (x:xs) def scrutinee = (xAnn : xsAnn, defAnn, thisCanExit, thisAlwaysReturn) where
    -- Annotate the first branch.
    (xAnn, xCtExecuted) = annCaseBranch env x scrutinee
    -- Get useful fields.
    xAnnot = AS.branchAnnot xAnn
    xCanExit = canExit xAnnot
    xAlwaysReturn = alwaysReturn xAnnot
    -- Check if its execution is known at compile-time.
    (env', xAlwaysExecuted, xNeverExecuted) = case xCtExecuted of
        Just True -> (setUnreachable env, True, False)
        Just False -> (env, False, True)
        Nothing -> (env, False, False)
    -- Annotate the rest of the branches with the new env.
    (xsAnn, defAnn, xsCanExit, xsAlwaysReturn) = annCaseBranches env' xs def scrutinee
    -- This branch can exit if either:
    -- - it can exit and it can be executed;
    -- - the next branches can exit and they can be executed.
    thisCanExit = (xCanExit && (not xNeverExecuted)) || (xsCanExit && (not xAlwaysExecuted))
    -- This branch always returns if either:
    -- - it always returns and it is always executed;
    -- - the next branches always return and this branch is never executed.
    thisAlwaysReturn = (xAlwaysReturn && xAlwaysExecuted) || (xsAlwaysReturn && xNeverExecuted)

-- Annotate a case branch.
--
-- Returns:
-- - The annotated case branch.
-- - Whether it is know at compile-time if the branch is executed:
--   - Just True -> always executed (so the next branches are never);
--   - Just False -> never executed;
--   - Nothing -> unknown at compile-time.
annCaseBranch :: Env -> AS.CaseBranch () -> Maybe CTValue -> (AS.CaseBranch Ann, Maybe Bool)
annCaseBranch env AS.CaseBranch {
    AS.pattern = pattern,
    AS.branchBody = body,
    AS.branchPos = branchPos
} scrutinee = (AS.CaseBranch {
    AS.pattern = annPattern,
    AS.branchBody = annBody,
    AS.branchAnnot = ann,
    AS.branchPos = branchPos
}, ctExecuted) where
    -- Annotate the pattern.
    annPattern = SSE.annExp (doNotRequireLExp env) pattern
    -- Get the pattern position.
    patternPos = AS.expPos annPattern
    -- The pattern constant value.
    patternConst = getCTValue (AS.expAnnot annPattern)
    -- The pattern must be a constant.
    patternNonConstantErr = case patternConst of
        Just _ -> []
        _ -> [E.err patternPos "The case branch pattern must be a constant."]
    -- Check if the pattern always matches.
    (env', ctExecuted) = case scrutinee of
        Just _ | scrutinee == patternConst -> (env, Just True)
        Just _ -> (setUnreachable env, Just False)
        _ -> (env, Nothing)
    -- Annotate the body.
    (annBody, bodyCanExit, bodyAlwaysReturn, _) = annStmts env' body
    bodyErrs = concatMap (errs . AS.annot) annBody
    -- Annotation.
    -- It is annotated as a stand-alone block of statements: it this branch never matches
    -- it will not affect the fields alwaysReturn and canExit. The responsibility of
    -- checking the control flow of the case statement is of the function
    -- `annCaseBranches`.
    ann = StmtAnn {
        errs = patternNonConstantErr ++ bodyErrs,
        alwaysReturn = bodyAlwaysReturn,
        canExit = bodyCanExit,
        unreachable = isUnreachable env
    }