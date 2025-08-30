-- Implementation of the annotation rules for DECLARATIONS.
module StatSem.Decl where

import Data.Either (partitionEithers)
import qualified AbsSyn             as AS
import qualified Types              as T
import qualified Errors             as E
import qualified PrettyPrinter      as PP
import StatSem.Utils
import StatSem.Exp
import StatSem.Stmt

-- TYPES
-- Void annotations after parsing.
type Decls = AS.Declarations ()
-- Annotated expressions after static semantics analysis.
type AnnDecls = AS.Declarations Ann

-- Annotate a declarations node.
annDecls :: Env -> Decls -> AnnDecls
annDecls env AS.ProgramDecl {
    AS.consts = consts,
    AS.vars = vars,
    AS.routines = routines
} = AS.ProgramDecl {
    AS.consts = constsAnn,
    AS.vars = varsAnn,
    AS.routines = routinesAnn,
    AS.declsAnnot = ann
} where
    constsAnn = annConstAs inEnvConsts consts
    varsAnn = annVarAs inEnvVars vars
    routinesAnn = annRoutines inEnvRoutines routines
    (inEnvConsts, annidatedRoutOverloadErr) = mergeEnv env outEnvRoutines
    (inEnvVars, _) = mergeEnv env $ fst $ mergeEnv outEnvConsts outEnvRoutines
    (inEnvRoutines, _) = mergeEnv env $ fst $ mergeEnv inEnvVars outEnvVars
    outEnvConsts = constAsEnv constsAnn -- Local environment of constants.
    outEnvVars = varAsEnv varsAnn -- Local environment of variables.
    (outEnvRoutines, sameLevelRoutOverloadErr) = routinesEnv routinesAnn -- Local environment of routines.
    errsConsts = concatMap (errs . AS.constAnnot) constsAnn
    errsVars = concatMap (errs . AS.varAnnot) varsAnn
    errsRoutines = concatMap (errs . AS.routAnnot) routinesAnn
    ann = DeclAnn {
        bindings = fst $ mergeEnv inEnvVars outEnvVars,
        errs = errsConsts ++ errsVars ++ annidatedRoutOverloadErr ++ sameLevelRoutOverloadErr ++ errsRoutines
    }

-- Annotate a list of constant declarations.
annConstAs :: Env -> [AS.ConstA ()] -> [AS.ConstA Ann]
annConstAs _ [] = []
annConstAs env (c : cs) = cAnn : annConstAs newEnv cs
    where
        cAnn = annConstA env c
        cEnv = bindings $ AS.constAnnot cAnn
        newEnv = fst $ mergeEnv env cEnv

-- Get the environment of a list of constant declarations.
constAsEnv :: [AS.ConstA Ann] -> Env
constAsEnv [] = emptyEnv
constAsEnv (c : cs) = fst $ mergeEnv (bindings $ AS.constAnnot c) (constAsEnv cs)

-- Annotate a constant declaration.
annConstA :: Env -> AS.ConstA () -> AS.ConstA Ann
annConstA env AS.ConstAss {
    AS.constIdents = idents,
    AS.constType = ty,
    AS.constValues = values,
    AS.constPos = pos
} = AS.ConstAss {
    AS.constIdents = idents,
    AS.constType = ty,
    AS.constValues = castedValuesAnn,
    AS.constAnnot = ann,
    AS.constPos = pos
} where
    numberErr = if length idents /= length values
                then [E.err pos "The number of identifiers does not match number of values."]
                else []
    valuesAnn = map (annExp (Just (T.type_ ty)) (doNotRequireLExp env)) values
    castedValuesAnn = map (\x -> (cast (doNotRequireLExp env) x (T.type_ ty))) valuesAnn
    tryLiterals = map ctvToLiteral castedValuesAnn
    (literals, errs) = partitionEithers tryLiterals
    ann = DeclAnn {
        bindings = envFromSymbolBindings (zip idents (map (Const pos) literals)),
        errs = (concat errs) ++ numberErr
    }

-- Annotate a list of variable declarations.
annVarAs :: Env -> [AS.VarA ()] -> [AS.VarA Ann]
annVarAs _ [] = []
annVarAs env (v : vs) = vAnn : annVarAs newEnv vs
    where
        vAnn = annVarA env v
        vEnv = bindings $ AS.varAnnot vAnn
        newEnv = fst $ mergeEnv env vEnv

-- Get the environment of a list of variable declarations.
varAsEnv :: [AS.VarA Ann] -> Env
varAsEnv [] = emptyEnv
varAsEnv (v : vs) = fst $ mergeEnv (bindings $ AS.varAnnot v) (varAsEnv vs)

-- Annotate a variable declaration.
annVarA :: Env -> AS.VarA () -> AS.VarA Ann
annVarA env AS.VarAss {
    AS.varIdents = idents,
    AS.varType = ty,
    AS.varValues = values,
    AS.varPos = pos
} = AS.VarAss {
    AS.varIdents = idents,
    AS.varType = ty,
    AS.varValues = castedValuesAnn,
    AS.varAnnot = ann,
    AS.varPos = pos
} where
    numberErr = if length idents /= length values
                then [E.err pos "The number of identifiers does not match number of values."]
                else []
    valuesAnn = map (annExp (Just (T.type_ ty)) (doNotRequireLExp env)) values
    castedValuesAnn = map (\x -> (cast (doNotRequireLExp env) x (T.type_ ty))) valuesAnn
    errs = T.getErrs $ map (expType . AS.expAnnot) castedValuesAnn
    ann = DeclAnn {
        bindings = envFromSymbolBindings (zip idents (repeat (Var pos ty Nothing))),
        errs = errs ++ numberErr
    }

-- Annotate a list of routine declarations.
annRoutines :: Env -> [AS.Routine ()] -> [AS.Routine Ann]
annRoutines _ [] = []
annRoutines env (r : rs) = annRoutine env r : annRoutines env rs

-- Get the environment of a list of routine declarations.
routinesEnv :: [AS.Routine Ann] -> (Env, [String])
routinesEnv [] = (emptyEnv, [])
routinesEnv (r : rs) = (thisEnv, thisErrs ++ nextErrs) where
    (thisEnv, thisErrs) = mergeEnv (bindings $ AS.routAnnot r) nextEnv
    (nextEnv, nextErrs) = (routinesEnv rs)

-- Annotate a routine declaration.
annRoutine :: Env -> AS.Routine () -> AS.Routine Ann
annRoutine env AS.Function {
    AS.routName = ident,
    AS.params = params,
    AS.returnType = ty,
    AS.routBody = body,
    AS.routPos = pos
} = AS.Function {
    AS.routName = ident,
    AS.params = params,
    AS.returnType = ty,
    AS.routBody = bodyAnn,
    AS.routAnnot = ann,
    AS.routPos = pos
} where
    returnRefErr = case T.type_ ty of
        T.TReference {} -> [E.err pos "Functions cannot return a reference type, but `" ++ PP.identPrinter ident ++ "` does."]
        _ -> []
    paramBindings = map (\p -> (AS.paramName p, Var (AS.paramPos p) (AS.paramType p) (Just (AS.mode p)))) params
    paramEnv = envFromSymbolBindings paramBindings
    env' = addReturn (fst $ mergeEnv env paramEnv) (Just (T.type_ ty))
    -- Annotate the body with the new environment.
    (bodyAnn, env'') = annRoutBody env' body
    -- Get the body errors.
    bodyErrs = errs $ AS.routBodyAnnot bodyAnn
    -- Check if the routine body contains a return statement for every flow of control
    -- (i.e. at the end of the RoutBody the environment is unreachable)
    doNotAlwaysReturnErr = if isUnreachable env''
        then []
        else [E.err pos "The function \"" ++ PP.identPrinter ident ++ "\" does not contain a return statement for every flow of control."]
    -- Function throws if its body throws.
    funThrows = throws $ AS.routBodyAnnot bodyAnn
    -- Create the annotation.
    ann = DeclAnn {
        bindings = envFromRoutBindings [(ident, Fun pos params ty funThrows)],
        errs = returnRefErr ++ bodyErrs ++ doNotAlwaysReturnErr
    }
annRoutine env AS.Procedure {
    AS.routName = ident,
    AS.params = params,
    AS.routBody = body,
    AS.routPos = pos
} = AS.Procedure {
    AS.routName = ident,
    AS.params = params,
    AS.routBody = bodyAnn,
    AS.routAnnot = ann,
    AS.routPos = pos
} where
    paramBindings = map (\p -> (AS.paramName p, Var (AS.paramPos p) (AS.paramType p) (Just (AS.mode p)))) params
    paramEnv = envFromSymbolBindings paramBindings
    env' = addReturn (fst $ mergeEnv env paramEnv) Nothing
    -- Annotate the body with the new environment.
    (bodyAnn, env'') = annRoutBody env' body
    -- Get the body errors.
    bodyErrs = errs $ AS.routBodyAnnot bodyAnn
    -- Check if the routine body contains a return statement for every flow of control
    -- (i.e. at the end of the RoutBody the environment is unreachable)
    doNotAlwaysReturnErr = if isUnreachable env''
        then []
        else [E.err pos "The procedure \"" ++ PP.identPrinter ident ++ "\" does not contain a return statement for every flow of control."]
    -- Procedure throws if its body throws.
    procThrows = throws $ AS.routBodyAnnot bodyAnn
    -- Create the annotation.
    ann = DeclAnn {
        bindings = envFromRoutBindings [(ident, Proc pos params procThrows)],
        errs = bodyErrs ++ doNotAlwaysReturnErr
    }

-- Annotate a routine body.
annRoutBody :: Env -> AS.RoutBody () -> (AS.RoutBody Ann, Env)
annRoutBody env AS.RoutBody {
    AS.declarations = decls,
    AS.statements = stmts
} = (AS.RoutBody {
    AS.declarations = declsAnn,
    AS.statements = stmtsAnn,
    AS.routBodyAnnot = ann
}, env') where
    -- Annotate the declarations and get the environment.
    declsAnn = annDecls env decls
    declEnv = bindings $ AS.declsAnnot declsAnn
    -- Annotate the statements with the environment of the declarations.
    (stmtsAnn, env', _, stmtsThrows) = annStmts declEnv stmts
    -- Errs from the declarations.
    declErrs = errs $ AS.declsAnnot declsAnn
    -- Errs from the statements.
    stmtsErrs = concatMap (errs . AS.annot) stmtsAnn
    -- Stmts throws:
    -- - Never if the reachable statements' throws field is always `Never` (or if there
    --   are no reachable statement);
    -- - Always if it exists a reachable statement whose throws field is `Always`;
    -- - Sometimes otherwise.
    -- reachableStmts = filter (not . unreachable . AS.annot) stmtsAnn
    -- stmtsThrows = if all (== Never) (map (throws . AS.annot) reachableStmts)
    --     then Never
    --     else if any (== Always) (map (throws . AS.annot) reachableStmts)
    --     then Always
    --     else Sometimes
    -- Create the annotation.
    ann = StmtAnn {
        errs = declErrs ++ stmtsErrs,
        unreachable = isUnreachable env,
        throws = stmtsThrows
    }