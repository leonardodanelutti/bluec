-- Implementation of the annotation rules for EXPRESSIONS.
module StatSem.Exp where

import StatSem.Utils
import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified Errors         as E
import qualified Types          as T
import qualified LexGrammar     as LG

-- TYPES
-- Void annotations after parsing.
type Exp = AS.Exp ()
-- Annotated expressions after static semantics analysis.
type AnnExp = AS.Exp Ann

-- Annotate an expression node.
annExp :: Env -> Exp -> AnnExp
annExp env exp@AS.Literal{} = annExpLit env exp
annExp env exp@AS.ExpIdent{} = annExpIdent env exp
annExp env exp@AS.FunCall{} = annExpRoutCall env exp
annExp env exp@AS.Sign{} = annExpSign env exp
-- ...

-----------
-- UTILS --
-----------

-- Get type and value from a literal expression.
getTypeAndValue :: AS.Literal -> (T.Type, CTValue)
getTypeAndValue (AS.LitInt v _) = (T.TInteger, CTVInteger v)
getTypeAndValue (AS.LitChar v _) = (T.TChar, CTVChar v)
getTypeAndValue (AS.LitString v _) = (T.TString, CTVString v)
getTypeAndValue (AS.LitBool v _) = (T.TBoolean, CTVBoolean v)
getTypeAndValue (AS.LitDouble v _) = (T.TReal, CTVReal v)

-- Check if the expected parameter types match the argument types.
checkParamTypes :: [AS.Param] -> [AS.Exp Ann] -> Bool
checkParamTypes [] [] = True
checkParamTypes (AS.Param { AS.paramType = T.TypePos { T.type_ = paramType } } : ps) (arg : args) =
    paramType == argType && checkParamTypes ps args
    where
        argType = expType $ AS.expAnnot arg
checkParamTypes _ _ = False

-- Cast an annotated expression to a specific type.
cast :: AnnExp -> T.Type -> AnnExp
cast exp newType = case expType ann of
    T.TError {} -> exp
    oldType | oldType == newType -> exp
    oldType -> case T.sup pos oldType newType of
        err@T.TError {} -> AS.Cast { AS.e = exp, AS.expAnnot = ExpAnn { expType = err, ctVal = Nothing }, AS.expPos = pos }
        _ -> case (exp, valCast) of
            (AS.Cast {}, _) -> exp { AS.expAnnot = castAnn } -- Simplify cast chains. (FIXME: è giusto fare sta cosa?)
            _ -> AS.Cast { AS.e = exp, AS.expAnnot = castAnn, AS.expPos = pos }
    where
        pos = AS.expPos exp
        ann = AS.expAnnot exp
        valCast = ctCast (getCTValue ann) newType -- Compile-time cast.
        castAnn = ExpAnn {
            expType = newType,
            ctVal   = valCast
        }

-----------
-- LEAFS --
-----------

-- Literal expressions.
-- Rules: RExpInt, RExpChar, RExpString, RExpBool, RExpDouble.
annExpLit :: Env -> Exp -> AnnExp
annExpLit _ AS.Literal { AS.val = exp, AS.expPos = expPos } = AS.Literal {
    AS.val = exp,
    AS.expAnnot = ann,
    AS.expPos = expPos
} where
    ann = ExpAnn {
        expType = expType,
        ctVal   = Just ctVal
    }
    (expType, ctVal) = getTypeAndValue exp
annExpLit _ _ = error "Unreachable: `annExpLit` called with non-`Literal` expression."

-- Identifier expressions.
-- Rule: LExpBase.
annExpIdent :: Env -> Exp -> AnnExp
annExpIdent env AS.ExpIdent {
    AS.name = ident,
    AS.expPos = pos
} = AS.ExpIdent {
    AS.name = ident,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    symbolType = Map.lookup ident (symbols env)
    routineType = Map.lookup ident (routines env)
    ann = case (symbolType, routineType) of
        -- Identifier not found.
        (Nothing, Nothing) -> IdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Unbound identifier: " ++ show ident] },
            ctVal = Nothing,
            identCategory = ErrorCategory
        }
        -- Var identifier.
        (Just (Var _ ty), Nothing) -> IdentAnn {
            expType = T.type_ ty,
            ctVal = Nothing,
            identCategory = VarCategory
        }
        -- Const identifier.
        (Just (Const val), Nothing) -> IdentAnn {
            expType = identType,
            ctVal = Just identVal,
            identCategory = ConstCategory
        } where (identType, identVal) = getTypeAndValue val
        -- Routine identifier.
        (Nothing, Just _) -> annRoutine env ident [] pos
        -- TODO: the case in which the ambiguity can be resolved by the type
        -- Ambiguous case.
        _ -> IdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Ambiguous identifier: " ++ show ident] },
            ctVal = Nothing,
            identCategory = ErrorCategory
        }
annExpIdent _ _ = error "Unreachable: `annExpIdent` called with non-`ExpIdent` expression."

--------------------
-- INTERNAL NODES --
--------------------

-- Routine call expressions.
-- Rule: RExpCall.
annExpRoutCall :: Env -> Exp -> AnnExp
annExpRoutCall env fc@AS.FunCall {
    AS.expCall = rc@AS.RoutineCall {
        AS.routCallName = ident,
        AS.args = args
    },
    AS.expPos = pos
} = fc {
    AS.expCall = rc { AS.args = annArgs },
    AS.expAnnot = annRoutine env ident annArgs pos
} where
    annArgs = map (annExp env) args
annExpRoutCall _ _ = error "Unreachable: `annExpRoutCall` called with non-`FunCall` expression."

annRoutine :: Env -> AS.Ident -> [AnnExp] -> LG.Posn -> Ann
annRoutine env ident annArgs pos = ExpAnn {
    expType = routType,
    ctVal = Nothing
} where
    routType = case Map.lookup ident (routines env) of
        Just (Fun _ params ty) -> case checkParamTypes params annArgs of
            True  -> T.type_ ty
            False -> T.TError {
                T.msgs = [E.err pos "Wrong argument types in routine call: " ++ show ident]
            }
        Just (Proc _ _) -> T.TError {
            T.msgs = [E.err pos "Cannot call a procedure as an expression: " ++ show ident ++ "."]
        }
        Nothing -> T.TError {
            T.msgs = [E.err pos "Unbound routine: " ++ show ident]
        }

-- Sign expressions.
-- Rule: RExpSign.
annExpSign :: Env -> Exp -> AnnExp
annExpSign env AS.Sign {
    AS.e = e,
    AS.expPos = pos
} = AS.Sign {
    AS.e = annE,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    annE = annExp env e
    intCastedAnnE = cast annE T.TInteger
    realCastedAnnE = cast annE T.TReal
    ann = case (expType $ AS.expAnnot intCastedAnnE, expType $ AS.expAnnot realCastedAnnE) of
        (T.TInteger, _) -> ExpAnn {
            expType = T.TInteger,
            ctVal = case (getCTValue $ AS.expAnnot annE) of
                Just (CTVInteger v) -> Just (CTVInteger (-v))
                Nothing             -> Nothing
                ctv                 -> error $ "Type unconsistency found: exp annotated as Integer but has " ++ show ctv ++ "." -- Should be unreachable.
        }
        (_, T.TReal) -> ExpAnn {
            expType = T.TReal,
            ctVal = case (getCTValue $ AS.expAnnot annE) of
                Just (CTVReal v) -> Just (CTVReal (-v))
                Nothing          -> Nothing
                ctv              -> error $ "Type unconsistency found: exp annotated as Real but has " ++ show ctv ++ "." -- Should be unreachable.
        }
        _ -> ExpAnn {
            expType = T.TError {
                T.msgs = [E.err pos "Cannot apply sign to expression of type " ++ show (expType $ AS.expAnnot annE)]
            },
            ctVal = Nothing
        }
annExpSign _ _ = error "Unreachable: `annExpSign` called with non-`Sign` expression."

-- Bool AND expressions.
-- Rule: RExpAnd.
annExpAnd :: Env -> Exp -> AnnExp
annExpAnd env AS.BinOp {
    AS.binOp = AS.And,
    AS.e1 = e1,
    AS.e2 = e2,
    AS.expPos = pos
} = AS.BinOp {
    AS.binOp = AS.And,
    AS.e1 = castedE1,
    AS.e2 = castedE2,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    ann = ExpAnn {
        expType = newType,
        ctVal = newVal
    }
    -- Call recursive annotation for both expressions.
    annE1 = annExp env e1
    annE2 = annExp env e2
    -- Get types.
    typeE1 = expType $ AS.expAnnot annE1
    typeE2 = expType $ AS.expAnnot annE2
    -- Cast both expressions to Boolean if possible.
    castedE1 = cast annE1 T.TBoolean
    castedE2 = cast annE2 T.TBoolean
    castedTypeE1 = expType $ AS.expAnnot castedE1
    castedTypeE2 = expType $ AS.expAnnot castedE2
    castedValE1 = ctVal $ AS.expAnnot castedE1
    castedValE2 = ctVal $ AS.expAnnot castedE2
    castedType = T.sup pos castedTypeE1 castedTypeE2
    (newType, newVal) = case (castedType, castedValE1, castedValE2) of
        (T.TBoolean, Just (CTVBoolean v1), Just (CTVBoolean v2)) -> (T.TBoolean, Just (CTVBoolean (v1 && v2)))
        (T.TBoolean, _, _) -> (T.TBoolean, Nothing)
        (T.TError { T.msgs = msgs }, _, _) -> (T.TError { T.msgs = msgs }, Nothing)
        _ -> (T.TError { T.msgs = [E.err pos $ "Type mismatch: AND operation expects two types compatible with Boolean, but got " ++ show typeE1 ++ " and " ++ show typeE2 ++ " instead."] }, Nothing)
annExpAnd _ _ = error "Unreachable: `annExpAnd` called with non-`BinOp` expression."

-- Bool OR expressions.
-- Rule: RExpOr.

-- Bool NOT expressions.
-- Rule: RExpNeg.
