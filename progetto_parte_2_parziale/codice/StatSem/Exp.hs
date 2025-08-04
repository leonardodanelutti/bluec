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
annExp env exp@AS.BinOp{} = annExpBinOp env exp
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
        ty | ty == newType -> case (exp, valCast) of
            (AS.Cast {}, _) -> exp { AS.expAnnot = castAnn } -- Simplify cast chains. (FIXME: è giusto fare sta cosa?)
            _ -> AS.Cast { AS.e = exp, AS.expAnnot = castAnn, AS.expPos = pos }
        _ -> AS.Cast {
            AS.e = exp,
            AS.expAnnot = ExpAnn {
                expType = T.TError { T.msgs = [E.err pos $ "Cannot cast expression of type " ++ show oldType ++ " to " ++ show newType ++ "."] },
                ctVal = Nothing },
            AS.expPos = pos }
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
            expType = T.TError { T.msgs = [E.err pos "Unbound identifier: " ++ show ident ++ "."] },
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
        (Nothing, Just _) -> annRoutineCall env ident [] pos
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
    AS.expAnnot = annRoutineCall env ident annArgs pos
} where
    annArgs = map (annExp env) args
annExpRoutCall _ _ = error "Unreachable: `annExpRoutCall` called with non-`FunCall` expression."

-- Helper function that creates an annotation for a routine call.
annRoutineCall :: Env -> AS.Ident -> [AnnExp] -> LG.Posn -> Ann
annRoutineCall env ident annArgs pos = RoutCallAnn {
    expType = routType,
    params = params
} where
    (routType, params) = case Map.lookup ident (routines env) of
        Just (Fun _ params ty) -> case checkParamTypes params annArgs of
            True  -> (T.type_ ty, params)
            False -> (T.TError {
                T.msgs = [E.err pos "Wrong argument types in routine call: " ++ show ident]
            }, [])
        Just (Proc _ params) -> (T.TError {
            T.msgs = [E.err pos "Cannot call a procedure as an expression: " ++ show ident ++ "."]
        }, [])
        Nothing -> (T.TError {
            T.msgs = [E.err pos "Unbound routine: " ++ show ident ++ "."]
        }, [])

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

-- Binary Operation expressions.
-- Rule: RExpOr, RExpAnd, RExpPlus, RExpMinus, RExpMult, RExpDivReal, RExpDivInt, RExpMod,
-- RExpPow.
annExpBinOp :: Env -> Exp -> AnnExp
annExpBinOp env AS.BinOp {
    AS.binOp = binOp,
    AS.e1 = e1,
    AS.e2 = e2,
    AS.expPos = pos
} = AS.BinOp {
    AS.binOp = binOp,
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
    -- Get expected types.
    expectedTypes = AS.typeOfBinOpArgs binOp
    -- Cast both expressions to the expected types (try for each type).
    castedsE1 = map (cast annE1) expectedTypes
    castedsE2 = map (cast annE2) expectedTypes
    castedTypesE1 = map (expType . AS.expAnnot) castedsE1
    castedTypesE2 = map (expType . AS.expAnnot) castedsE2
    castedValsE1 = map (ctVal . AS.expAnnot) castedsE1
    castedValsE2 = map (ctVal . AS.expAnnot) castedsE2
    castedTypes = zipWith (T.sup pos) castedTypesE1 castedTypesE2
    -- Input:
    -- - list of types to which the expressions are tried to be casted (in order of
    --   precedence).
    -- - 2 lists of compile-time values of the casted expressions (one for each
    --   expression; in the same order above).
    -- - 2 lists of casted expressions (one for each expression; in the same order above).
    -- Output:
    -- - the first type to which the cast is successful, or an error type if no cast is
    --   successful.
    -- - the compile-time value of the binary operation, if both operands are compile-time
    --   values and the operation is successful.
    -- - the 2 casted expressions.
    getTypeAndVal :: [T.Type] -> [Maybe CTValue] -> [Maybe CTValue] -> [AnnExp] -> [AnnExp] -> (T.Type, Maybe CTValue, AnnExp, AnnExp)
    getTypeAndVal [] [] [] [] [] = (T.TError { T.msgs = [E.err pos "Type mismatch: the binary operation " ++ show binOp ++ " expects operands to be compatible with one of the following types: " ++ show expectedTypes ++ ", but got " ++ show typeE1 ++ " and " ++ show typeE2 ++ " instead."] }, Nothing, last castedsE1, last castedsE2)
    getTypeAndVal (at:ats) (v1:vs1) (v2:vs2) (c1:cs1) (c2:cs2) = case (at, v1, v2) of
        (T.TError { T.msgs = msgs }, _, _) -> getTypeAndVal ats vs1 vs2 cs1 cs2
        (_, Just v1, Just v2) -> (at, Just $ applyBinOp binOp v1 v2, c1, c2)
        _ -> (at, Nothing, c1, c2)
    getTypeAndVal _ _ _ _ _ = error "Unreachable: `getTypeAndVal` called with lists of different lengths."
    (newType, newVal, castedE1, castedE2) = getTypeAndVal castedTypes castedValsE1 castedValsE2 castedsE1 castedsE2
annExpAnd _ _ = error "Unreachable: `annExpAnd` called with non-`BinOp` expression."

-- Bool NOT expressions.
-- Rule: RExpNeg.
