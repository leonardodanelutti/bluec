-- Implementation of the annotation rules for EXPRESSIONS.
module StatSem.Exp where

import StatSem.Utils
import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified Errors         as E
import qualified Types          as T

-- TYPES
-- Void annotations after parsing.
type Exp = AS.Exp ()
-- Annotated expressions after static semantics analysis.
type AnnExp = AS.Exp Ann

-- Annotate an expression node.
annExp :: Env -> Exp -> AnnExp
annExp env exp@AS.Literal{} = annExpLit env exp
annExp env exp@AS.ExpIdent{} = annExpIdent env exp
annExp env exp@AS.RoutCall{} = annExpRoutCall env exp
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
checkParamTypes params args = checkParamTypesBuffered params [] args

-- checkParamTypes with buffered argument types. This is useful because each argument
-- can have multiple types (e.g., if it is an identifier).
--
-- For example:
-- ...
-- routines
-- foo() -> Integer, Boolean is ... do ... end foo
-- bar(a: Integer, b: Boolean) is ... do ... end bar
-- ...
-- bar(foo())
-- ...
checkParamTypesBuffered :: [AS.Param] -> [T.Type] -> [AS.Exp Ann] -> Bool
checkParamTypesBuffered [] [] [] = True
checkParamTypesBuffered (AS.Param { AS.paramType = T.TypePos { T.type_ = paramType } } : ps) (argType : argTypes) args =
    paramType == argType && checkParamTypesBuffered ps argTypes args
checkParamTypesBuffered ps [] (arg : args) =
    case AS.expAnnot arg of
        ExpAnn { expType = argType } -> checkParamTypesBuffered ps [argType] args
        IdentAnn { expType = argType } -> checkParamTypesBuffered ps [argType] args
        RoutAnn { expTypes = argTypes } -> checkParamTypesBuffered ps argTypes args
checkParamTypesBuffered _ _ _ = False

-- -- Get the number of types of an annotated expression.
-- getExpTypeCount :: AnnExp -> Int
-- getExpTypeCount AS.Literal { AS.expAnnot =ann } = getAnnTypeCount ann

-- -- Check if an annotated expression has one and only one type.
-- isSingleType :: AnnExp -> Bool
-- isSingleType exp = getExpTypeCount exp == 1

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
annExpLit env AS.Literal { AS.val = exp, AS.expPos = expPos } = AS.Literal {
    AS.val      = exp,
    AS.expAnnot = ann,
    AS.expPos   = expPos
} where
    ann = ExpAnn {
            expType = expType,
            ctVal   = Just ctVal
        }
    (expType, ctVal) = getTypeAndValue exp

-- Identifier expressions.
-- Rule: LExpBase.
annExpIdent :: Env -> Exp -> AnnExp
annExpIdent env
            AS.ExpIdent {
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
        (Just (Var pos ty), Nothing) -> IdentAnn {
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
        (Nothing, Just (pos, params, ty)) -> RoutAnn {
            expTypes = map T.type_ ty
        }
        -- TODO: the case in which the ambiguity can be resolved by the type
        -- Ambiguous case.
        _ -> IdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Ambiguous identifier: " ++ show ident] },
            ctVal = Nothing,
            identCategory = ErrorCategory
        }

--------------------
-- INTERNAL NODES --
--------------------

-- Routine call expressions.
-- Rule: RExpCall.
annExpRoutCall :: Env -> Exp -> AnnExp
annExpRoutCall env
            AS.RoutCall {
                AS.expCall = AS.RoutineCall {
                    AS.routCallName = ident,
                    AS.args = args,
                    AS.routCallPos = pos
                },
                AS.expPos = expPos
            } = AS.RoutCall {
    AS.expCall = AS.RoutineCall {
                    AS.routCallName = ident,
                    AS.args = annArgs,
                    AS.routCallPos = pos
                },
    AS.expAnnot = ann,
    AS.expPos = expPos
} where
    ann = RoutAnn {
            expTypes = routType
        }
    
    annArgs = map (annExp env) args
    routType = case Map.lookup ident (routines env) of
        Just (_, params, ty) -> case checkParamTypes params annArgs of
            True  -> map T.type_ ty
            False -> [T.TError {
                T.msgs = [E.err expPos "Wrong argument types in routine call: " ++ show ident]
            }]
        Nothing -> [T.TError {
            T.msgs = [E.err expPos "Unbound routine: " ++ show ident]
        }]

-- Sign expressions.
-- Rule: RExpSign.
annExpSign :: Env -> Exp -> AnnExp
annExpSign env
            AS.Sign {
                AS.e = e,
                AS.expPos = pos
            } = AS.Sign {
    AS.e = annE,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    ann = ExpAnn {
        expType = ty,
        ctVal = val
    }
    annE = annExp env e
    typeE = getType $ AS.expAnnot annE
    valE = ctVal $ AS.expAnnot annE
    ty = case typeE of
        SingleType t@T.TInteger -> t
        SingleType t@T.TReal    -> t
        -- TODO: add types that can be casted to Integer or Real
        SingleType t@T.TError {} -> t
        MultipleTypes ts -> case T.getErrs ts of
            [] -> T.TError { T.msgs = [E.err pos "Sign operation not applicable to multiple types: " ++ show ts ++ "."] }
            errs -> T.TError { T.msgs = errs }
        _ -> T.TError { T.msgs = [E.err pos $ "Sign operation not applicable to type: " ++ show typeE ++ "."] }-- TODO: error handling with loc
    val = case valE of
        Just (CTVInteger v) -> Just (CTVInteger (-v))
        Just (CTVReal v)    -> Just (CTVReal (-v))
        _                   -> Nothing

-- Bool AND expressions.
-- Rule: RExpAnd.
annExpAnd :: Env -> Exp -> AnnExp
annExpAnd env
            AS.BinOp {
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
    typeE1 = getType $ AS.expAnnot annE1
    typeE2 = getType $ AS.expAnnot annE2
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

-- Bool OR expressions.
-- Rule: RExpOr.

-- Bool NOT expressions.
-- Rule: RExpNeg.
