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
        ExpAnn { expType = argTypes } -> checkParamTypesBuffered ps argTypes args
        _ -> False
checkParamTypesBuffered _ _ _ = False

-- -- Get the number of types of an annotated expression.
-- getExpTypeCount :: AnnExp -> Int
-- getExpTypeCount AS.Literal { AS.expAnnot =ann } = getAnnTypeCount ann

-- -- Check if an annotated expression has one and only one type.
-- isSingleType :: AnnExp -> Bool
-- isSingleType exp = getExpTypeCount exp == 1

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
            expType     = [expType],
            expVal      = expVal
        }
    (expType, expVal) = getTypeAndValue exp

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
    ann = IdentAnn {
            identType = identType,
            identVal = identVal,
            identCategory = identCategory
        }
    symbolType = Map.lookup ident (symbols env)
    routineType = Map.lookup ident (routines env)
    (identType, identVal, identCategory) = case (symbolType, routineType) of
        (Nothing, Nothing) -> ([T.TError { T.msgs = [E.err pos "Unbound identifier: " ++ show ident] }], CTVUnknown, ErrorCategory)
        (Just (Var pos ty), Nothing) -> ([T.type_ ty], CTVUnknown, VarCategory) -- Variable case
        (Just (Const val), Nothing) -> ([identType], identVal, ConstCategory) where (identType, identVal) = getTypeAndValue val -- Constant case
        (Nothing, Just (pos, params, ty)) -> (map T.type_ ty, CTVUnknown, RoutineCategory) -- Routine case
        -- TODO: the case in which the ambiguity can be resolved by the type
        _ -> ([T.TError { T.msgs = [E.err pos "Ambiguous identifier: " ++ show ident] }], CTVUnknown, ErrorCategory) -- Ambiguous case

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
    ann = ExpAnn {
            expType = routType,
            expVal = CTVUnknown -- TODO: do we need to compute the value at compile time if possible?
        }
    
    annArgs = map (annExp env) args
    routType = case Map.lookup ident (routines env) of
        Just (pos, params, ty) -> case checkParamTypes params annArgs of
            True  -> map T.type_ ty
            False -> [T.TError {
                T.msgs = [E.err expPos "Wrong argument types in routine call: " ++ show ident]
            }]
        Nothing       -> [T.TError {
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
            expVal = val
        }
    annE = annExp env e
    typeE = expType $ AS.expAnnot annE
    valE = expVal $ AS.expAnnot annE
    ty = case typeE of
        t@[T.TInteger] -> t
        t@[T.TReal]    -> t
        -- TODO: add types that can be casted to Integer or Real
        t@[T.TError { T.msgs = msgs }] -> t
        _ -> [T.TError { T.msgs = [E.err pos $ "Sign operation not applicable to type: " ++ show typeE] }]-- TODO: error handling with loc
    val = case valE of
        CTVInteger v -> CTVInteger (-v)
        CTVReal v    -> CTVReal (-v)
        CTVUnknown   -> CTVUnknown
        _            -> CTVUnknown -- Sign operation not applicable to other types

-- Bool AND expressions.
-- Rule: RExpAnd.
annExpAnd :: Env -> Exp -> AnnExp
annExpAnd env
            AS.BoolAnd {
                AS.e1 = e1,
                AS.e2 = e2,
                AS.expPos = pos
            } = AS.BoolAnd {
    AS.e1 = annE1,
    AS.e2 = annE2,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    ann = ExpAnn {
            expType = mergedTypes,
            expVal = val
        }
    annE1 = annExp env e1
    annE2 = annExp env e2
    typeE1 = expType $ AS.expAnnot annE1
    typeE2 = expType $ AS.expAnnot annE2
    valE1 = expVal $ AS.expAnnot annE1
    valE2 = expVal $ AS.expAnnot annE2
    val = case (valE1, valE2) of
        (CTVBoolean v1, CTVBoolean v2) -> CTVBoolean (v1 && v2)
        _ -> CTVUnknown
    -- Check if both expressions can be casted to Boolean.
    -- If not, annotate an error.
    typesCompatibility1 = T.supL pos [T.TBoolean] typeE1
    typesCompatibility2 = T.supL pos [T.TBoolean] typeE2
    mergedTypes = case (typesCompatibility1, typesCompatibility2) of
        ([T.TBoolean], [T.TBoolean]) -> [T.TBoolean]
        ([t1], [t2]) -> [T.mergeErrs [t1, t2]]


-- Bool OR expressions.
-- Rule: RExpOr.

-- Bool NOT expressions.
-- Rule: RExpNeg.
