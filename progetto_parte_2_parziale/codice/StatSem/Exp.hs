-- Implementation of the annotation rules for EXPRESSIONS.
module StatSem.Exp where

import StatSem.Utils
import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS

-- Annotate an expression node.
annExp :: Env -> AS.Exp () -> AS.Exp Ann
annExp env exp@AS.Literal{} = annExpLit env exp
annExp env exp@AS.RoutCall{} = annExpIdent env exp
-- ...

-----------
-- UTILS --
-----------

-- Get type and value from a literal expression.
getTypeAndValue :: AS.Literal -> (AS.Type, CTValue)
getTypeAndValue (AS.LitInt v _) = (AS.TInteger, CTVInteger v)
getTypeAndValue (AS.LitChar v _) = (AS.TChar, CTVChar v)
getTypeAndValue (AS.LitString v _) = (AS.TString, CTVString v)
getTypeAndValue (AS.LitBool v _) = (AS.TBoolean, CTVBoolean v)
getTypeAndValue (AS.LitDouble v _) = (AS.TReal, CTVReal v)

-----------
-- LEAFS --
-----------

-- Literal expressions.
-- Rules: RExpInt, RExpChar, RExpString, RExpBool, RExpDouble.
annExpLit :: Env -> AS.Exp () -> AS.Exp Ann
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
annExpIdent :: Env -> AS.Exp () -> AS.Exp Ann
annExpIdent env
            AS.ExpIdent {
                AS.name = ident,
                AS.expPos = expPos
            } = AS.ExpIdent {
    AS.name = ident,
    AS.expAnnot = ann,
    AS.expPos = expPos
} where
    ann = IdentAnn {
            identType = identType,
            identVal = identVal,
            identCategory = identCategory
        }
    symbolType = Map.lookup ident (symbols env)
    routineType = Map.lookup ident (routines env)
    (identType, identVal, identCategory) = case (symbolType, routineType) of
        (Nothing, Nothing) -> ([AS.TError { AS.msgs = ["Unbound identifier: " ++ show ident] }], CTVUnknown, ErrorCategory)
        (Just (Var pos ty), Nothing) -> ([AS.type_ ty], CTVUnknown, VarCategory) -- Variable case
        (Just (Const val), Nothing) -> ([identType], identVal, ConstCategory) where (identType, identVal) = getTypeAndValue val -- Constant case
        (Nothing, Just (pos, params, ty)) -> (map AS.type_ ty, CTVUnknown, RoutineCategory) -- Routine case
        -- TODO: the case in which the ambiguity can be resolved by the type
        _ -> ([AS.TError { AS.msgs = ["Ambiguous identifier: " ++ show ident] }], CTVUnknown, ErrorCategory) -- Ambiguous case

--------------------
-- INTERNAL NODES --
--------------------

-- Routine call expressions.
-- Rule: RExpCall.
annExpRoutCall :: Env -> AS.Exp () -> AS.Exp Ann
annExpRoutCall env
            AS.RoutCall {
                AS.expCall = AS.RoutineCall {
                    AS.routCallName = ident,
                    AS.args = args,
                    AS.routCallPos = pos
                },
                AS.expPos = exprPos
            } = AS.RoutCall {
    AS.expCall = AS.RoutineCall {
                    AS.routCallName = ident,
                    AS.args = annArgs,
                    AS.routCallPos = pos
                },
    AS.expAnnot = ann,
    AS.expPos = exprPos
} where
    ann = ExpAnn {
            expType = routType,
            expVal = CTVUnknown -- TODO: do we need to compute the value at compile time if possible?
        }
    
    annArgs = map (annExp env) args
    routType = case Map.lookup ident (routines env) of
        Just (pos, params, ty) -> case checkParamTypes params annArgs of
            True  -> map AS.type_ ty
            False -> [AS.TError {
                AS.msgs = ["Wrong argument types in routine call: " ++ show ident]
            }]
        Nothing       -> [AS.TError {
            AS.msgs = ["Unbound routine: " ++ show ident]
        }]

checkParamTypes :: [AS.Param] -> [AS.Exp Ann] -> Bool
checkParamTypes params args = checkParamTypesBuffered params [] args

checkParamTypesBuffered :: [AS.Param] -> [AS.Type] -> [AS.Exp Ann] -> Bool
checkParamTypesBuffered [] [] [] = True
checkParamTypesBuffered (AS.Param { AS.paramType = AS.TypePos { AS.type_ = paramType } } : ps) (argType : argTypes) args =
    paramType == argType && checkParamTypesBuffered ps argTypes args
checkParamTypesBuffered ps [] (arg : args) =
    case AS.expAnnot arg of
        ExpAnn { expType = argTypes } -> checkParamTypesBuffered ps argTypes args
        _ -> False
checkParamTypesBuffered _ _ _ = False