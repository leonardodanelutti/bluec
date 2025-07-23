-- Implementation of the annotation rules for EXPRESSIONS.
module StatSem.Exp where

import StatSem.Utils
import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified Errors         as E

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
getTypeAndValue :: AS.Literal -> (AS.Type, CTValue)
getTypeAndValue (AS.LitInt v _) = (AS.TInteger, CTVInteger v)
getTypeAndValue (AS.LitChar v _) = (AS.TChar, CTVChar v)
getTypeAndValue (AS.LitString v _) = (AS.TString, CTVString v)
getTypeAndValue (AS.LitBool v _) = (AS.TBoolean, CTVBoolean v)
getTypeAndValue (AS.LitDouble v _) = (AS.TReal, CTVReal v)

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
checkParamTypesBuffered :: [AS.Param] -> [AS.Type] -> [AS.Exp Ann] -> Bool
checkParamTypesBuffered [] [] [] = True
checkParamTypesBuffered (AS.Param { AS.paramType = AS.TypePos { AS.type_ = paramType } } : ps) (argType : argTypes) args =
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
        (Nothing, Nothing) -> ([AS.TError { AS.msgs = [E.err pos "Unbound identifier: " ++ show ident] }], CTVUnknown, ErrorCategory)
        (Just (Var pos ty), Nothing) -> ([AS.type_ ty], CTVUnknown, VarCategory) -- Variable case
        (Just (Const val), Nothing) -> ([identType], identVal, ConstCategory) where (identType, identVal) = getTypeAndValue val -- Constant case
        (Nothing, Just (pos, params, ty)) -> (map AS.type_ ty, CTVUnknown, RoutineCategory) -- Routine case
        -- TODO: the case in which the ambiguity can be resolved by the type
        _ -> ([AS.TError { AS.msgs = [E.err pos "Ambiguous identifier: " ++ show ident] }], CTVUnknown, ErrorCategory) -- Ambiguous case

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
            True  -> map AS.type_ ty
            False -> [AS.TError {
                AS.msgs = [E.err expPos "Wrong argument types in routine call: " ++ show ident]
            }]
        Nothing       -> [AS.TError {
            AS.msgs = [E.err expPos "Unbound routine: " ++ show ident]
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
        t@[AS.TInteger] -> t
        t@[AS.TReal]    -> t
        -- TODO: add types that can be casted to Integer or Real
        t@[AS.TError { AS.msgs = msgs }] -> t
        _ -> [AS.TError { AS.msgs = [E.err pos $ "Sign operation not applicable to type: " ++ show typeE] }]-- TODO: error handling with loc
    val = case valE of
        CTVInteger v -> CTVInteger (-v)
        CTVReal v    -> CTVReal (-v)
        CTVUnknown   -> CTVUnknown
        _            -> CTVUnknown -- Sign operation not applicable to other types