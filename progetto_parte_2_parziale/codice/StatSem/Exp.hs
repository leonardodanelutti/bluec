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

-- LEAFS

-- Literal expressions.
-- Rules: RExpInt, RExpChar, RExpString, RExpBool, RExpDouble.
annExpLit :: Env -> AS.Exp () -> AS.Exp Ann
annExpLit env AS.Literal { AS.val = exp, AS.expPos = expPos } = AS.Literal {
    AS.val      = exp,
    AS.expAnnot = ann,
    AS.expPos   = expPos
} where
    ann = ExpAnn {
            expType     = expType,
            expVal      = expVal
        }
    (expType, expVal) = getTypeAndValue exp

-- Get type and value from a literal expression.
getTypeAndValue :: AS.Literal -> (AS.Type, CTValue)
getTypeAndValue (AS.LitInt v _) = (AS.TInteger, CTVInteger v)
getTypeAndValue (AS.LitChar v _) = (AS.TChar, CTVChar v)
getTypeAndValue (AS.LitString v _) = (AS.TString, CTVString v)
getTypeAndValue (AS.LitBool v _) = (AS.TBoolean, CTVBoolean v)
getTypeAndValue (AS.LitDouble v _) = (AS.TReal, CTVReal v)

-- Identifier expressions.
-- Rule: LExpBase.
annExpIdent :: Env -> AS.Exp () -> AS.Exp Ann
annExpIdent env
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
                    AS.args = map (annExp env) args,
                    AS.routCallPos = pos
                },
    AS.expAnnot = ann,
    AS.expPos = exprPos
} where
    ann = ExpAnn {
            expType = routType,
            expVal = CTVUnknown -- TODO: do we need to compute the value at compile time if possible?
        }
    routType = case Map.lookup ident (routines env) of
        Just (pos, params, ty) -> AS.type_ ty -- TODO: check args against params
        Nothing       -> AS.TError {
            AS.msgs = ["Unbound routine: " ++ show ident]
        }
-- TODO: vars and consts.
annExpIdent env
            AS.ExpIdent {
                AS.name = ident,
                AS.expPos = expPos
            } = AS.ExpIdent {
    AS.name = ident,
    AS.expAnnot = ann,
    AS.expPos = expPos
} where
    ann = ExpAnn {
            expType = expType,
            expVal = expVal
        }
    symbolType = Map.lookup ident (symbols env)
    routineType = Map.lookup ident (routines env)
    (expType, expVal) = case (symbolType, routineType) of
        (Nothing, Nothing) -> (AS.TError { AS.msgs = ["Unbound identifier: " ++ show ident] }, CTVUnknown)
        (Just (Var pos ty), Nothing) -> (AS.type_ ty, CTVUnknown) -- Variable case
        (Just (Const val), Nothing) -> getTypeAndValue val -- Constant case
        (Nothing, Just (pos, params, ty)) -> (AS.type_ ty, CTVUnknown) -- Routine case
        _ -> (AS.TError { AS.msgs = ["Ambiguous identifier: " ++ show ident] }, CTVUnknown) -- Ambiguous case