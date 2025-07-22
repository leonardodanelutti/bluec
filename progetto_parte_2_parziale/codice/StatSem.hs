-- Static semantics analysis module for the compiler.

module StatSem where

import qualified Data.Map.Lazy as Map
import qualified AbsSyn as AS
import qualified LexGrammar as LG

-- Environment with different namespaces for variables, functions, and constants.
--
-- Routine calls without parameters and variables can be indistinguishable if have the
-- same name and same type. In those cases it will be necessary to use the parentheses
-- to disambiguate between a variable and a function call.
data Env = Env {
    vars :: Map.Map AS.Ident (LG.Posn, AS.TypePos),
    routines :: Map.Map AS.Ident (LG.Posn, [AS.Param], AS.TypePos),
    consts :: Map.Map AS.Ident AS.Literal
}

-- Datatype for static semantics analysis module annotations (on the AST).
data Annotation
    = ExpAnn {
        expType :: AS.Type,
        expVal :: CTValue
        -- TODO: other fileds
    }
    | IdentAnn {
        identType :: AS.Type,
        identVal :: CTValue
    }

-- Values known at compile time.
data CTValue
    = CTVInteger Integer
    | CTVChar Char
    | CTVString String
    | CTVBoolean Bool
    | CTVReal Double
    | CTVUnknown -- Represents a value that is not known at compile time.

-- SDD ANNOTATION RULES

-- EXPRESSIONS

checkExp :: Env -> AS.Exp () -> AS.Exp Annotation
checkExp env exp@AS.Literal{} = checkExpLit env exp
checkExp env exp@AS.RoutCall{} = checkExpIdent env exp
-- ...

-- LEAFS

-- Literal expressions.
-- Rules: RExpInt, RExpChar, RExpString, RExpBool, RExpDouble.
checkExpLit :: Env -> AS.Exp () -> AS.Exp Annotation
checkExpLit env AS.Literal { AS.val = exp, AS.expPos = expPos } = AS.Literal {
    AS.val      = exp,
    AS.expAnnot = ann,
    AS.expPos   = expPos
} where
    ann = ExpAnn {
            expType     = expType,
            expVal      = expVal
        }
    (expType, expVal) = case exp of
        AS.LitInt v _ -> (AS.TInteger, CTVInteger v)
        AS.LitChar v _ -> (AS.TChar, CTVChar v)
        AS.LitString v _ -> (AS.TString, CTVString v)
        AS.LitBool v _ -> (AS.TBoolean, CTVBoolean v)
        AS.LitDouble v _ -> (AS.TReal, CTVReal v)

-- Identifier expressions.
-- Rule: LExpBase.
checkExpIdent :: Env -> AS.Exp () -> AS.Exp Annotation
checkExpIdent env
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
                    AS.args = map (checkExp env) args,
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