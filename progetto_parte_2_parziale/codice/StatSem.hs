-- Static semantics analysis module for the compiler.

module StatSem where

import qualified Data.Map.Lazy as Map
import qualified AbSyn as AS

-- Environment bindings.
type Env = Map.Map AS.Ident EnvEntry

data EnvEntry
    = Variable AS.Pos AS.Type
    | Function AS.Pos [ AS.Parameter ] AS.Type
    | Constant AS.Literal

-- Datatype for static semantics analysis module annotations (on the AST).
data Annotation
    = ExprAnn {
        exprType :: T.Type,
        exprVal :: CTValue,
        -- TODO: other fileds
    }

-- Values known at compile time.
data CTValue
    = CTVInt Int
    | CTVFloat Float
    | CTVChar Char
    | CTVString String
    | CTVBool Bool

-- SDD RULES

-- Annotation rules for expressions.

-- Literal expressions.
checkExprLit :: E.Env -> ExprParsed -> ExprAnnot
checkExprLit env expr = AS.Literal {
    AS.val       = e',
    AS.exprAnnot = ann,
    AS.exprLoc   = AS.exprLoc expr
} where
    ann = ExprAnn {
            exprType     = exprType,
            exprVal      = exprVal
        }
    (exprType, exprVal) = case AS.val expr of
        AS.IntLit v -> (T.TInt, E.SVInt v)
        AS.BoolLit v -> (T.TBool, E.CTVBool v)
        AS.CharLit v -> (T.TChar, E.CTVChar v)
        AS.StringLit v -> (T.TString, E.CTVString v)