-- Utilities function for TAC
module TAC.Utilis where

import qualified AbsSyn         as AS
import qualified StatSem.Utils  as SS
import qualified Types          as T
import qualified LexGrammar     as Lex
import TAC.Def

-- Check if an expression contains a given identifier
containsIdent :: AS.Ident -> AS.Exp SS.Ann -> Bool
containsIdent targetIdent (AS.BoolNeg expr _ _) = containsIdent targetIdent expr
containsIdent targetIdent (AS.Sign expr _ _) = containsIdent targetIdent expr
containsIdent targetIdent (AS.BinOp _ leftExpr rightExpr _ _) = 
    containsIdent targetIdent leftExpr || containsIdent targetIdent rightExpr
containsIdent targetIdent (AS.RelOp _ leftExpr rightExpr _ _) = 
    containsIdent targetIdent leftExpr || containsIdent targetIdent rightExpr
containsIdent targetIdent (AS.Ref expr _ _) = containsIdent targetIdent expr
containsIdent targetIdent (AS.FunCall call _ _) = 
    any (containsIdent targetIdent) (AS.args call)
containsIdent targetIdent (AS.Deref expr _ _) = containsIdent targetIdent expr
containsIdent targetIdent (AS.ArrayIndex arrayExpr indexExpr _ _) = 
    containsIdent targetIdent arrayExpr || containsIdent targetIdent indexExpr
containsIdent targetIdent (AS.ExpIdent currentIdent _ _) = 
    AS.identName targetIdent == AS.identName currentIdent
containsIdent targetIdent (AS.Array elements _ _) = 
    any (containsIdent targetIdent) elements
containsIdent targetIdent (AS.Cast expr _ _) = containsIdent targetIdent expr
containsIdent targetIdent (AS.IfElse condExpr thenExpr elseExpr _ _) = 
    containsIdent targetIdent condExpr || 
    containsIdent targetIdent thenExpr || 
    containsIdent targetIdent elseExpr
containsIdent _ _ = False

-- Replace all occurrences of one identifier with another in an expression
replaceIdent :: AS.Ident -> AS.Ident -> AS.Exp SS.Ann -> AS.Exp SS.Ann
replaceIdent oldIdent newIdent (AS.BoolNeg expr annotation position) = 
    AS.BoolNeg (replaceIdent oldIdent newIdent expr) annotation position
replaceIdent oldIdent newIdent (AS.Sign expr annotation position) = 
    AS.Sign (replaceIdent oldIdent newIdent expr) annotation position
replaceIdent oldIdent newIdent (AS.BinOp operator leftExpr rightExpr annotation position) = 
    AS.BinOp operator 
             (replaceIdent oldIdent newIdent leftExpr) 
             (replaceIdent oldIdent newIdent rightExpr) 
             annotation position
replaceIdent oldIdent newIdent (AS.RelOp operator leftExpr rightExpr annotation position) = 
    AS.RelOp operator 
             (replaceIdent oldIdent newIdent leftExpr) 
             (replaceIdent oldIdent newIdent rightExpr) 
             annotation position
replaceIdent oldIdent newIdent (AS.Ref expr annotation position) = 
    AS.Ref (replaceIdent oldIdent newIdent expr) annotation position
replaceIdent oldIdent newIdent (AS.FunCall call annotation position) = 
    AS.FunCall (call { AS.args = map (replaceIdent oldIdent newIdent) (AS.args call) }) 
               annotation position
replaceIdent oldIdent newIdent (AS.Deref expr annotation position) = 
    AS.Deref (replaceIdent oldIdent newIdent expr) annotation position
replaceIdent oldIdent newIdent (AS.ArrayIndex arrayExpr indexExpr annotation position) = 
    AS.ArrayIndex (replaceIdent oldIdent newIdent arrayExpr) 
                  (replaceIdent oldIdent newIdent indexExpr) 
                  annotation position
replaceIdent oldIdent newIdent (AS.ExpIdent currentIdent annotation position)
    | AS.identName oldIdent == AS.identName currentIdent = 
        AS.ExpIdent newIdent annotation position
    | otherwise = AS.ExpIdent currentIdent annotation position
replaceIdent oldIdent newIdent (AS.Array elements annotation position) = 
    AS.Array (map (replaceIdent oldIdent newIdent) elements) annotation position
replaceIdent oldIdent newIdent (AS.Cast expr annotation position) = 
    AS.Cast (replaceIdent oldIdent newIdent expr) annotation position
replaceIdent oldIdent newIdent (AS.IfElse condExpr thenExpr elseExpr annotation position) = 
    AS.IfElse (replaceIdent oldIdent newIdent condExpr) 
              (replaceIdent oldIdent newIdent thenExpr) 
              (replaceIdent oldIdent newIdent elseExpr) 
              annotation position
-- Base case: expression doesn't contain identifiers that can be replaced
replaceIdent _ _ expression = expression

relName :: AS.RelOp -> String
relName relOp = case relOp of
    AS.Eq -> "Eq"
    AS.Neq -> "Neq"
    AS.Lt -> "Lt"
    AS.Le -> "Le"
    AS.Gt -> "Gt"
    AS.Ge -> "Ge"

stringRelName :: AS.RelOp -> String
stringRelName relOp = "string" ++ relName relOp ++ "'"

-- Names for string comparison functions and the respective parameters
stringRelNames :: AS.RelOp -> (Label, Address, Address)
stringRelNames relOp = let funName = stringRelName relOp in (
        FunctionUnique funName,
        PName $ ProgramName (funName ++ "val1") Lex.defaultPosn,
        PName $ ProgramName (funName ++ "val2") Lex.defaultPosn
    )

arrayRelName :: AS.RelOp -> T.Type -> String
arrayRelName relOp ty = "array" ++ relName relOp ++ show ty ++ "'"

-- Names for array comparison functions and the respective parameters
arrayRelNames :: AS.RelOp -> T.Type -> (Label, Address, Address, Address)
arrayRelNames relOp ty = let funName = arrayRelName relOp ty in (
        FunctionUnique funName,
        PName $ ProgramName (funName ++ "val1") Lex.defaultPosn,
        PName $ ProgramName (funName ++ "val2") Lex.defaultPosn,
        PName $ ProgramName (funName ++ "Size") Lex.defaultPosn
    )

-- Find all identifiers in a left expression
findIdentLExpr :: AS.Exp SS.Ann -> [AS.Ident]
findIdentLExpr exp = case exp of
    AS.ExpIdent name _ _ -> [name]
    AS.Deref expr _ _ -> findIdentLExpr expr
    AS.ArrayIndex expr idx _ _ -> findIdentLExpr expr ++ findIdentLExpr idx
    _ -> error "genLExpr: Invalid left-hand side expression (parser should prevent this)"