-- Implementation of the annotation rules for EXPRESSIONS.
module StatSem.Exp where

import StatSem.Utils
import qualified Data.Map.Lazy  as Map
import qualified AbsSyn         as AS
import qualified Errors         as E
import qualified Types          as T
import qualified LexGrammar     as LG
import qualified PrettyPrinter  as PP

-- TYPES
-- Void annotations after parsing.
type Exp = AS.Exp ()
-- Annotated expressions after static semantics analysis.
type AnnExp = AS.Exp Ann

-- Annotate an expression node.
annExp :: Maybe T.Type -> Env -> Exp -> AnnExp
annExp _ env exp@AS.Literal{} = annExpLit env exp
annExp _ env exp@AS.ExpIdent{} = annExpIdent env exp
annExp _ env exp@AS.FunCall{} = annExpRoutCall env exp
annExp expectedType env exp@AS.Sign{} = annExpSign expectedType env exp
annExp _ env exp@AS.BinOp{} = annExpBinOp env exp
annExp _ env exp@AS.BoolNeg{} = annExpNot env exp
annExp _ env exp@AS.RelOp{} = annExpRelOp env exp
annExp expectedType env exp@AS.Ref{} = annExpRef expectedType env exp
annExp expectedType env exp@AS.Deref{} = annExpDeref expectedType env exp
annExp _ env exp@AS.ArrayIndex {} = annExpArrayIndex env exp
annExp expectedType env exp@AS.Array {} = annExpArray expectedType env exp
annExp expectedType env exp@AS.ArrayExpr {} = annExpArrayExpr expectedType env exp
annExp expectedType env exp@AS.IfElse {} = annExpIfElse expectedType env exp
annExp _ _ AS.Cast {} = error "Unreachable: `annExp` called with `Cast` expression, which can only be create by the Semantic Analyzer itself."

-----------
-- UTILS --
-----------
-- Expression-specific utilities.

-- Get type and value from a literal expression.
getTypeAndValue :: AS.Literal -> (T.Type, CTValue)
getTypeAndValue (AS.LitInt v _) = (T.TInteger, CTVInteger v)
getTypeAndValue (AS.LitChar v _) = (T.TChar, CTVChar v)
getTypeAndValue (AS.LitString v _) = (T.TString, CTVString v)
getTypeAndValue (AS.LitBool v _) = (T.TBoolean, CTVBoolean v)
getTypeAndValue (AS.LitDouble v _) = (T.TReal, CTVReal v)

-- Cast an annotated expression to a specific type.
cast :: Env -> AnnExp -> T.Type -> AnnExp
cast env exp newType = case expType ann of
    T.TError {} -> exp
    oldType | oldType == newType -> exp
    oldType -> case T.sup pos oldType newType of
        err@T.TError {} -> AS.Cast { AS.e = exp, AS.expAnnot = ExpAnn { expType = err, ctVal = Nothing, readOnly = False }, AS.expPos = pos }
        ty | ty == newType -> case (exp, valCast) of
            (AS.Cast {}, _) -> exp { AS.expAnnot = castAnn' } -- Simplify cast chains.
            _ -> AS.Cast { AS.e = exp, AS.expAnnot = castAnn', AS.expPos = pos }
        _ -> AS.Cast {
            AS.e = exp,
            AS.expAnnot = ExpAnn {
                expType = T.TError { T.msgs = [E.err pos $ "Cannot cast expression of type " ++ show oldType ++ " to " ++ show newType ++ "."] },
                ctVal = Nothing,
                readOnly = False
            },
            AS.expPos = pos }
    where
        pos = AS.expPos exp
        ann = AS.expAnnot exp
        valCast = ctCast (getCTValue ann) newType -- Compile-time cast.
        castAnn = ExpAnn {
            expType = newType,
            ctVal   = valCast,
            readOnly = True
        }
        castAnn' = if mustBeLExp env then addErr castAnn expectedLExpErr else castAnn
        expectedLExpErr = E.err pos "Implicit casting is forbidden in left-hand side expressions."

-----------
-- LEAFS --
-----------

-- Literal expressions.
-- Rules: RExpInt, RExpChar, RExpString, RExpBool, RExpDouble.
annExpLit :: Env -> Exp -> AnnExp
annExpLit env AS.Literal { AS.val = exp, AS.expPos = expPos } = AS.Literal {
    AS.val = exp,
    AS.expAnnot = ann',
    AS.expPos = expPos
} where
    ann = ExpAnn {
        expType = expType,
        ctVal   = Just ctVal,
        readOnly = False
    }
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
    (expType, ctVal) = getTypeAndValue exp
    expectedLExpErr = E.err expPos "Left-hand side expression required, but found literal."
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
        (Nothing, Nothing) -> ExpIdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Unbound identifier: " ++ PP.identPrinter ident ++ "."] },
            ctVal = Nothing,
            identCategory = ErrorCategory,
            mode = Nothing,
            declPos = Nothing,
            readOnly = False
        }
        -- Var identifier.
        (Just (Var varPos ty mode readOnly), Nothing) -> ExpIdentAnn {
            expType = T.type_ ty,
            ctVal = Nothing,
            identCategory = VarCategory,
            mode = mode,
            declPos = Just varPos,
            readOnly = readOnly
        }
        -- Const identifier.
        (Just (Const constPos val), Nothing) -> ann'
            where
            (identType, identVal) = getTypeAndValue val
            ann = ExpIdentAnn {
                expType = identType,
                ctVal = Just identVal,
                identCategory = ConstCategory,
                mode = Nothing,
                declPos = Just constPos,
                readOnly = False
            }
            ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
            expectedLExpErr = E.err pos "Left-hand side expression required, but found constant."
        -- Routine identifier.
        (Nothing, Just _) -> ann
            where (ann, _) = annRoutineCall env ident [] pos
        -- Var identifier (the ambiguity is resolved because function calls cannot be
        -- LExp).
        (Just (Var varPos ty mode readOnly), _) | mustBeLExp env -> ExpIdentAnn {
            expType = T.type_ ty,
            ctVal = Nothing,
            identCategory = VarCategory,
            mode = mode,
            declPos = Just varPos,
            readOnly = readOnly
        }
        -- Const identifier (the ambiguity is resolved because function calls cannot be
        -- LExp).
        (Just (Const constPos val), _) | mustBeLExp env -> ann'
            where
            (identType, identVal) = getTypeAndValue val
            ann = ExpIdentAnn {
                expType = identType,
                ctVal = Just identVal,
                identCategory = ConstCategory,
                mode = Nothing,
                declPos = Just constPos,
                readOnly = False
            }
            ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
            expectedLExpErr = E.err pos "Left-hand side expression required, but found constant."
        -- Ambiguous case.
        _ -> ExpIdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Ambiguous identifier: " ++ PP.identPrinter ident] },
            ctVal = Nothing,
            identCategory = ErrorCategory,
            mode = Nothing,
            declPos = Nothing,
            readOnly = False
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
    AS.expCall = rc { AS.args = castedArgs },
    AS.expAnnot = ann
} where
    (ann, castedArgs) = annRoutineCall env ident args pos
annExpRoutCall _ _ = error "Unreachable: `annExpRoutCall` called with non-`FunCall` expression."

-- Helper function that creates an annotation for a routine call, and casts the arguments
-- to the expected types.
annRoutineCall :: Env -> AS.Ident -> [Exp] -> LG.Posn -> (Ann, [AnnExp])
annRoutineCall env ident args pos = (ann', castedArgs) where
    (routType, params, castedArgs, declPos, thisThrows) = case Map.lookup ident (routines env) of
        Just (Fun declPos params ty thisThrows) -> case length params == length args of
            True  -> case T.getErrs $ map (expType . AS.expAnnot) castedArgs of
                [] -> (T.type_ ty, Just params, castedArgs, Just declPos, thisThrows)
                errs -> (T.TError { T.msgs = errs }, Just params, castedArgs, Just declPos, thisThrows)
                where
                    castedArgs = annAndCastRoutArgs env args params
            False -> (T.TError {
                T.msgs = [E.err pos "Wrong argument number in routine call: " ++ PP.identPrinter ident ++ ". Expected " ++ show (length params) ++ " but got " ++ show (length args) ++ "."]
            }, Just params, map (annExp Nothing env) args, Just declPos, thisThrows)
        Just (Proc declPos params thisThrows) -> (T.TError {
            T.msgs = [E.err pos "Cannot call a procedure as an expression: " ++ PP.identPrinter ident ++ "."]
        }, Just params, annAndCastRoutArgs env args params, Just declPos, thisThrows)
        Nothing -> (T.TError {
            T.msgs = [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."]
        }, Nothing, map (annExp Nothing (doNotRequireLExp env)) args, Nothing, Never)
    ann = ExpFunCallAnn {
        expType = routType,
        params = params,
        declPos = declPos,
        throws = thisThrows,
        readOnly = False
    }
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
    expectedLExpErr = E.err pos "Left-hand side expression required, but found routine call."

-- Helper function that annotates routine call arguments accordingly to their passing
-- modality and try to cast the actual parameters (arguments) to the formal parameters
-- types.
--
-- Preconditions:
-- - The number of actual parameters must match the number of formal parameters.
-- - The `Param`s must have a valid type (not `TError`).
annAndCastRoutArgs :: Env -> [Exp] -> [AS.Param] -> [AnnExp]
annAndCastRoutArgs _ _ (AS.Param { AS.paramType = T.TypePos { T.type_ = T.TError {} } } : _) =
    error "Unreachable: `castParamTypes` called with an error type in the parameter list." -- Unreachable if Param comes from the parser.
annAndCastRoutArgs env (arg : args) (AS.Param { AS.mode = mode, AS.paramType = T.TypePos { T.type_ = paramType } } : params) =
    castedArg : annAndCastRoutArgs env args params
    where
        castedArg = case mode of
            -- Arguments passed by value do not need to be LExp.
            AS.Mval -> cast (doNotRequireLExp env) (annExp (Just paramType) (doNotRequireLExp env) arg) paramType
            AS.Mconst -> cast (doNotRequireLExp env) (annExp (Just paramType) (doNotRequireLExp env) arg) paramType
            -- Arguments passed by value-result and reference must be LExp and must be non-constant.
            _       -> cast (requireLExp env) (annExp (Just paramType) (requireLExp env) arg) paramType
annAndCastRoutArgs _ [] [] = []
annAndCastRoutArgs _ _ _ = error "Unreachable: `annRoutArgs` called with different number of parameters and arguments."

-- Sign expressions.
-- Rule: RExpSign.
annExpSign :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpSign expectedType env AS.Sign {
    AS.e = e,
    AS.expPos = pos
} = AS.Sign {
    AS.e = annE,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    annE = annExp expectedType (doNotRequireLExp env) e
    intCastedAnnE = cast (doNotRequireLExp env) annE T.TInteger
    realCastedAnnE = cast (doNotRequireLExp env) annE T.TReal
    ann = case (expType $ AS.expAnnot intCastedAnnE, expType $ AS.expAnnot realCastedAnnE) of
        (T.TInteger, _) -> ExpAnn {
            expType = T.TInteger,
            ctVal = case (getCTValue $ AS.expAnnot annE) of
                Just (CTVInteger v) -> Just (CTVInteger (-v))
                Nothing             -> Nothing
                ctv                 -> error $ "Type unconsistency found: exp annotated as Integer but has " ++ show ctv ++ ".", -- Should be unreachable.
                readOnly = False
        }
        (_, T.TReal) -> ExpAnn {
            expType = T.TReal,
            ctVal = case (getCTValue $ AS.expAnnot annE) of
                Just (CTVReal v) -> Just (CTVReal (-v))
                Nothing          -> Nothing
                ctv              -> error $ "Type unconsistency found: exp annotated as Real but has " ++ show ctv ++ ".", -- Should be unreachable.
                readOnly = False
        }
        _ -> ExpAnn {
            expType = T.TError {
                T.msgs = [E.err pos "Cannot apply sign to expression of type `" ++ show (expType $ AS.expAnnot annE) ++ "`."]
            },
            ctVal = Nothing,
            readOnly = False
        }
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
    expectedLExpErr = E.err pos "Sign is forbidden in left-hand side expressions."
annExpSign _ _ _ = error "Unreachable: `annExpSign` called with non-`Sign` expression."

-- Binary Operation expressions.
-- Rules: RExpOr, RExpAnd, RExpPlus, RExpMinus, RExpMult, RExpDivReal, RExpDivInt,
-- RExpMod, RExpPow.
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
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    ann = ExpAnn {
        expType = newType,
        ctVal = newVal,
        readOnly = False
    }
    -- Call recursive annotation for both expressions.
    annE1 = annExp Nothing (doNotRequireLExp env) e1 -- `Nothing` because possibly multiple types are expected
    annE2 = annExp Nothing (doNotRequireLExp env) e2 -- `Nothing` because possibly multiple types are expected
    -- Get types.
    typeE1 = expType $ AS.expAnnot annE1
    typeE2 = expType $ AS.expAnnot annE2
    -- Get expected types.
    expectedTypes = AS.typeOfBinOpArgs binOp
    -- Cast both expressions to the expected types (try for each type).
    castedsE1 = map (cast (doNotRequireLExp env) annE1) expectedTypes
    castedsE2 = map (cast (doNotRequireLExp env) annE2) expectedTypes
    castedTypesE1 = map (expType . AS.expAnnot) castedsE1
    castedTypesE2 = map (expType . AS.expAnnot) castedsE2
    castedValsE1 = map (getCTValue . AS.expAnnot) castedsE1
    castedValsE2 = map (getCTValue . AS.expAnnot) castedsE2
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
        (T.TError {}, _, _) -> getTypeAndVal ats vs1 vs2 cs1 cs2
        (_, v1, v2) -> (at, applyBinOpSC binOp v1 v2, c1, c2)
    getTypeAndVal _ _ _ _ _ = error "Unreachable: `getTypeAndVal` called with lists of different lengths."
    (newType, newVal, castedE1, castedE2) = case (typeE1, typeE2) of
        (T.TError { T.msgs = msgs1 }, T.TError { T.msgs = msgs2 }) -> (
            T.TError { T.msgs = msgs1 ++ msgs2 },
            Nothing,
            annE1,
            annE2)
        (e@T.TError {}, _) -> (e, Nothing, annE1, annE2)
        (_, e@T.TError {}) -> (e, Nothing, annE1, annE2)
        _ -> getTypeAndVal castedTypes castedValsE1 castedValsE2 castedsE1 castedsE2
    expectedLExpErr = E.err pos "Left-hand side expression required, but found binary operation."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpBinOp _ _ = error "Unreachable: `annExpBinOp` called with non-`BinOp` expression."

-- Bool NOT expressions.
-- Rule: RExpNeg.
annExpNot :: Env -> Exp -> AnnExp
annExpNot env AS.BoolNeg {
    AS.e = e,
    AS.expPos = pos
} = AS.BoolNeg {
    AS.e = castedE,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Call recursive annotation.
    annE = annExp (Just T.TBoolean) (doNotRequireLExp env) e
    -- Cast the expression to the expected type (TBoolean).
    castedE = cast (doNotRequireLExp env) annE T.TBoolean
    castedTypeE = expType $ AS.expAnnot castedE
    castedValE = getCTValue $ AS.expAnnot castedE
    -- Create annotation.
    ann = ExpAnn {
        expType = castedTypeE,
        ctVal = case castedValE of
            Just (CTVBoolean v) -> Just (CTVBoolean (not v))
            _ -> Nothing,
        readOnly = False
    }
    expectedLExpErr = E.err pos "Left-hand side expression required, but found not operation."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpNot _ _ = error "Unreachable: `annExpNot` called with non-`BoolNeg` expression."

-- Relational Operation expressions.
-- Rules: RExpGt, RExpLs, RExpGe, RExpLe, RExpEq, RExpNe.
annExpRelOp :: Env -> Exp -> AnnExp
annExpRelOp env AS.RelOp {
    AS.relOp = relOp,
    AS.e1 = e1,
    AS.e2 = e2,
    AS.expPos = pos
} = AS.RelOp {
    AS.relOp = relOp,
    AS.e1 = castedE1,
    AS.e2 = castedE2,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    ann = ExpAnn {
        expType = newType,
        ctVal = newVal,
        readOnly = False
    }
    -- Call recursive annotation for both expressions.
    annE1 = annExp Nothing (doNotRequireLExp env) e1 -- `Nothing` because any type is acceptable
    annE2 = annExp Nothing (doNotRequireLExp env) e2 -- `Nothing` because any type is acceptable
    -- Get types.
    typeE1 = expType $ AS.expAnnot annE1
    typeE2 = expType $ AS.expAnnot annE2
    -- Get super type.
    ty = T.sup pos typeE1 typeE2
    -- Cast both expressions to the sup type.
    castedE1 = cast (doNotRequireLExp env) annE1 ty
    castedE2 = cast (doNotRequireLExp env) annE2 ty
    castedTypeE1 = expType $ AS.expAnnot castedE1
    castedTypeE2 = expType $ AS.expAnnot castedE2
    castedValE1 = getCTValue $ AS.expAnnot castedE1
    castedValE2 = getCTValue $ AS.expAnnot castedE2
    newType = case T.sup pos castedTypeE1 castedTypeE2 of
        e@T.TError {} -> e
        _ -> T.TBoolean -- Relational operations always return a boolean value.
    -- If both expressions are compile-time values, apply the relational operation.
    newVal = case (castedValE1, castedValE2) of
        (Just v1, Just v2) -> Just (CTVBoolean (applyRelOp relOp v1 v2))
        _ -> Nothing
    expectedLExpErr = E.err pos "Left-hand side expression required, but found relational operation."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpRelOp _ _ = error "Unreachable: `annExpRelOp` called with non-`RelOp` expression."

-- Reference expressions.
-- Rule: RExpRef.
annExpRef :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpRef expectedType env AS.Ref {
    AS.e = e,
    AS.expPos = pos
} = AS.Ref {
    AS.e = annE,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Propagate the expected type.
    innerExpectedType = case expectedType of
        Just T.TReference { T.refType = ty } -> Just ty
        _ -> Nothing -- No error because it will be returned when a leaf with a wrong
                           -- type is encountered.
    annE = annExp innerExpectedType (requireLExp env) e -- Only LExp can be referenced.
    ann = ExpAnn {
        expType = case refType of
            e@T.TError {} -> e
            _ -> T.TReference refType,
        ctVal = Nothing,
        readOnly = False
    }
    refType = expType $ AS.expAnnot annE
    expectedLExpErr = E.err pos "Reference is forbidden in left-hand side expressions."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpRef _ _ _ = error "Unreachable: `annExpRef` called with non-`Ref` expression."

-- Dereference expressions.
-- Rule: RExpDeref.
annExpDeref :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpDeref expectedType env AS.Deref {
    AS.e = e,
    AS.expPos = pos
} = AS.Deref {
    AS.e = annE,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    -- Propagate the expected type.
    innerExpectedType = case expectedType of
        Just ty -> Just T.TReference { T.refType = ty }
        Nothing -> Nothing
    annE = annExp innerExpectedType (requireLExp env) e -- Only LExp can be dereferenced.
    ann = ExpAnn {
        expType = case expType $ AS.expAnnot annE of
            T.TReference ty -> ty
            e@T.TError {} -> e
            _ -> T.TError {
                T.msgs = [E.err pos "Cannot dereference expression of type " ++ show (expType $ AS.expAnnot annE) ++ "."]
            },
        ctVal = Nothing,
        -- Dereferencing a read-only reference yields a read-only value.
        readOnly = readOnly $ AS.expAnnot annE
    }
annExpDeref _ _ _ = error "Unreachable: `annExpDeref` called with non-`Deref` expression."

-- Array dereference expressions. (e[index])
-- Rule: LExpArrayDeref.
annExpArrayIndex :: Env -> Exp -> AnnExp
annExpArrayIndex env AS.ArrayIndex {
    AS.e = e,
    AS.index = index,
    AS.expPos = pos
} = AS.ArrayIndex {
    AS.e = annE,
    AS.index = castedIndex,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    annE = annExp Nothing (requireLExp env) e -- The expected type cannot be propagated because the size of the array is not known.
    annIndex = annExp (Just T.TInteger) (doNotRequireLExp env) index
    castedIndex = cast (doNotRequireLExp env) annIndex T.TInteger
    typeE = expType $ AS.expAnnot annE
    ty = case typeE of
        T.TArray { T.elementsType = ty } -> ty
        T.TError {} -> typeE
        _ -> T.TError {
            T.msgs = [E.err pos "Cannot index expression of type " ++ show typeE ++ "."]
        }
    ann = ExpAnn {
        expType = ty,
        ctVal = Nothing, -- Constants cannot be arrays.
        -- Dereferencing a read-only reference yields a read-only value.
        readOnly = readOnly $ AS.expAnnot annE
    }
annExpArrayIndex _ _ = error "Unreachable: `annExpArrayIndex` called with non-`ArrayIndex` expression."

-- Array.
-- Rule: RExpArray.
annExpArray :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpArray expectedType env AS.Array {
    AS.elements = elements,
    AS.expPos = pos
} = AS.Array {
    AS.elements = case supType of
        T.TError {} -> annElements -- If the super type is an error, keep the original annotations without casting.
        _ -> castedElements, -- Otherwise, cast all elements to the super type.
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Propagate the expected type.
    innerExpectedType = case expectedType of
        Just T.TArray { T.elementsType = ty } -> Just ty
        _ -> Nothing -- No error because it will be returned when a leaf with a wrong type is encountered.
    annElements = map (annExp innerExpectedType (doNotRequireLExp env)) elements
    -- Get the type of all the elements.
    elementTypes = map (expType . AS.expAnnot) annElements
    -- Get the super type of all the elements.
    supType = case elementTypes of
        [] -> T.TError { T.msgs = [E.err pos "Cannot create an empty array."] }
        t:ts -> foldr (T.sup pos) t ts
    castedElements = map (\e -> cast (doNotRequireLExp env) e supType) annElements -- Cast all elements to the super type.
    -- Create the annotation.
    ann = ExpAnn {
        expType = case supType of
            T.TError {} -> supType
            _ -> T.TArray {
                T.elementsType = supType,
                T.size = length elements
            },
        ctVal = Nothing, -- Constants cannot be arrays.
        readOnly = False
    }
    expectedLExpErr = E.err pos "Left-hand side expression required, but found array."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpArray _ _ _ = error "Unreachable: `annExpArray` called with non-`Array` expression."

-- Array expressions.
-- Rule: RExpArrayExpr.
annExpArrayExpr :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpArrayExpr expectedType env AS.ArrayExpr {
    AS.e = e,
    AS.indexes = [],
    AS.expPos = pos
} = AS.ArrayExpr {
    AS.e = annE,
    AS.indexes = [],
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    -- Propagate the expected type.
    innerExpectedType = case expectedType of
        Just T.TArray { T.elementsType = ty } -> Just ty
        _ -> Nothing -- No error because it will be returned when a leaf with a wrong type is encountered.
    -- Annotate e.
    annE = annExp innerExpectedType (doNotRequireLExp env) e
    -- Get the e errs.
    eErrs = T.getErrs $ [expType (AS.expAnnot annE)]
    -- Annotation.
    ann = ExpAnn {
        expType = T.TError { T.msgs = eErrs ++ [E.err pos "Array expression must have at least one index."] },
        ctVal = Nothing,
        readOnly = False
    }
annExpArrayExpr Nothing env AS.ArrayExpr {
    AS.e = e,
    AS.indexes = indexes,
    AS.expPos = pos
} = AS.ArrayExpr {
    AS.e = annE,
    AS.indexes = indexes,
    AS.expAnnot = ann,
    AS.expPos = pos
 } where
    -- Annotate e.
    annE = annExp Nothing (doNotRequireLExp env) e
    -- Annotation.
    ann = ExpAnn {
        -- Errors from annE are not propagated because potentially contain misleading errors (like unbounded indexes variables).
        expType = T.TError { T.msgs = [E.err pos "Array expression type cannot be inferred. Suggestion: create a constant or a variable and specify the type."] },
        ctVal = Nothing,
        readOnly = False
    }
annExpArrayExpr (Just T.TArray { T.elementsType = ty, T.size = s }) env AS.ArrayExpr {
    AS.e = e,
    AS.indexes = [index],
    AS.expPos = pos
} = AS.Array {
    AS.elements = castedElements,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Bind the index.
    bindings = map (\x -> [(index, Const LG.defaultPosn (AS.LitInt x LG.defaultPosn))]) [1..s]
    bindingEnvs = map envFromSymbolBindings bindings
    envs = map (fst . mergeEnv env) bindingEnvs -- The error can be ignored because there are no new routine bindings.
    -- Annotate e.
    annEs = map (\env -> (annExp (Just ty) (doNotRequireLExp env) e)) envs
    -- Cast to the expected type.
    castedElements = map (\e -> cast (doNotRequireLExp env) e ty) annEs -- Cast all elements to the super type.
    -- Get errors.
    castedEErrs = T.getErrs $ map (expType . AS.expAnnot) castedElements
    -- Create the annotation.
    ann = ExpAnn {
        expType = case castedEErrs of
            [] -> T.TArray {
                T.elementsType = ty,
                T.size = s
            }
            e:_ -> T.TError { T.msgs = [e] }, -- Take only the first error since they are all the same
        ctVal = Nothing, -- Constants cannot be arrays.
        readOnly = False
    }
    expectedLExpErr = E.err pos "Left-hand side expression required, but found array."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpArrayExpr (Just T.TArray { T.elementsType = ty, T.size = s }) env AS.ArrayExpr {
    AS.e = e,
    AS.indexes = (index:indexes),
    AS.expPos = pos
} = AS.Array {
    AS.elements = castedElements,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Bind the index.
    bindings = map (\x -> [(index, Const LG.defaultPosn (AS.LitInt x LG.defaultPosn))]) [1..s]
    bindingEnvs = map envFromSymbolBindings bindings
    envs = map (fst . mergeEnv env) bindingEnvs -- The error can be ignored because there are no new routine bindings.
    -- Create an ArrayExpr for each environment.
    eArrayExprs = map (\env -> annExpArrayExpr (Just ty) env AS.ArrayExpr {
        AS.e = e,
        AS.indexes = indexes,
        AS.expAnnot = (),
        AS.expPos = pos
    }) envs
    -- Cast to the expected type.
    castedElements = map (\e -> cast (doNotRequireLExp env) e ty) eArrayExprs -- Cast all elements to the super type.
    -- Get errors.
    castedEErrs = T.getErrs $ map (expType . AS.expAnnot) castedElements
    -- Create the annotation.
    ann = ExpAnn {
        expType = case castedEErrs of
            [] -> T.TArray {
                T.elementsType = ty,
                T.size = s
            }
            e:_ -> T.TError { T.msgs = [e] }, -- Take only the first error since they are all the same
        ctVal = Nothing, -- Constants cannot be arrays.
        readOnly = False
    }
    expectedLExpErr = E.err pos "Left-hand side expression required, but found array."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpArrayExpr (Just _) env AS.ArrayExpr {
    AS.e = e,
    AS.indexes = indexes,
    AS.expPos = pos
} = AS.ArrayExpr {
    AS.e = annE,
    AS.indexes = indexes,
    AS.expAnnot = ann,
    AS.expPos = pos
 } where
    -- Annotate e.
    annE = annExp Nothing (doNotRequireLExp env) e
    -- Get the e errs.
    eErrs = T.getErrs $ [expType (AS.expAnnot annE)]
    -- Annotation.
    ann = ExpAnn {
        expType = T.TError { T.msgs = eErrs ++ [E.err pos "Array expression must have an array type, but has type " ++ show (expType (AS.expAnnot annE)) ++ "."] }, -- TODO: pretty print type in err
        ctVal = Nothing,
        readOnly = False
    }
annExpArrayExpr _ _ _ = error "Unreachable: `annExpArrayExpr` called with non-`ArrayExpr` expression."
-- TODO: array expression can now build non-constant arrays. It is right?

annExpIfElse :: Maybe T.Type -> Env -> Exp -> AnnExp
annExpIfElse expectedType env AS.IfElse {
    AS.condExp = cond,
    AS.thenExp = thenExp,
    AS.elseExp = elseExp,
    AS.expPos = pos
} = AS.IfElse {
    AS.condExp = castedCond,
    AS.thenExp = annThen,
    AS.elseExp = annElse,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    -- Annotate the condition.
    annCond = annExp (Just T.TBoolean) (doNotRequireLExp env) cond
    -- Cast the condition to a boolean type.
    castedCond = cast (doNotRequireLExp env) annCond T.TBoolean
    -- Get the errors from the condition.
    condErrs = T.getErrs [expType (AS.expAnnot castedCond)]
    -- Condition compile-time value.
    condCTVal = getCTValue $ AS.expAnnot castedCond
    -- Annotate the then and else branches.
    annThen = annExp expectedType (doNotRequireLExp env) thenExp
    annElse = annExp expectedType (doNotRequireLExp env) elseExp
    -- Get the errors from the branches.
    thenErrs = T.getErrs [expType (AS.expAnnot annThen)]
    elseErrs = T.getErrs [expType (AS.expAnnot annElse)]
    -- The type of the expression.
    branchesType = T.sup pos (expType $ AS.expAnnot annThen) (expType $ AS.expAnnot annElse)
    -- Compile-time value.
    ctVal = case condCTVal of
        Just (CTVBoolean True) -> getCTValue $ AS.expAnnot annThen
        Just (CTVBoolean False) -> getCTValue $ AS.expAnnot annElse
        _ -> Nothing
    ann = ExpAnn {
        expType = branchesType,
        ctVal = ctVal,
        readOnly = False
    }
    ann' = foldl addErr (foldl addErr (foldl addErr ann condErrs) thenErrs) elseErrs
annExpIfElse _ _ _ = error "Unreachable: `annExpIfElse` called with non-`IfElse` expression."