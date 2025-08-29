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
annExp :: Env -> Exp -> AnnExp
annExp env exp@AS.Literal{} = annExpLit env exp
annExp env exp@AS.ExpIdent{} = annExpIdent env exp
annExp env exp@AS.FunCall{} = annExpRoutCall env exp
annExp env exp@AS.Sign{} = annExpSign env exp
annExp env exp@AS.BinOp{} = annExpBinOp env exp
annExp env exp@AS.BoolNeg{} = annExpNot env exp
annExp env exp@AS.RelOp{} = annExpRelOp env exp
annExp env exp@AS.Ref{} = annExpRef env exp
annExp env exp@AS.Deref{} = annExpDeref env exp
annExp env exp@AS.ArrayIndex {} = annExpArrayIndex env exp
annExp env exp@AS.Array {} = annExpArray env exp
annExp _ AS.Cast {} = error "Unreachable: `annExp` called with `Cast` expression, which can only be create by the Semantic Analyzer itself."

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
        err@T.TError {} -> AS.Cast { AS.e = exp, AS.expAnnot = ExpAnn { expType = err, ctVal = Nothing }, AS.expPos = pos }
        ty | ty == newType -> case (exp, valCast) of
            (AS.Cast {}, _) -> exp { AS.expAnnot = castAnn' } -- Simplify cast chains.
            _ -> AS.Cast { AS.e = exp, AS.expAnnot = castAnn', AS.expPos = pos }
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
        ctVal   = Just ctVal
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
            declPos = Nothing
        }
        -- Var identifier.
        (Just (Var varPos ty mode), Nothing) -> ExpIdentAnn {
            expType = T.type_ ty,
            ctVal = Nothing,
            identCategory = VarCategory,
            mode = mode,
            declPos = Just varPos
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
                declPos = Just constPos
            }
            ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
            expectedLExpErr = E.err pos "Left-hand side expression required, but found constant."
        -- Routine identifier.
        (Nothing, Just _) -> ann
            where (ann, _) = annRoutineCall env ident [] pos
        -- Var identifier (the ambiguity is resolved because function calls cannot be
        -- LExp).
        (Just (Var varPos ty mode), _) | mustBeLExp env -> ExpIdentAnn {
            expType = T.type_ ty,
            ctVal = Nothing,
            identCategory = VarCategory,
            mode = mode,
            declPos = Just varPos
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
                declPos = Just constPos
            }
            ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
            expectedLExpErr = E.err pos "Left-hand side expression required, but found constant."
        -- Ambiguous case.
        _ -> ExpIdentAnn {
            expType = T.TError { T.msgs = [E.err pos "Ambiguous identifier: " ++ PP.identPrinter ident] },
            ctVal = Nothing,
            identCategory = ErrorCategory,
            mode = Nothing,
            declPos = Nothing
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
            }, Just params, map (annExp env) args, Just declPos, thisThrows)
        Just (Proc declPos params thisThrows) -> (T.TError {
            T.msgs = [E.err pos "Cannot call a procedure as an expression: " ++ PP.identPrinter ident ++ "."]
        }, Just params, annAndCastRoutArgs env args params, Just declPos, thisThrows)
        Nothing -> (T.TError {
            T.msgs = [E.err pos "Unbound routine: " ++ PP.identPrinter ident ++ "."]
        }, Nothing, map (annExp (doNotRequireLExp env)) args, Nothing, Never)
    ann = ExpFunCallAnn {
        expType = routType,
        params = params,
        declPos = declPos,
        throws = thisThrows
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
            AS.Mval -> cast (doNotRequireLExp env) (annExp (doNotRequireLExp env) arg) paramType
            -- Arguments passed by value-result and reference must be LExp and must be non-constant.
            _       -> cast (requireLExp env) (annExp (requireLExp env) arg) paramType
annAndCastRoutArgs _ [] [] = []
annAndCastRoutArgs _ _ _ = error "Unreachable: `annRoutArgs` called with different number of parameters and arguments."

-- Sign expressions.
-- Rule: RExpSign.
annExpSign :: Env -> Exp -> AnnExp
annExpSign env AS.Sign {
    AS.e = e,
    AS.expPos = pos
} = AS.Sign {
    AS.e = annE,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    annE = annExp (doNotRequireLExp env) e
    intCastedAnnE = cast (doNotRequireLExp env) annE T.TInteger
    realCastedAnnE = cast (doNotRequireLExp env) annE T.TReal
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
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
    expectedLExpErr = E.err pos "Sign is forbidden in left-hand side expressions."
annExpSign _ _ = error "Unreachable: `annExpSign` called with non-`Sign` expression."

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
        ctVal = newVal
    }
    -- Call recursive annotation for both expressions.
    annE1 = annExp (doNotRequireLExp env) e1
    annE2 = annExp (doNotRequireLExp env) e2
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
    annE = annExp (doNotRequireLExp env) e
    -- Cast the expression to the expected type (TBoolean).
    castedE = cast (doNotRequireLExp env) annE T.TBoolean
    castedTypeE = expType $ AS.expAnnot castedE
    castedValE = getCTValue $ AS.expAnnot castedE
    -- Create annotation.
    ann = ExpAnn {
        expType = castedTypeE,
        ctVal = case castedValE of
            Just (CTVBoolean v) -> Just (CTVBoolean (not v))
            _ -> Nothing
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
        ctVal = newVal
    }
    -- Call recursive annotation for both expressions.
    annE1 = annExp (doNotRequireLExp env) e1
    annE2 = annExp (doNotRequireLExp env) e2
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
annExpRef :: Env -> Exp -> AnnExp
annExpRef env AS.Ref {
    AS.e = e,
    AS.expPos = pos
} = AS.Ref {
    AS.e = annE,
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    annE = annExp (requireLExp env) e -- Only LExp can be referenced.
    ann = ExpAnn {
        expType = case refType of
            e@T.TError {} -> e
            _ -> T.TReference refType,
        ctVal = Nothing
    }
    refType = expType $ AS.expAnnot annE
    expectedLExpErr = E.err pos "Reference is forbidden in left-hand side expressions."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpRef _ _ = error "Unreachable: `annExpRef` called with non-`Ref` expression."

-- Dereference expressions.
-- Rule: RExpDeref.
annExpDeref :: Env -> Exp -> AnnExp
annExpDeref env AS.Deref {
    AS.e = e,
    AS.expPos = pos
} = AS.Deref {
    AS.e = annE,
    AS.expAnnot = ann,
    AS.expPos = pos
} where
    annE = annExp (requireLExp env) e -- Only LExp can be dereferenced.
    ann = ExpAnn {
        expType = case expType $ AS.expAnnot annE of
            T.TReference ty -> ty
            e@T.TError {} -> e
            _ -> T.TError {
                T.msgs = [E.err pos "Cannot dereference expression of type " ++ show (expType $ AS.expAnnot annE) ++ "."]
            },
        ctVal = Nothing
    }
annExpDeref _ _ = error "Unreachable: `annExpDeref` called with non-`Deref` expression."

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
    annE = annExp (requireLExp env) e
    annIndex = annExp (doNotRequireLExp env) index
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
        ctVal = Nothing -- Constants cannot be arrays.
    }
annExpArrayIndex _ _ = error "Unreachable: `annExpArrayIndex` called with non-`ArrayIndex` expression."

-- Array expressions.
-- Rule: RExpArray.
annExpArray :: Env -> Exp -> AnnExp
annExpArray env AS.Array {
    AS.elements = elements,
    AS.expPos = pos
} = AS.Array {
    AS.elements = case supType of
        T.TError {} -> annElements -- If the super type is an error, keep the original annotations without casting.
        _ -> castedElements, -- Otherwise, cast all elements to the super type.
    AS.expAnnot = ann',
    AS.expPos = pos
} where
    annElements = map (annExp (doNotRequireLExp env)) elements
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
        ctVal = Nothing -- Constants cannot be arrays.
    }
    expectedLExpErr = E.err pos "Left-hand side expression required, but found array."
    ann' = if mustBeLExp env then addErr ann expectedLExpErr else ann
annExpArray _ _ = error "Unreachable: `annExpArray` called with non-`Array` expression."