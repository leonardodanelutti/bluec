module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified StatSem.Utils             as SS
import qualified Types                     as T
import qualified LexGrammar                as Lex
import qualified TAC.Printer()

import TAC.Def
import TAC.State

import Control.Monad (when, unless, foldM, void)
import Data.Maybe

-- Entrypoint for TAC generation
-- Generate the TAC code form the state
genProgram :: AS.Declarations SS.Ann -> TAC
genProgram ast =
    let finalState = St.execState (genEntryPoint ast) initState
        -- Reverse the stream to have the instructions in the correct order
    in TAC (reverse $ stream finalState) (reverse $ staticStream finalState)


-- TAC GENERATION FUNCTIONS

genEntryPoint :: AS.Declarations SS.Ann -> StateMonad ()
genEntryPoint (AS.ProgramDecl _ vars routines _) = do
    newRoutine
    attachToNext $ FunctionUnique "EntryPoint"
    mapM_ genVar vars
    -- Program calls main and then ends
    out (ProcCall (FunctionUnique "main") 0)
    out (Custom "END_PROGRAM")
    endRoutine
    mapM_ genRoutine routines

genDeclarations :: AS.Declarations SS.Ann -> StateMonad ()
genDeclarations (AS.ProgramDecl _ vars routines _) = do
    mapM_ genVar vars
    mapM_ genRoutine routines

genVar :: AS.VarA SS.Ann -> StateMonad ()
genVar (AS.VarAss idents _ values _ pos) =
    -- Evaluate the first left expression, then the first right expression
    -- then the seconds left expression and so on
    mapM_ genAssignment (zip idents values)
    where
    genAssignment :: (AS.Ident, AS.Exp SS.Ann) -> StateMonad ()
    genAssignment (ident, exp) = do
        let name = PName $ ProgramName (AS.identName ident) pos
        genExprAddr exp name

genExpr :: AS.Exp SS.Ann -> Maybe Address -> StateMonad Address
genExpr exp addrIn = do
    case AS.expAnnot exp of
        -- Handle expressions with known values
        SS.ExpAnn { SS.ctVal = Just val } -> genLit val
        SS.ExpIdentAnn { SS.ctVal = Just val } -> genLit val
        -- Handle expressions with no known values
        _ -> genUnknownExpr exp addrIn
    where
        genLit :: SS.CTValue -> StateMonad Address
        genLit (SS.CTVInteger v) = returnAddr addrIn (Lit (LitInt  v)) TInteger
        genLit (SS.CTVReal v)    = returnAddr addrIn (Lit (LitReal v)) TReal
        genLit (SS.CTVChar v)    = returnAddr addrIn (Lit (LitChar v)) TChar
        genLit (SS.CTVBoolean v) = returnAddr addrIn (Lit (LitBool v)) TBoolean
        -- Put strings in static data
        genLit (SS.CTVString v)  = do
            stringAddr <- outStatic (STVString (v ++ "\0"))
            returnAddr addrIn stringAddr Addr

genUnknownExpr :: AS.Exp SS.Ann -> Maybe Address -> StateMonad Address
genUnknownExpr exp addrIn = do
    case exp of
        AS.BoolNeg e _ _ -> do
            addrOut <- getOutAddr addrIn
            inner <- genExprTmp e
            out (UOp addrOut BoolNeg inner TBoolean)
            return addrOut

        AS.Sign e ann _ -> do
            addrOut <- getOutAddr addrIn
            inner <- genExprTmp e
            let ty = castTypeToTACType $ SS.expType ann
            out (UOp addrOut (Sign ty) inner ty)
            return addrOut

        AS.BinOp AS.Or e1 e2 _ _ -> do
            addrOut <- getOutAddr addrIn
            label <- genBoolExp AS.Or addrOut e1 e2
            attachToNext label
            return addrOut

        AS.BinOp AS.And e1 e2 _ _ -> do
            addrOut <- getOutAddr addrIn
            label <- genBoolExp AS.And addrOut e1 e2
            attachToNext label
            return addrOut

        AS.BinOp binOp e1 e2 ann _ -> do
            addrOut <- getOutAddr addrIn
            -- Left to right evaluation
            addr1 <- genExprTmp e1
            addr2 <- genExprTmp e2
            let tacOp = castBinOpToTACOp binOp (SS.expType ann)
                ty = castTypeToTACType $ SS.expType ann
            out (BOp addrOut tacOp addr1 addr2 ty)
            return addrOut

        AS.RelOp relOp e1 e2 _ _ -> do
            addrOut <- getOutAddr addrIn
            addr1 <- genExprTmp e1
            addr2 <- genExprTmp e2
            let ty = SS.expType $ AS.expAnnot e1
            genRepOp addrOut relOp addr1 addr2 ty
            return addrOut

        AS.Ref e _ _ -> do
            addrOut <- getOutAddr addrIn
            addr <- genExprTmp e
            out (Ref addrOut addr)
            return addrOut

        -- Already handled by genExpr
        AS.Literal {} -> error "genUnknownExpr: Literal expressions should be handled by genExpr"

        AS.FunCall call ann@(SS.ExpFunCallAnn {SS.expType = returnType}) _ -> do
            addrOut <- getOutAddr addrIn
            genFunCall call ann returnType addrOut

        AS.FunCall {} -> error "genUnknownExpr: FunCall without ExpFunCallAnn annotation is unreachable"

        AS.Deref e ann _ -> do
            addrOut <- getOutAddr addrIn
            addr <- genExprTmp e
            let ty = castTypeToTACType $ SS.expType ann
            out (DerefGet addrOut addr ty)
            return addrOut

        AS.ArrayIndex _ _ ann _ -> do
            addrOut <- getOutAddr addrIn
            genArrayIndex exp ann addrOut

        -- ConstCategory is handled by genExpr
        AS.ExpIdent ident (SS.ExpIdentAnn ty _ SS.VarCategory mode (Just declPos)) _ -> do
            let identName = PName $ ProgramName (AS.identName ident) declPos
                identName' = PName $ ProgramName (AS.identName ident ++ "'") declPos
                tyTAC = castTypeToTACType ty
            case mode of
                -- Identifiers that are passed by value-result are appended with the "'"
                Just AS.Mvalres -> returnAddr addrIn identName' tyTAC
                -- Identifiers that are passes by reference are dereferenced before being used
                Just AS.Mref -> do
                    addrOut <- getOutAddr addrIn
                    out (DerefGet addrOut identName tyTAC)
                    returnAddr addrIn addrOut tyTAC

                Just AS.Mval -> returnAddr addrIn identName tyTAC

                Nothing -> returnAddr addrIn identName tyTAC

        AS.ExpIdent ident ann@(SS.ExpFunCallAnn {SS.expType = returnType}) _ -> do
            addrOut <- getOutAddr addrIn
            genFunCall (AS.callFromIdent ident) ann returnType addrOut

        AS.ExpIdent {} -> error "genUnknownExpr: ExpIdent with unexpected annotation type"

        AS.Array elements ann _ -> do
            addrOut <- getOutAddr addrIn
            let arrSize  = T.getTypeSize $ SS.expType ann
            out (ArrayGen addrOut arrSize)
            genArray elements addrOut 0
            return addrOut

        AS.Cast e ann _ -> do
            addrOut <- getOutAddr addrIn
            addr <- genExprTmp e
            let oldType = SS.expType $ AS.expAnnot e
                newType = SS.expType ann
                cast = case (oldType, newType) of
                    (T.TInteger, T.TBoolean) -> IntToBool
                    (T.TInteger, T.TReal)    -> IntToDouble
                    (T.TChar, T.TInteger)    -> CharToInt
                    (T.TChar, T.TReal)       -> CharToReal
                    _ -> error $ "Unsupported cast from " ++ show oldType ++ " to " ++ show newType
                newTypeTAC = castTypeToTACType newType
            out (UOp addrOut (Coer cast) addr newTypeTAC)
            return addrOut

-- Generate an expression, the result is stored in a temporary variable that is returned
genExprTmp :: AS.Exp SS.Ann -> StateMonad Address
genExprTmp exp = genExpr exp Nothing

-- Generate an expression, the result is stored in the given address
genExprAddr :: AS.Exp SS.Ann -> Address -> StateMonad ()
genExprAddr exp addr = void $ genExpr exp (Just addr)

-- In the generation of an expression: if the address is specified and if
-- the address is an identifier or a Literal then we need to assign the value
-- to the address specified.
returnAddr :: Maybe Address -> Address -> TACType -> StateMonad Address
returnAddr addrOutMaybe addrReturn ty = case addrOutMaybe of
    Just addrOut -> do
        out (NOp addrOut addrReturn ty)
        return addrOut
    Nothing -> return addrReturn


-- GENERATION OF BOOLEAN OPERATIONS

genBoolExp :: AS.BinOp -> Address -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Label
genBoolExp op tmp e1 e2 = do
    label <- newLabel
    endLabel1 <- genBoolExp' e1

    -- Short-circuit evaluation, jump after the second operand
    let instr = case op of
            AS.Or -> BoolJumpT tmp label
            AS.And -> BoolJumpF tmp label
            _ -> error "Only Or and And operations should be passed to genBinExp"
    out instr

    mapM_ attachToNext endLabel1
    endLabel2 <- genBoolExp' e2
    mapM_ attachToNext endLabel2

    return label

    where
        -- Continue the generation of the boolean expression
        genBoolExp' :: AS.Exp SS.Ann -> StateMonad (Maybe Label)
        genBoolExp' (AS.BinOp AS.Or e1 e2 _ _) = do
            l <- genBoolExp AS.Or tmp e1 e2
            return $ Just l
        genBoolExp' (AS.BinOp AS.And e1 e2 _ _) = do
            l <- genBoolExp AS.And tmp e1 e2
            return $ Just l
        genBoolExp' e = do
            genExprAddr e tmp
            return Nothing

-- Whenever to jump if an a condition is true or false
-- otherwise fallthrough to the next instruction
data IfJump
    = JumpTrue Label
    | JumpFalse Label

-- Generate a boolean expression inside an "if" condition 
genBoolExpIf :: AS.Exp SS.Ann -> IfJump -> StateMonad ()
genBoolExpIf (AS.BinOp AS.Or e1 e2 _ _) jump = do
    case jump of
        JumpTrue _ -> do
            genBoolExpIf e1 jump
            genBoolExpIf e2 jump
        JumpFalse _ -> do
            label <- newLabel
            genBoolExpIf e1 (JumpTrue label)
            genBoolExpIf e2 jump
            attachToNext label
genBoolExpIf (AS.BinOp AS.And e1 e2 _ _) jump = do
    case jump of
        JumpTrue _ -> do
            label <- newLabel
            genBoolExpIf e1 (JumpFalse label)
            genBoolExpIf e2 jump
            attachToNext label
        JumpFalse _ -> do
            genBoolExpIf e1 jump
            genBoolExpIf e2 jump
genBoolExpIf (AS.RelOp op e1 e2 _ _) jump = do
    addr1 <- genExprTmp e1
    addr2 <- genExprTmp e2
    let opTAC = castRelOpToTACRelOp op (SS.expType $ AS.expAnnot e1)
        (opIf, label) = case jump of
            JumpTrue label -> (opTAC, label)
            JumpFalse label -> (negateRelOp opTAC, label)
    out (RelJump opIf addr1 addr2 label)
genBoolExpIf e jump = do
    a <- genExprTmp e
    case jump of
        JumpTrue label -> out (BoolJumpT a label)
        JumpFalse label -> out (BoolJumpF a label)

-- GENERATION OF RELATIONAL OPERATIONS

genRepOp :: Address -> AS.RelOp -> Address -> Address -> T.Type -> StateMonad ()
genRepOp k relOp addr1 addr2 ty = do
    let tyTAC = castTypeToTACType ty
    case ty of
        T.TString -> do
            -- Generate the string comparison function (if not already generated)
            -- and call it
            let (funName, firstArg, secondArg) = stringRelNames relOp
            isCompGenerated <- isStringCompGen relOp
            unless isCompGenerated (genStringComp relOp)
            unless isCompGenerated (addStringCompGen relOp)
            out (NOp firstArg addr1 Addr)
            out (Param firstArg Addr)
            out (NOp secondArg addr2 Addr)
            out (Param secondArg Addr)
            out (FunCall k funName 2 TBoolean)
        T.TArray {} -> do
            -- Generate the array comparison function (if not already generated)
            -- and call it
            let innerType = T.arrayInnerType ty
                (funName, firstArg, secondArg, size) = arrayRelNames relOp innerType
            isCompGenerated <- isArrayCompGen (relOp, innerType)
            unless isCompGenerated (genArrayComp relOp innerType)
            unless isCompGenerated (addArrayCompGen (relOp, innerType))
            out (NOp firstArg addr1 Addr)
            out (Param firstArg tyTAC)
            out (NOp secondArg addr2 Addr)
            out (Param secondArg tyTAC)
            out (NOp size (Lit $ LitInt (T.getTypeSize ty)) TInteger)
            out (FunCall k funName 3 TBoolean)
        T.TReference refType -> do
            let tyTAC = castTypeToTACType refType
            derefTemp1 <- newTmp
            derefTemp2 <- newTmp
            out (DerefGet derefTemp1 addr1 tyTAC)
            out (DerefGet derefTemp2 addr2 tyTAC)
            genRepOp k relOp derefTemp1 derefTemp2 refType
        _ -> let tacRelOp = castRelOpToTACOp relOp ty
             in do
                out (BOp k tacRelOp addr1 addr2 TBoolean)

genStringComp :: AS.RelOp -> StateMonad ()
genStringComp relOp = do
    let (funName, arg1, arg2) = stringRelNames relOp
    labelLoop <- newLabel
    labelEnd <- newLabel
    idx <- newTmp
    char1 <- newTmp
    char2 <- newTmp
    cmp <- newTmp
    newRoutine
    attachToNext funName
    out (Comment $ "String comparison: " ++ relName relOp)
    out (NOp idx (Lit $ LitInt (-1)) TInteger)

    -- Main comparison loop
    attachToNext labelLoop
    out (BOp idx (Add TInteger) (Lit $ LitInt 1) idx TInteger)
    out (IndexGet char1 arg1 idx TChar)
    out (IndexGet char2 arg2 idx TChar)

    -- Check if characters are equal
    out (BOp cmp (Rel (RelEq TChar)) char1 char2 TBoolean)
    out (BoolJumpF cmp labelEnd)  -- If different, exit loop

    -- Characters are equal, check if we reached end of both strings
    out (BOp cmp (Rel (RelEq TChar)) char1 (Lit $ LitChar '\0') TBoolean)
    out (BoolJumpF cmp labelLoop)  -- If not at end, continue loop

    -- Both strings are equal (reached end together)
    let equalResult = relOp `elem` [AS.Eq, AS.Le, AS.Ge]
    out (Return (Lit $ LitBool equalResult))

    -- Characters are different, determine result
    attachToNext labelEnd
    let tacRelOp = case relOp of
            AS.Eq -> RelEq TChar    -- Will be false since we know they're different
            AS.Neq -> RelNeq TChar  -- Will be true since we know they're different  
            AS.Lt -> RelLt TChar
            AS.Le -> RelLe TChar
            AS.Gt -> RelGt TChar
            AS.Ge -> RelGe TChar
    out (BOp cmp (Rel tacRelOp) char1 char2 TBoolean)
    out (Return cmp)
    endRoutine

genArrayComp :: AS.RelOp -> T.Type -> StateMonad ()
genArrayComp relOp innerType = do
    let innerTypeTAC = castTypeToTACType innerType
        (funName, arg1, arg2, size) = arrayRelNames relOp innerType
    labelLoop <- newLabel
    labelTrue <- newLabel
    labelFalse <- newLabel
    idx <- newTmp
    val1 <- newTmp
    val2 <- newTmp
    cmp <- newTmp
    newRoutine
    attachToNext funName
    out (Comment $ "Array comparison: " ++ relName relOp ++ " for type " ++ show innerType)
    out (NOp idx (Lit $ LitInt (- (T.getTypeSize innerType))) TInteger)

    -- Start the main comparison loop
    attachToNext labelLoop
    out (BOp idx (Add TInteger) (Lit $ LitInt (T.getTypeSize innerType)) idx TInteger)
    out (IndexGet val1 arg1 idx innerTypeTAC)
    out (IndexGet val2 arg2 idx innerTypeTAC)

    -- Check if we reached the end of the array
    if relOp `elem` [AS.Eq, AS.Le, AS.Ge] then
        out (RelJump (RelGe TInteger) idx size labelTrue)
    else
        out (RelJump (RelLt TInteger) idx size labelFalse)

    -- If elements are equal, continue
    genRepOp cmp AS.Eq val1 val2 innerType
    out (BoolJumpT cmp labelLoop)

    -- If they are not equal, first check the relation they are in
    when (relOp `elem` [AS.Lt, AS.Le]) $ do
        genRepOp cmp AS.Lt val1 val2 innerType
        out (BoolJumpT cmp labelTrue)

    when (relOp `elem` [AS.Gt, AS.Ge]) $ do
        genRepOp cmp AS.Gt val1 val2 innerType
        out (BoolJumpT cmp labelFalse)

    -- anf then return the right value
    if relOp == AS.Neq then do
        attachToNext labelTrue
        out (Return (Lit $ LitBool True))

        attachToNext labelFalse
        out (Return (Lit $ LitBool False))
    else do
        attachToNext labelFalse
        out (Return (Lit $ LitBool False))

        attachToNext labelTrue
        out (Return (Lit $ LitBool True))
    endRoutine

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
        PName $ ProgramName (funName ++ "val11") Lex.defaultPosn,
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


-- GENERATION OF ARRAY INDEXING

-- Generate the address of an array index operation
genArrayIndex :: AS.Exp SS.Ann -> SS.Ann -> Address -> StateMonad Address
genArrayIndex exp ann addrOut = do

    -- Generate base address
    (baseAddr, baseType) <- genArrayBase exp
    -- Generate index
    indexes <- arrayIndexes exp
    indexAddr <- genArrayIndex indexes (T.arrayDim baseType)

    -- Generate the IndexGet instruction
    out (IndexGet addrOut baseAddr indexAddr (castTypeToTACType $ SS.expType ann))

    return addrOut

    where

        genArrayBase :: AS.Exp SS.Ann -> StateMonad (Address, T.Type)
        genArrayBase (AS.ArrayIndex expr _ _ _) = genArrayBase expr
        genArrayBase notIndexExpr = do
            addr <- genExprTmp notIndexExpr
            let ty = SS.expType $ AS.expAnnot notIndexExpr
            return (addr, ty)

        -- Generate the list of indexes from the array indexing expression
        arrayIndexes :: AS.Exp SS.Ann -> StateMonad [Address]
        arrayIndexes (AS.ArrayIndex e idx _ _) = do
            idxAddr <- genExprTmp idx
            rest <- arrayIndexes e
            return (rest ++ [idxAddr])
        arrayIndexes _ = return []

        -- Generate the array index from the list of addresses and dimensions
        genArrayIndex :: [Address] -> [Int] -> StateMonad Address
        genArrayIndex (a:as) (_:ls) = do
            let accStart = maybeLit a
            idxTemp <- newTmp

            -- Calculate the index.
            -- The offset to the first non-literal index is calculated using foldM
            accEnd <- foldM
                (\acc (addr, dim) -> case (acc, maybeLit addr) of
                    (Just litAcc, Just newLit) -> do
                        return (Just ((litAcc * dim) + newLit))
                    (Just litAcc, Nothing) -> do
                        out (BOp idxTemp (Add TInteger) addr (Lit $ LitInt (litAcc * dim)) TInteger)
                        return Nothing
                    _ -> do
                        out (BOp idxTemp (Mul TInteger) (Lit $ LitInt dim) idxTemp TInteger)
                        out (BOp idxTemp (Add TInteger) addr idxTemp TInteger)
                        return Nothing
                )
                accStart
                (zip as ls)

            -- Multiply index by inner type size
            let innerTypeSize = T.getTypeSize $ T.arrayInnerType $ SS.expType ann
                innerTypeAddr = Lit $ LitInt innerTypeSize

            case accEnd of
                Just idx -> do
                    return (Lit $ LitInt (idx * innerTypeSize))
                Nothing -> do
                    out (BOp idxTemp (Mul TInteger) innerTypeAddr idxTemp TInteger)
                    return idxTemp

            where
                maybeLit :: Address -> Maybe Int
                maybeLit (Lit (LitInt v)) = Just v
                maybeLit _ = Nothing
        genArrayIndex _ _ = error "genArrayIndex: expected at least one index address and one dimension"


-- GENERATION OF ARRAYS DEFINITION

-- Generate an array with the given elements, starting at the given address,
-- offset is a recursion parameter
genArray :: [AS.Exp SS.Ann] -> Address -> Int -> StateMonad ()
genArray elements k offset = do
    mapM_
        (\(i, e) ->
            let ty = SS.expType $ AS.expAnnot e
                tyTAC = castTypeToTACType ty
                elemSize = T.getTypeSize ty
            in
            case e of
                AS.Array elements' _ _ -> do
                    -- If the element is an array, we generate it recursively
                    genArray elements' k (offset + i * elemSize)
                _ -> case ty of
                    -- If the element is an array, we have to copy it
                    T.TArray {} -> do
                        let innerType = T.arrayInnerType ty
                            innerTypeSize = T.getTypeSize innerType
                            fullSize = elemSize `div` innerTypeSize
                        addr <- genExprTmp e
                        copyTmp <- newTmp
                        mapM_
                            (\j -> do
                                out (IndexGet copyTmp addr (Lit $ LitInt (j * innerTypeSize)) (castTypeToTACType innerType))
                                out (IndexSet k (Lit $ LitInt (offset + i * elemSize + j * innerTypeSize)) copyTmp tyTAC))
                            [0.. (fullSize - 1)]
                    _ -> do
                        -- Generate the element and set it in the array
                        addr <- genExprTmp e
                        out (IndexSet k (Lit $ LitInt (offset + i * elemSize)) addr tyTAC)
        )
        (zip [0..] elements)


-- GENERATION OF ROUTINE CALLS

genFunCall :: AS.RoutineCall SS.Ann -> SS.Ann -> T.Type -> Address -> StateMonad Address
genFunCall (AS.RoutineCall routName args _) ann returnType addrOut = do
    genActualParams args ann
    let definitionPos = fromJust $ SS.declPos ann
        routLab = Function $ ProgramName (AS.identName routName) definitionPos
        numArgs = length args
        ty = castTypeToTACType returnType
    out (FunCall addrOut routLab numArgs ty)
    return addrOut

genProcCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad ()
genProcCall (AS.RoutineCall routName args _) ann = do
    genActualParams args ann
    let definitionPos = fromJust $ SS.declPos ann
        routLab = Function $ ProgramName (AS.identName routName) definitionPos
        numArgs = length args
    out (ProcCall routLab numArgs)

-- Generate the actual parameters for a routine call
genActualParams ::  [AS.Exp SS.Ann] -> SS.Ann -> StateMonad ()
genActualParams args ann =
    let params = SS.params ann
        exprs = zip args (fromJust params)
    in
        -- Left to right evaluation of the parameters
        mapM_ (\(expr, param) -> do
            let paramName = AS.identName $ AS.paramName param
                paramAddr = PName $ ProgramName paramName (AS.paramPos param)
                paramType = T.type_ $ AS.paramType param
                paramTACType = castTypeToTACType paramType

            -- Generate the address of the parameter
            addr <- genExprTmp expr

            case (AS.mode param, paramType) of
                -- Value parameters: copy the value
                (AS.Mval, T.TArray {}) -> do
                    out (ArrayGen paramAddr (T.getTypeSize paramType))
                    copy paramAddr addr paramType
                    out (Param paramAddr paramTACType)
                (AS.Mval, _) -> do
                    copy paramAddr addr paramType
                    out (Param paramAddr paramTACType)
                -- Value-result and reference parameters: pass the address
                (_, _) -> do
                    out (Ref paramAddr addr)
                    out (Param paramAddr Addr)
        ) exprs

-- copy data stored in the second address to the first address
copy :: Address -> Address -> T.Type -> StateMonad ()
copy newAddr prevAddr ty = case ty of
    T.TArray elemType _ ->
        let innerTypeSize = T.getTypeSize $ T.arrayInnerType ty
            arraySize = T.getTypeSize ty `div` innerTypeSize
        in
        mapM_ (\i -> do
            k <- newTmp
            out (IndexGet k prevAddr (Lit $ LitInt (i * innerTypeSize)) (castTypeToTACType elemType))
            out (IndexSet newAddr (Lit $ LitInt (i * innerTypeSize)) k (castTypeToTACType elemType))
        ) [0..(arraySize-1)]

    _ -> do
        out (NOp newAddr prevAddr (castTypeToTACType ty))


-- GENERATION OF ROUTINES

genRoutine :: AS.Routine SS.Ann -> StateMonad ()
genRoutine rout = let
    position = AS.routPos rout
    name = AS.identName $ AS.routName rout
    params = AS.params rout
    body = AS.routBody rout
    in do
        newRoutine -- Needed for nested routines declarations
        attachToNext $ Function (ProgramName name position)
        genParamsPre params
        let paramPost = genParamsPost params
        genRoutineBody body paramPost
        endRoutine

-- Generate preamble for valres parameters
genParamsPre :: [AS.Param] -> StateMonad ()
genParamsPre params = do
    let paramsValRes = filter (\p -> AS.mode p == AS.Mvalres) params
    unless (null paramsValRes) $ out (Comment "Start of preamble")
    mapM_ (\(AS.Param {AS.paramName = ident, AS.paramType = tyPos, AS.paramPos = pos}) -> do
        let name = PName $ ProgramName (AS.identName ident) pos
            name' = PName $ ProgramName (AS.identName ident ++ "'") pos
            ty = T.type_ tyPos
            tyTAC = castTypeToTACType ty
        case ty of
            -- Array needed to be copied to the local variable
            T.TArray {} -> do
                out (ArrayGen name' (T.getTypeSize ty))
                k <- newTmp
                out (DerefGet k name tyTAC)
                copy name' k ty
            -- Strings and references to values are not copied (reference are)
            _ -> out (DerefGet name' name tyTAC)
        ) paramsValRes
    unless (null paramsValRes) $ out (Comment "End of preamble")

-- Generate post-amble for valres parameters
genParamsPost :: [AS.Param] -> Instructions
genParamsPost params = let paramsValRes = filter (\p -> AS.mode p == AS.Mvalres) params in
    map (
        \ AS.Param {AS.paramName = ident, AS.paramType = tyPos, AS.paramPos = pos} ->
            let name = PName $ ProgramName (AS.identName ident) pos
                name' = PName $ ProgramName (AS.identName ident ++ "'") pos
                tyTAC = castTypeToTACType $ T.type_ tyPos in
            simpleInstruction (DerefSet name name' tyTAC)
    ) paramsValRes

genRoutineBody :: AS.RoutBody SS.Ann -> Instructions -> StateMonad ()
genRoutineBody (AS.RoutBody decls stmts _) post = do
    genDeclarations decls
    mapM_ (`genStmt` post) stmts


-- GENERATION OF STATEMENTS

genStmt :: AS.Statement SS.Ann -> Instructions -> StateMonad ()
genStmt stmt post = do
    -- don't generate unreachable code
    if SS.unreachable $ AS.annot stmt then
        return ()
    else case stmt of
        AS.StmtAss left right _ _ ->
            -- Evaluate the first left expression, then the first right expression
            -- then the seconds left expression and so on
            mapM_ genAssign (zip left right)
        AS.StmtRoutCall call ann -> case SS.returnType ann of
            Just returnType -> do -- Function call
                k <- newTmp
                void $ genFunCall call ann returnType k
            Nothing ->  -- Procedure call
                genProcCall call ann
        AS.StmtReturn val _ _-> do
            addr <- genExprTmp val
            -- Generate post-amble
            unless (null post) $ out (Comment "Start of post-amble")
            mapM_ outLabeled post
            out (Return addr)
        AS.StmtReturnEmpty _ _-> do
            -- Generate post-amble
            unless (null post) $ out (Comment "Start of post-amble")
            mapM_ outLabeled post
            out ReturnEmpty
        AS.StmtLoop body exit _ _ -> do
            newLoop
            (loopStart, loopEnd) <- getLoopLabels
            -- Generate first body
            mapM_ (`genStmt` post) body
            -- Generate the others (exit-condition, body) pairs
            mapM_ (\exit@(AS.LoopExit cond body _ _) -> do
                -- don't generate unreachable exits
                if SS.unreachable $ AS.loopExitAnnot exit then
                    return ()
                else do
                    genBoolExpIf cond (JumpTrue loopEnd)
                    mapM_ (`genStmt` post) body
                ) exit
            out (UnJump loopStart)
            endLoop
        AS.StmtForEach varName varType arrayExp body _ _ -> do
            let progName = PName $ ProgramName (AS.identName varName) (AS.identPos varName)
                varType' = T.type_ varType
                typeTAC = castTypeToTACType varType'
                typeSize = T.getTypeSize varType'
            arrAddr <- genExprTmp arrayExp
            index <- newTmp
            out (NOp index (Lit $ LitInt 0) TInteger)
            -- Start loop
            newLoop
            (loopStart, _) <- getLoopLabels
            out (IndexGet progName arrAddr index typeTAC)
            -- Generate foreach body
            mapM_ (`genStmt` post) body
            -- Add array size to index
            out (BOp index (Add TInteger) index (Lit $ LitInt typeSize) TInteger)
            out (UnJump loopStart)
            endLoop
        AS.StmtContinue _ _ -> do
            (loopStart, _) <- getLoopLabels
            out (UnJump loopStart)
        AS.StmtIf cond body _ _ -> do
            endLabel <- newLabel
            genBoolExpIf cond (JumpFalse endLabel)
            mapM_ (`genStmt` post) body
            attachToNext endLabel
        AS.StmtIfElse cond bodyT bodyF _ _ -> do
            elseLabel <- newLabel
            endLabel <- newLabel
            genBoolExpIf cond (JumpFalse elseLabel)
            mapM_ (`genStmt` post) bodyT
            out (UnJump endLabel)
            attachToNext elseLabel
            mapM_ (`genStmt` post) bodyF
            attachToNext endLabel


-- GENERATION OF ASSIGNMENTS

genAssign :: (AS.Exp SS.Ann, AS.Exp SS.Ann) -> StateMonad ()
genAssign (left, right) =
    let ty = SS.expType $ AS.expAnnot right in
    case ty of
        T.TArray {} -> do
            -- Arrays needs to be copied
            lExprRes <- genExprTmp left
            rExprRes <- genExprTmp right
            copy lExprRes rExprRes ty
        _ ->  do
            lExprRes <- genLExpr left
            let tyTAC = castTypeToTACType ty
            case lExprRes of
                RefSet addr -> do
                    rExprRes <- genExprTmp right
                    out (DerefSet addr rExprRes tyTAC)
                ArraySet addr idx -> do
                    rExprRes <- genExprTmp right
                    out (IndexSet addr idx rExprRes tyTAC)
                Simple addr -> do
                    genExprAddr right addr

data LExprRes
    = RefSet Address
    | ArraySet Address Address
    | Simple Address

genLExpr :: AS.Exp SS.Ann -> StateMonad LExprRes
genLExpr exp = case exp of
    AS.ExpIdent name (SS.ExpIdentAnn {SS.declPos = Just pos}) _ -> do
        let identName = PName $ ProgramName (AS.identName name) pos
        return $ Simple identName
    AS.Deref expr _ _ -> do
        addr <- genExprTmp expr
        return $ RefSet addr
    AS.ArrayIndex expr index _ _ -> do
        addr <- genExprTmp expr
        idx <- genExprTmp index
        return $ ArraySet addr idx
    _ -> error "genLExpr: Invalid left-hand side expression (parser should prevent this)"
