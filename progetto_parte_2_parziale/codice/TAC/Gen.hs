module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified StatSem.Utils             as SS
import qualified Types                     as T
import qualified TAC.Printer()
import TAC.Def
import Control.Monad (unless)


-- MONAD DEFINITION AND HELPER FUNCTIONS

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCount, staticCount :: Int,
    -- The routine stack and the current routine is used to menage nested routines
    routineStack :: [Instructions],
    currentRoutine :: Instructions,
    -- Current stream for TAC code
    stream :: Instructions,
    -- Static data stream
    staticStream :: [StaticData],
    -- List of labels to be attached to the next instruction
    nextLabel :: [Label],
    -- current loop labels stack
    loopLabelsStack :: [(Label, Label)]
}

-- Add instruction to the stream
outLabeled :: LabeledInstruction -> StateMonad ()
outLabeled instrLab = do
    state <- St.get
    let -- Add pending label
        instrLab' = instrLab { label = nextLabel state }
        newStream = instrLab' : currentRoutine state
    St.put $ state { currentRoutine = newStream, nextLabel = [] }

-- Add instruction (with no label) to the stream
out :: Instruction -> StateMonad ()
out = outLabeled . simpleInstruction

-- Add static data to the static stream
outStatic :: StaticValue -> StateMonad Address
outStatic instrLab = do
    state <- St.get
    let k = staticCount state
        newStream = Static (Stat k) instrLab : staticStream state
    St.put $ state { staticStream = newStream,
                     staticCount = k + 1 }
    return (Stat k)

-- Return a new temporary variable
newTmp :: StateMonad Address
newTmp = do
    state <- St.get
    let k = tempCount state
    St.put $ state { tempCount = k + 1 }
    return $ Temp (Temporary k)

-- Return a new label
newLabel :: StateMonad Label
newLabel = do
    state <- St.get
    let k = labelCount state
    St.put $ state { labelCount = k + 1 }
    return $ Loc k

-- Attach a label to the next instruction
attachToNext :: Label -> StateMonad ()
attachToNext label = do
    state <- St.get
    St.put $ state { nextLabel = label : nextLabel state }

-- Call when starting a new routine, pushes the current routine onto the stack
-- This is used to menage nested routines
newRoutine :: StateMonad ()
newRoutine = do
    state <- St.get
    St.put $ state { routineStack = currentRoutine state : routineStack state,
                     currentRoutine = [] }

-- Call when ending a routine (cannot be called before a newRoutine)
-- This is used to menage nested routines
endRoutine :: StateMonad ()
endRoutine = do
    state <- St.get
    let currentRout = currentRoutine state
    case routineStack state of
        [] -> St.put state
        (r:rs) -> do
            St.put $ state { routineStack = rs,
                             currentRoutine = r,
                             stream = currentRout ++ stream state }

-- Call when starting a new loop. Creates a new labels for the loop start and
-- end and pushes them onto a stack.
newLoop :: StateMonad ()
newLoop = do
    loopStart <- newLabel
    loopEnd <- newLabel
    attachToNext loopStart
    state <- St.get
    St.put $ state { loopLabelsStack = (loopStart, loopEnd) : loopLabelsStack state }

-- Call when ending a loop. Attaches the loop end label to the next instruction
-- Only call when exiting a loop
endLoop :: StateMonad ()
endLoop = do
    state <- St.get
    let (_, loopEnd) : stack = loopLabelsStack state
    attachToNext loopEnd
    -- Get updated state
    state' <- St.get
    St.put $ state' { loopLabelsStack = stack }

-- Only call when inside a loop
getLoopLabels :: StateMonad (Label, Label)
getLoopLabels = do
    head . loopLabelsStack <$> St.get

-- Entrypoint for TAC generation

-- Generate the TAC code form the state
genProgram :: AS.Declarations SS.Ann -> TAC
genProgram ast =
    let finalState = St.execState (genEntryPoint ast) initState
        -- Reverse the stream to have the instructions in the correct order
    in TAC (reverse $ stream finalState) (reverse $ staticStream finalState)

initState :: TACState
initState = TACState {
    tempCount = 0,
    labelCount = 0,
    staticCount = 0,
    routineStack = [],
    currentRoutine = [],
    stream = [],
    staticStream = [],
    nextLabel = [],
    loopLabelsStack = []
}


-- TAC GENERATION FUNCTIONS

genEntryPoint :: AS.Declarations SS.Ann -> StateMonad ()
genEntryPoint (AS.ProgramDecl _ vars routines _) = do
    newRoutine
    mapM_ genVar vars
    -- Program calls main and then ends
    out (ProcCall (Function "main") 0)
    out (Custom "END_PROGRAM")
    endRoutine
    mapM_ genRoutine routines

genDeclarations :: AS.Declarations SS.Ann -> StateMonad ()
genDeclarations (AS.ProgramDecl _ vars routines _) = do
    mapM_ genVar vars
    mapM_ genRoutine routines

genVar :: AS.VarA SS.Ann -> StateMonad ()
genVar (AS.VarAss idents ty values _ pos) =
    -- Evaluate the first left expression, then the first right expression
    -- then the seconds left expression and so on
    mapM_ genAssignment (zip idents values)
    where
    genAssignment :: (AS.Ident, AS.Exp SS.Ann) -> StateMonad ()
    genAssignment (ident, exp) = do
        addr <- genExpr exp
        let tyTAC = castTypeToTACType $ T.type_ ty
            name = PName $ ProgramName (AS.identName ident) pos
        out (NOp name addr tyTAC)

genExpr :: AS.Exp SS.Ann -> StateMonad Address
genExpr exp = do
    case AS.expAnnot exp of
        -- Handle expressions with known values
        SS.ExpAnn { SS.ctVal = Just val } -> genLit val
        SS.IdentAnn { SS.ctVal = Just val } -> genLit val
        -- Handle expressions with no known values
        _ -> genUnknownExpr exp
    where
        genLit :: SS.CTValue -> StateMonad Address
        genLit (SS.CTVInteger v) = return $ Lit (LitInt v)
        genLit (SS.CTVReal v)    = return $ Lit (LitReal v)
        genLit (SS.CTVChar v)    = return $ Lit (LitChar v)
        genLit (SS.CTVBoolean v) = return $ Lit (LitBool v)
        -- Put strings in static data
        genLit (SS.CTVString v)  = outStatic (STVString (v ++ "\0"))
        genLit (SS.CTVRef v)     = do
            k <- newTmp
            lit <- genLit v
            out (Ref k lit)
            return k

genUnknownExpr :: AS.Exp SS.Ann -> StateMonad Address
genUnknownExpr exp = do
    case exp of
        AS.BoolNeg e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out (UOp k BoolNeg addr TBoolean)
            return addr

        AS.Sign e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let ty = castTypeToTACType $ SS.expType ann
            out (UOp k (Sign ty) addr ty)
            return addr

        AS.BinOp AS.Or e1 e2 _ _ -> do
            k <- newTmp
            genBoolExp AS.Or k e1 e2

        AS.BinOp AS.And e1 e2 _ _ -> do
            k <- newTmp
            genBoolExp AS.And k e1 e2

        AS.BinOp binOp e1 e2 ann _ -> do
            k <- newTmp
            -- Left to right evaluation
            addr1 <- genExpr e1
            addr2 <- genExpr e2
            let tacOp = castBinOpToTACOp binOp (SS.expType ann)
                ty = castTypeToTACType $ SS.expType ann
            out (BOp k tacOp addr1 addr2 ty)
            return k

        AS.RelOp relOp e1 e2 _ _ -> genRepOp relOp e1 e2

        AS.Ref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out (Ref k addr)
            return k

        -- Already handled by genExpr
        AS.Literal {} -> error "Literal should not be passed to genUnknownExpr"

        AS.FunCall call ann@(SS.FunCallAnn {}) _ -> do
            genFunCall call ann

        AS.FunCall {} -> error "Unreachable"

        AS.Deref e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let ty = castTypeToTACType $ SS.expType ann
            out (DerefGet k addr ty)
            return k

        AS.ArrayIndex _ _ ann _ -> genArrayIndex exp ann

        -- ConstCategory is handled by genExpr
        AS.ExpIdent ident (SS.IdentAnn ty _ SS.VarCategory mode (Just declPos)) _ -> do
            let identName = PName $ ProgramName (AS.identName ident) declPos
                identName' = PName $ ProgramName (AS.identName ident ++ "'") declPos
                tyTAC = castTypeToTACType ty
            case mode of
                -- Identifiers that are passed by value-result are appended with
                -- the "'" and a dereference is made
                Just AS.Mvalres -> do
                    k <- newTmp
                    out (DerefGet k identName' tyTAC)
                    return k
                -- Identifiers that are passes by reference are dereferenced before being used
                Just AS.Mref -> do
                    k <- newTmp
                    out (DerefGet k identName tyTAC)
                    return k

                Just AS.Mval -> return identName

                Nothing -> return identName

        AS.ExpIdent ident ann@(SS.FunCallAnn {}) _ -> do
            genFunCall (AS.callFromIdent ident) ann

        AS.ExpIdent {} -> error "Unreachable"

        -- TODO: When we write something like `A := Array[1,2]`, it generates:
        -- genArray t0, 8
        -- t0[0] = 1
        -- t0[4] = 2
        -- A = t0
        -- Maybe we shouldn't generate a temporary variable here?
        -- Same for AS.Ref
        AS.Array elements ann _ -> do
            k <- newTmp
            let arrSize  = T.getTypeSize $ SS.expType ann
            out (ArrayGen k arrSize)
            genArray elements k 0
            return k

        AS.Cast e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let oldType = SS.expType $ AS.expAnnot e
                newType = SS.expType ann
                cast = case (oldType, newType) of
                    (T.TBoolean, T.TInteger) -> BoolToInt
                    (T.TInteger, T.TReal)    -> IntToDouble
                    (T.TChar, T.TInteger)    -> CharToInt
                    -- TODO: Char to String?
                    _ -> error $ "Unsupported cast from " ++ show oldType ++ " to " ++ show newType
                newTypeTAC = castTypeToTACType newType
            out (UOp k (Coer cast) addr newTypeTAC)
            return k


-- GENERATION OF BOOLEAN OPERATIONS

-- TODO: a lot of useless temporary variables are generated here
genBoolExp :: AS.BinOp -> Address -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genBoolExp op tmp e1 e2 = do
    -- Generate left operand
    (addr1, isBoolOp1) <- genExpr' e1
    unless isBoolOp1 (out (NOp tmp addr1 TBoolean))

    -- Jump!
    label <- newLabel
    let instr = case op of
            AS.Or -> BoolJumpT tmp label
            AS.And -> BoolJumpF tmp label
            _ -> error "Only Or and And operations should be passed to genBinExp"
    out instr

    -- Generate right operand
    (addr2, isBoolOp2) <- genExpr' e2
    unless isBoolOp2 (out (NOp tmp addr2 TBoolean))

    -- Jump to the next instruction
    attachToNext label

    return tmp
    where
        genExpr' :: AS.Exp SS.Ann -> StateMonad (Address, Bool)
        genExpr' (AS.BinOp AS.Or e1 e2 _ _)  = do
            addr <- genBoolExp AS.Or tmp e1 e2
            return (addr, True)
        genExpr' (AS.BinOp AS.And e1 e2 _ _) = do
            addr <- genBoolExp AS.And tmp e1 e2
            return (addr, True)
        genExpr' e = do
            addr <- genExpr e
            return (addr, False)

-- GENERATION OF RELATIONAL OPERATIONS

genRepOp :: AS.RelOp -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genRepOp relOp e1 e2 = do
    k <- newTmp
    -- Left to right evaluation
    addr1 <- genExpr e1
    addr2 <- genExpr e2
    let ty = SS.expType $ AS.expAnnot e1
    case ty of
        T.TString -> error "TODO:"
        T.TArray {} -> error "TODO:"
        T.TReference {} -> error "TODO:"
        _ -> let tacRelOp = castRelOpToTACOp relOp ty
             in do
                out (BOp k tacRelOp addr1 addr2 TBoolean)
    return k


-- GENERATION OF ARRAY INDEXING

-- Generate the address of an array index operation
genArrayIndex :: AS.Exp SS.Ann -> SS.Ann -> StateMonad Address
genArrayIndex exp ann = do
    k <- newTmp

    -- Generate base address
    (baseAddr, baseType) <- genArrayBase exp
    -- Generate index
    indexes <- arrayIndexes exp
    indexAddr <- genArrayIndex indexes (T.arrayDim baseType)

    -- Multiply index by inner type size
    let innerTypeSize = T.getTypeSize $ T.arrayInnerType $ SS.expType ann
        innerTypeAddr = Lit $ LitInt innerTypeSize
    out (BOp indexAddr (Mul TInteger) innerTypeAddr indexAddr TInteger)

    -- Generate the IndexGet instruction
    out (IndexGet k baseAddr indexAddr (castTypeToTACType $ SS.expType ann))

    return k

    where

        genArrayBase :: AS.Exp SS.Ann -> StateMonad (Address, T.Type)
        genArrayBase (AS.ArrayIndex expr _ _ _) = genArrayBase expr
        genArrayBase notIndexExpr = do
            addr <- genExpr notIndexExpr
            let ty = SS.expType $ AS.expAnnot notIndexExpr
            return (addr, ty)

        -- Generate the list of indexes from the array indexing expression
        arrayIndexes :: AS.Exp SS.Ann -> StateMonad [Address]
        arrayIndexes (AS.ArrayIndex e idx _ _) = do
            idxAddr <- genExpr idx
            rest <- arrayIndexes e
            return (rest ++ [idxAddr])
        arrayIndexes _ = return []

        -- Generate the array index from the list of addresses and dimensions
        -- TODO: If indexes are literals, we can optimize the generation
        genArrayIndex :: [Address] -> [Int] -> StateMonad Address
        genArrayIndex (a:as) (_:ls) = do
            idxTemp <- newTmp
            out (NOp idxTemp a TInteger)
            mapM_
                (\(addr, dim) -> do
                    out (BOp idxTemp (Mul TInteger) (Lit $ LitInt dim) idxTemp TInteger)
                    out (BOp idxTemp (Add TInteger) addr idxTemp TInteger))
                (zip as ls)
            return idxTemp
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
                _ -> do
                    -- Generate the element and set it in the array
                    addr <- genExpr e
                    out (IndexSet k (Lit $ LitInt (offset + i * elemSize)) addr tyTAC)
        )
        (zip [0..] elements)


-- GENERATION OF ROUTINE CALLS

genFunCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad Address
genFunCall (AS.RoutineCall routName args _) ann = do
    k <- newTmp
    genActualParams args ann
    let returnType = SS.expType ann
        routLab = Function $ AS.identName routName
        numArgs = length args
        ty = castTypeToTACType returnType
    case returnType of
        T.TArray {} -> error "TODO: handle array return types"
        -- Ref cannot be returned by a function
        _ -> out (FunCall k routLab numArgs ty)
    return k

genProcCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad ()
genProcCall (AS.RoutineCall routName args _) ann = do
    genActualParams args ann
    let routLab = Function $ AS.identName routName
        numArgs = length args
    out (ProcCall routLab numArgs)

-- Generate the actual parameters for a routine call
genActualParams ::  [AS.Exp SS.Ann] -> SS.Ann -> StateMonad ()
genActualParams args ann =
    let params = SS.params ann
        exprs = zip args params
    in
        -- Left to right evaluation of the parameters
        mapM_ (\(expr, param) -> do
            let paramName = AS.identName $ AS.paramName param
                paramAddr = PName $ ProgramName paramName (AS.paramPos param)
                paramType = T.type_ $ AS.paramType param
                paramTACType = castTypeToTACType paramType

            -- Generate the address of the parameter
            addr <- genExpr expr

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
    name = AS.identName $ AS.routName rout
    params = AS.params rout
    body = AS.routBody rout
    in do
        newRoutine -- Needed for nested routines declarations
        attachToNext $ Function name
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
            T.TString -> error "TODO: handle strings, also for references, what we should do?"
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
            -- TODO: strings? (Array should be good)
            simpleInstruction (DerefSet name name' tyTAC)
    ) paramsValRes

genRoutineBody :: AS.RoutBody SS.Ann -> Instructions -> StateMonad ()
genRoutineBody (AS.RoutBody decls stmts _) post = do
    genDeclarations decls
    mapM_ (`genStmt` post) stmts


-- GENERATION OF STATEMENTS

genStmt :: AS.Statement SS.Ann -> Instructions -> StateMonad ()
genStmt stmt post = case stmt of
    AS.StmtAss left right _ _ ->
        -- Evaluate the first left expression, then the first right expression
        -- then the seconds left expression and so on
        mapM_ genAssign (zip left right)
    AS.StmtRoutCall call ann -> if isFunction ann then
            do
                _ <- genFunCall call ann
                return ()
        else
            genProcCall call ann
    AS.StmtReturn val _ _-> do
        addr <- genExpr val
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
        mapM_ (\(AS.LoopExit cond body _ _) -> do
                -- TODO: Better way to generate condition using shortcut?
                condAddr <- genExpr cond
                out (BoolJumpT condAddr loopEnd)
                mapM_ (`genStmt` post) body
            ) exit
        out (UnJump loopStart)
        endLoop
    AS.StmtContinue _ _ -> do
        (loopStart, _) <- getLoopLabels
        out (UnJump loopStart)
    AS.StmtIf cond body _ _ -> do
        endLabel <- newLabel
        -- TODO: Better way to generate condition using shortcut?
        condAddr <- genExpr cond
        out (BoolJumpF condAddr endLabel)
        mapM_ (`genStmt` post) body
        attachToNext endLabel
    AS.StmtIfElse cond bodyT bodyF _ _ -> do
        elseLabel <- newLabel
        endLabel <- newLabel
        -- TODO: Better way to generate condition using shortcut?
        condAddr <- genExpr cond
        out (BoolJumpF condAddr elseLabel)
        mapM_ (`genStmt` post) bodyT
        out (UnJump endLabel)
        attachToNext elseLabel
        mapM_ (`genStmt` post) bodyF
        attachToNext endLabel

isFunction :: SS.Ann -> Bool
isFunction = error "TODO: should be known by annotations, currently only procedures should be statements"


-- GENERATION OF ASSIGNMENTS

genAssign :: (AS.Exp SS.Ann, AS.Exp SS.Ann) -> StateMonad ()
genAssign (left, right) =
    let ty = SS.expType $ AS.expAnnot right in
    case ty of
        T.TArray {} -> do
            -- Arrays needs to be copied
            lExprRes <- genExpr left
            rExprRes <- genExpr right
            copy lExprRes rExprRes ty
        _ ->  do
            lExprRes <- genLExpr left
            rExprRes <- genExpr right
            let tyTAC = castTypeToTACType ty
            case lExprRes of
                RefSet addr ->
                    out (DerefSet addr rExprRes tyTAC)
                ArraySet addr idx ->
                    out (IndexSet addr idx rExprRes tyTAC)
                Simple addr ->
                    out (NOp addr rExprRes tyTAC)

data LExprRes
    = RefSet Address
    | ArraySet Address Address
    | Simple Address

genLExpr :: AS.Exp SS.Ann -> StateMonad LExprRes
genLExpr exp = case exp of
    AS.ExpIdent name (SS.IdentAnn {SS.declPos = Just pos}) _ -> do
        let identName = PName $ ProgramName (AS.identName name) pos
        return $ Simple identName
    AS.Deref expr _ _ -> do
        addr <- genExpr expr
        return $ RefSet addr
    AS.ArrayIndex expr index _ _ -> do
        addr <- genExpr expr
        idx <- genExpr index
        return $ ArraySet addr idx
    _ -> error "Unreachable, error thrown while parsing"
