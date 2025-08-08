module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified StatSem.Utils             as SS
import qualified Types                     as T
import TAC.Def


-- Monad definitions and helper functions

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCount :: Int,
    routineStack :: [TAC],
    currentStream :: TAC,
    nextLabel :: Maybe Label,
    loopLabelsStack :: [(Label, Label)]
}

out :: LabeledInstruction -> StateMonad ()
out instr = do
    state <- St.get
    --
    let -- Add pending label
        instr' = case nextLabel state of
            Just label -> instr { label = Just label }
            Nothing    -> instr
        newStream = instr' : currentStream state
    St.put $ state { currentStream = newStream, nextLabel = Nothing }

newTmp :: StateMonad Address
newTmp = do
    state <- St.get
    let k = tempCount state
    St.put $ state { tempCount = k + 1 }
    return $ Temp (Temporary k)

newLabel :: StateMonad Label
newLabel = do
    state <- St.get
    let k = labelCount state
    St.put $ state { labelCount = k + 1 }
    return $ Loc k

attachToNext :: Label -> StateMonad ()
attachToNext label = do
    state <- St.get
    let newNext = Just label
    St.put $ state { nextLabel = newNext }

newRoutine :: StateMonad ()
newRoutine = do
    state <- St.get
    St.put $ state { routineStack = currentStream state : routineStack state,
                     currentStream = [] }

endRoutine :: StateMonad ()
endRoutine = do
    state <- St.get
    let currentRoutine = currentStream state
    case routineStack state of
        [] -> St.put state
        (r:rs) -> do
            St.put $ state { routineStack = rs,
                             currentStream = r ++ currentRoutine }

newLoop :: StateMonad ()
newLoop = do
    state <- St.get
    loopStart <- newLabel
    loopEnd <- newLabel
    attachToNext loopStart
    -- Get updated state
    state' <- St.get
    St.put $ state' { loopLabelsStack = (loopStart, loopEnd) : loopLabelsStack state' }

-- Only call when inside a loop
getLoopLabels :: StateMonad (Label, Label)
getLoopLabels = do
    head . loopLabelsStack <$> St.get

-- Only call when exiting a loop
endLoop :: StateMonad ()
endLoop = do
    state <- St.get
    let (_, loopEnd) : stack = loopLabelsStack state
    attachToNext loopEnd
    -- Get updated state
    state' <- St.get
    St.put $ state' { loopLabelsStack = stack }

-- Entrypoint for TAC generation
genProgram :: AS.Declarations SS.Ann -> TAC
genProgram ast =
    let finalState = St.execState (genDeclarations ast) initState
        -- Reverse the stream to have the instructions in the correct order
    in reverse (currentStream finalState)

initState :: TACState
initState = TACState {
    tempCount = 0,
    labelCount = 0,
    routineStack = [],
    currentStream = [],
    nextLabel = Nothing,
    loopLabelsStack = []
}

-- TAC generation functions

genDeclarations :: AS.Declarations SS.Ann -> StateMonad ()
genDeclarations (AS.ProgramDecl _ vars routines _) = do
    mapM_ genVar vars
    mapM_ genRoutine routines

genVar :: AS.VarA SS.Ann -> StateMonad ()
genVar (AS.VarAss idents ty values _ _) =
    mapM_ genAssignment (zip idents values)
    where
    genAssignment :: (AS.Ident, AS.Exp SS.Ann) -> StateMonad ()
    genAssignment (ident, exp) = do
        addr <- genExpr exp
        let tyTAC = castTypeToTACType $ T.type_ ty
            name = PName $ ProgramName (AS.identName ident) "TODO: add routine name to variable"
        out $ LabeledInstruction Nothing (NOp name addr tyTAC)

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
        genLit (SS.CTVString v)  = genString v
        genLit (SS.CTVRef v)     = do
            k <- newTmp
            lit <- genLit v
            out $ simpleInstruction (Ref k lit)
            return k

genString :: String -> StateMonad Address
-- TODO: Strings are array of chars
genString _ = error "genString not implemented yet"

genUnknownExpr :: AS.Exp SS.Ann -> StateMonad Address
genUnknownExpr exp = do
    case exp of
        AS.BoolNeg e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out $ simpleInstruction (UOp k BoolNeg addr TBoolean)
            return addr

        AS.Sign e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let ty = castTypeToTACType $ SS.expType ann
            out $ simpleInstruction (UOp k (Sign ty) addr ty)
            return addr

        AS.BinOp AS.Or e1 e2 _ _ -> do
            k <- newTmp
            genBinExp AS.Or k e1 e2

        AS.BinOp AS.And e1 e2 _ _ -> do
            k <- newTmp
            genBinExp AS.And k e1 e2

        AS.BinOp binOp e1 e2 ann _ -> do
            k <- newTmp
            -- Left to right evaluation
            addr1 <- genExpr e1
            addr2 <- genExpr e2
            let tacOp = castBinOpToTACOp binOp (SS.expType ann)
                ty = castTypeToTACType $ SS.expType ann
            out $ simpleInstruction (BOp k tacOp addr1 addr2 ty)
            return k

        AS.RelOp relOp e1 e2 _ _ -> genRepOp relOp e1 e2

        AS.Ref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out $ simpleInstruction (Ref k addr)
            return k

        -- Already handled by genExpr
        AS.Literal {} -> error "Literal should not be passed to genUnknownExpr"

        AS.FunCall call ann@(SS.RoutCallAnn {}) _ -> do
            genFunCall call ann

        AS.FunCall {} -> error "Unreachable"

        AS.Deref e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let ty = castTypeToTACType $ SS.expType ann
            out $ simpleInstruction (DerefGet k addr ty)
            return k

        AS.ArrayIndex e index ann _ -> do
            k <- newTmp
            addr <- genExpr e
            indexAddr <- genExpr index
            let elemSize = T.getTypeSize $ SS.expType $ AS.expAnnot e
                ty = castTypeToTACType $ SS.expType ann
            if elemSize == 1 then do
                out $ simpleInstruction (IndexGet k addr indexAddr ty)
            else do
                -- If the element size is not 1, we need to multiply the index by the element size
                out $ simpleInstruction (BOp indexAddr (Mul TInteger) indexAddr (Lit $ LitInt elemSize) TInteger)
                out $ simpleInstruction (IndexGet k addr indexAddr ty)

            return k

        -- ConstCategory is handled by genExpr
        AS.ExpIdent ident (SS.IdentAnn ty _ SS.VarCategory mode) _ -> do
            let identName = PName $ ProgramName (AS.identName ident) "TODO: add routine name to variable"
                identName' = PName $ ProgramName (AS.identName ident ++ "'") "TODO: add routine name to variable"
                tyTAC = castTypeToTACType $ T.elementsType ty
            case mode of
                -- Identifiers that are passed by value-result are appended with
                -- the "'" and a dereference is made
                Just AS.Mvalres -> do
                    k <- newTmp
                    out $ simpleInstruction (DerefGet k identName' tyTAC)
                    return k
                -- Identifiers that are passes by reference are dereferenced before being used
                Just AS.Mref -> do
                    k <- newTmp
                    out $ simpleInstruction (DerefGet k identName tyTAC)
                    return k

                Just AS.Mval -> return identName

                Nothing -> return identName

        AS.ExpIdent ident ann@(SS.RoutCallAnn {}) _ -> do
            genFunCall (AS.callFromIdent ident) ann

        AS.ExpIdent {} -> error "Unreachable"

        AS.Array elements ann _ -> do
            k <- newTmp
            let arrSize  = T.getTypeSize $ SS.expType ann
            out $ simpleInstruction (ArrayGen k arrSize)
            genArray elements k

            where
                genArray :: [AS.Exp SS.Ann] -> Address -> StateMonad Address
                genArray elements k = do
                    let elemSize = T.getTypeSize  $ T.elementsType $ SS.expType ann
                        tyTAC = castTypeToTACType $ T.elementsType $ SS.expType ann
                    mapM_
                        (\(i, e) -> do
                            addr <- genExpArray e k
                            out $ simpleInstruction (IndexSet k (Lit $ LitInt (i * elemSize)) addr tyTAC))
                        (zip [0..] elements)
                    return k

                genExpArray :: AS.Exp SS.Ann -> Address -> StateMonad Address
                genExpArray (AS.Array elements _ _) k = genArray elements k
                genExpArray e _ = genExpr e

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
            out $ simpleInstruction (UOp k (Coer cast) addr newTypeTAC)
            return k

genBinExp :: AS.BinOp -> Address -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genBinExp op tmp e1 e2 = do
    -- left to right evaluation
    addr1 <- genExpr' e1
    out $ simpleInstruction (NOp tmp addr1 TBoolean)
    label <- newLabel
    let instr = case op of
            AS.Or -> BoolJumpF tmp label
            AS.And -> BoolJumpT tmp label
            _ -> error "Only Or and And operations should be passed to genBinExp"
    out $ simpleInstruction instr
    addr2 <- genExpr' e2
    out $ simpleInstruction (NOp tmp addr2 TBoolean)
    attachToNext label
    return tmp
    where
        genExpr' :: AS.Exp SS.Ann -> StateMonad Address
        genExpr' (AS.BinOp AS.Or e1 e2 _ _)  = genBinExp AS.Or tmp e1 e2
        genExpr' (AS.BinOp AS.And e1 e2 _ _) = genBinExp AS.And tmp e1 e2
        genExpr' e = genExpr e

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
                out $ simpleInstruction (BOp k tacRelOp addr1 addr2 TBoolean)
    return k

genFunCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad Address
genFunCall (AS.RoutineCall routName args _) ann = do
    k <- newTmp
    genActualParams args ann (AS.identName routName)
    let returnType = SS.expType ann
        routLab = Function $ AS.identName routName
        numArgs = length args
        ty = castTypeToTACType returnType
    case returnType of
        T.TArray {} -> error "TODO: handle array return types"
        T.TString -> error "TODO: handle string return types"
        -- Ref cannot be returned by a function
        _ -> out $ simpleInstruction (FunCall k routLab numArgs ty)
    return k

genProcCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad ()
genProcCall (AS.RoutineCall routName args _) ann = do
    genActualParams args ann (AS.identName routName)
    let routLab = Function $ AS.identName routName
        numArgs = length args
    out $ simpleInstruction (ProcCall routLab numArgs)

genActualParams ::  [AS.Exp SS.Ann] -> SS.Ann -> String -> StateMonad ()
genActualParams args ann procName =
    let params = SS.params ann
        exprs = zip args params
    in
        mapM_ (\(expr, param) -> do
            let paramName = AS.identName $ AS.paramName param
                paramAddr = PName $ ProgramName paramName procName
                paramType = T.type_ $ AS.paramType param
                paramTACType = castTypeToTACType paramType

            addr <- genExpr expr

            case AS.mode param of
                AS.Mval -> case paramType of
                    T.TString -> error "TODO: handle string"
                    T.TArray {} -> do
                        out $ simpleInstruction (ArrayGen paramAddr (T.getTypeSize paramType))
                        copy paramAddr addr paramType
                        out $ simpleInstruction (Param paramAddr paramTACType)
                    _ -> do
                        copy paramAddr addr paramType
                        out $ simpleInstruction (Param paramAddr paramTACType)
                -- Create a reference for value-result and reference
                _ -> do
                    out $ simpleInstruction (Ref paramAddr addr)
                    out $ simpleInstruction (Param paramAddr paramTACType)
        ) exprs

-- copy data stored in the second address to the first address
copy :: Address -> Address -> T.Type -> StateMonad ()
copy newAddr prevAddr ty = case ty of
    T.TString -> error "TODO: implement copy for strings"
    T.TArray elemType len ->
        let elemSize = T.getTypeSize ty
            elemTACTy = castTypeToTACType elemType
        in
        mapM_ (\i -> do
            k <- newTmp
            out $ simpleInstruction (IndexGet k prevAddr (Lit $ LitInt (i * elemSize)) elemTACTy)
            out $ simpleInstruction (IndexSet newAddr (Lit $ LitInt (i * elemSize)) k elemTACTy)
            copy newAddr k elemType
        ) [0..len]

    _ -> do
        out $ simpleInstruction (NOp newAddr prevAddr (castTypeToTACType ty))


genRoutine :: AS.Routine SS.Ann -> StateMonad ()
genRoutine rout = let
    name = AS.identName $ AS.routName rout
    params = filter (\p -> AS.mode p == AS.Mvalres) (AS.params rout)
    body = AS.routBody rout
    in do
        newRoutine
        attachToNext $ Function name
        genParamsPre params name
        let paramPost = genParamsPost params name
        genRoutineBody body paramPost
        endRoutine


genParamsPre :: [AS.Param] -> String -> StateMonad ()
genParamsPre params routName = let paramsValRes = filter (\p -> AS.mode p == AS.Mvalres) params in
    mapM_ (
        \ AS.Param {AS.paramName = ident, AS.paramType = ty} -> do
            let name = PName $ ProgramName (AS.identName ident) routName
                name' = PName $ ProgramName (AS.identName ident ++ "'") routName
                tyTAC = castTypeToTACType $ T.type_ ty
            out $ simpleInstruction (DerefGet name' name tyTAC)
    ) paramsValRes

genParamsPost :: [AS.Param] -> String -> TAC
genParamsPost params routName = let paramsValRes = filter (\p -> AS.mode p == AS.Mvalres) params in
    map (
        \ AS.Param {AS.paramName = ident, AS.paramType = ty} ->
            let name = PName $ ProgramName (AS.identName ident) routName
                name' = PName $ ProgramName (AS.identName ident ++ "'") routName
                tyTAC = castTypeToTACType $ T.type_ ty in
            simpleInstruction (DerefSet name name' tyTAC)
    ) paramsValRes

genRoutineBody :: AS.RoutBody SS.Ann -> TAC -> StateMonad ()
genRoutineBody (AS.RoutBody decls stmts) post = do
    genDeclarations decls
    mapM_ (`genStmt` post) stmts

genStmt :: AS.Statement SS.Ann -> TAC -> StateMonad ()
genStmt stmt post = case stmt of
    AS.StmtAss left right _ _ ->
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
        mapM_ out post
        out $ simpleInstruction (Return addr)
    AS.StmtReturnEmpty _ _-> do
        -- Generate post-amble
        mapM_ out post
        out $ simpleInstruction ReturnEmpty
    AS.StmtLoop body exit _ _ -> do
        newLoop
        -- Generate first body
        mapM_ (`genStmt` post) body
        -- Generate the others (exit-condition, body) pairs
        mapM_ (\(AS.LoopExit cond body _ _) -> do
                -- TODO: Better way to generate condition using shortcut?
                condAddr <- genExpr cond
                (_, loopEnd) <- getLoopLabels
                out $ simpleInstruction (BoolJumpT condAddr loopEnd)
                mapM_ (`genStmt` post) body
            ) exit
        endLoop
    AS.StmtContinue _ _ -> do
        (loopStart, _) <- getLoopLabels
        out $ simpleInstruction (UnJump loopStart)
    AS.StmtIf cond body _ _ -> do
        endLabel <- newLabel
        -- TODO: Better way to generate condition using shortcut?
        condAddr <- genExpr cond
        out $ simpleInstruction (BoolJumpF condAddr endLabel)
        mapM_ (`genStmt` post) body
        attachToNext endLabel
    AS.StmtIfElse cond bodyT bodyF _ _ -> do
        elseLabel <- newLabel
        endLabel <- newLabel
        -- TODO: Better way to generate condition using shortcut?
        condAddr <- genExpr cond
        out $ simpleInstruction (BoolJumpF condAddr elseLabel)
        mapM_ (`genStmt` post) bodyT
        attachToNext elseLabel
        out $ simpleInstruction (UnJump endLabel)
        mapM_ (`genStmt` post) bodyF
        attachToNext endLabel

isFunction :: SS.Ann -> Bool
isFunction = error "TODO: should be known by annotations"

genAssign :: (AS.Exp SS.Ann, AS.Exp SS.Ann) -> StateMonad ()
genAssign (left, right) =
    let ty = getAssignType right in
    case ty of
        T.TArray {} -> do
            lExprRes <- genExpr left
            rExprRes <- genExpr right
            copy lExprRes rExprRes ty
        _ ->  do
            lExprRes <- genLExpr left
            rExprRes <- genExpr right
            let tyTAC = castTypeToTACType $ getAssignType right
            case lExprRes of
                RefSet addr ->
                    out $ simpleInstruction (DerefSet addr rExprRes tyTAC)
                ArraySet addr idx ->
                    out $ simpleInstruction (IndexSet addr idx rExprRes tyTAC)
                Simple addr ->
                    out $ simpleInstruction (NOp addr rExprRes tyTAC)

getAssignType :: AS.Exp SS.Ann -> T.Type
getAssignType = error "TODO: should be known by annotations"

data LExprRes
    = RefSet Address
    | ArraySet Address Address
    | Simple Address

genLExpr :: AS.Exp SS.Ann -> StateMonad LExprRes
genLExpr exp = case exp of
    AS.ExpIdent name _ _ -> do
        let identName = PName $ ProgramName (AS.identName name) "TODO: add routine name to variable"
        return $ Simple identName
    AS.Deref expr _ _ -> do
        addr <- genExpr expr
        return $ RefSet addr
    AS.ArrayIndex expr index _ _ -> do
        addr <- genExpr expr
        idx <- genExpr index
        return $ ArraySet addr idx
    _ -> error "Unreachable, error thrown while parsing"


