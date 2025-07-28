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
    stream :: TAC,
    nextLabel :: Maybe Label
}

out :: LabeledInstruction -> StateMonad ()
out instr = do
    state <- St.get
    let instr' = case nextLabel state of
            Just label -> instr { label = Just label }
            Nothing    -> instr
        newStream = instr' : stream state
    St.put $ state { stream = newStream, nextLabel = Nothing }

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
    return $ Label k

attachToNext :: Label -> StateMonad ()
attachToNext label = do
    state <- St.get
    let newNext = Just label
    St.put $ state { nextLabel = newNext }

-- TAC generation functions

genDeclarations :: AS.Declarations SS.Ann -> StateMonad ()
genDeclarations (AS.ProgramDecl _ vars routines) = do
    mapM_ genVar vars
    mapM_ genRoutine routines

genVar :: AS.VarA SS.Ann -> StateMonad ()
genVar (AS.VarAss idents _ values _ _) =
    mapM_ genAssignment (zip idents values)
    where
    genAssignment :: (AS.Ident, AS.Exp SS.Ann) -> StateMonad ()
    genAssignment (ident, exp) = do
        addr <- genExpr exp
        out $ LabeledInstruction Nothing (NOp (PName $ ProgramName $ AS.identName ident) addr)

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

genString :: String -> StateMonad Address
genString _ = error "genString not implemented yet"

genUnknownExpr :: AS.Exp SS.Ann -> StateMonad Address
genUnknownExpr exp = do
    case exp of
        AS.BoolNeg e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out $ LabeledInstruction Nothing (UOp k BoolNeg addr)
            return addr

        AS.Sign e ann _ -> do
            k <- newTmp
            addr <- genExpr e
            let ty = castTypeToTACType $ SS.expType ann
            out $ LabeledInstruction Nothing (UOp k (Sign ty) addr)
            return addr

        -- TODO: This can be optimized (unnecessary temporary variables and consecutive OR and AND)
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
            out $ LabeledInstruction Nothing (BOp k tacOp addr1 addr2)
            return k

        AS.RelOp relOp e1 e2 _ _ -> genRepOp relOp e1 e2

        AS.Ref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out $ LabeledInstruction Nothing (Ref k addr)
            return k

        -- Already handled by genExpr
        AS.Literal {} -> error "Literal should not be passed to genUnknownExpr"

        -- Routine calls with multiple return values are handled only where they can appear
        AS.FunCall call (SS.ExpAnn {}) _ -> do
            genFunCall call

        AS.FunCall {} -> error "Unreachable"

        AS.Deref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            out $ LabeledInstruction Nothing (DerefGet k addr)
            return k

        AS.ArrayIndex e index _ _ -> do
            k <- newTmp
            addr <- genExpr e
            indexAddr <- genExpr index
            let elemSize = T.getTypeSize $ SS.expType $ AS.expAnnot e
            if elemSize == 1 then do
                out $ LabeledInstruction Nothing (IndexGet k addr indexAddr)
            else do
                -- If the element size is not 1, we need to multiply the index by the element size
                out $ LabeledInstruction Nothing (BOp indexAddr (Mul TInteger) indexAddr (Lit $ LitInt elemSize))
                out $ LabeledInstruction Nothing (IndexGet k addr indexAddr)
            
            return k

        -- ConstCategory is handled by genExpr
        AS.ExpIdent ident (SS.IdentAnn _ _ SS.VarCategory) _ -> do
                return $ PName (ProgramName $ AS.identName ident)

        -- Routine calls with multiple return values are handled only where they can appear
        AS.ExpIdent ident (SS.ExpAnn {}) _ -> do
            genFunCall (AS.callFromIdent ident)

        AS.ExpIdent {} -> error "Unreachable"

        AS.Array elements ann _ -> do
            k <- newTmp
            let arrSize  = T.getTypeSize $ SS.expType ann
                elemSize = T.getTypeSize $ T.elementsType $ SS.expType ann
            out $ LabeledInstruction Nothing (ArrayGen k arrSize)
            mapM_ 
                (\(i, e) -> do
                    addr <- genExpr e
                    out $ LabeledInstruction Nothing (IndexSet k (Lit $ LitInt (i * elemSize)) addr)) 
                (zip [0..] elements)

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
            out $ LabeledInstruction Nothing (UOp k (Coer cast) addr)
            return k

genBinExp :: AS.BinOp -> Address -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genBinExp op tmp e1 e2 = do
    addr1 <- genExpr' e1
    out $ LabeledInstruction Nothing (NOp tmp addr1)
    label <- newLabel
    let instr = case op of
            AS.Or -> BoolJumpF tmp label
            AS.And -> BoolJumpT tmp label
            _ -> error "Only Or and And operations should be passed to genBinExp"
    out $ LabeledInstruction Nothing instr
    addr2 <- genExpr' e2
    out $ LabeledInstruction Nothing (NOp tmp addr2)
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
    addr1 <- genExpr e1
    addr2 <- genExpr e2
    let ty = SS.expType $ AS.expAnnot e1
    case ty of
        T.TString -> error "TODO:"
        T.TArray {} -> error "TODO:"
        T.TReference {} -> error "TODO:"
        T.TError {} -> error "Unreachable"
        _ -> let tacRelOp = castRelOpToTACOp relOp ty
             in do
                out $ LabeledInstruction Nothing (BOp k tacRelOp addr1 addr2)
    return k

genFunCall :: AS.RoutineCall SS.Ann -> StateMonad Address
genFunCall = error "TODO: implement genFunCall"


genRoutine :: AS.Routine SS.Ann -> StateMonad ()
genRoutine = error "TODO: implement genRoutine"

