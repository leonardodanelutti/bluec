module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified StatSem.Utils             as SS
import qualified Types                     as T
import TAC.Def

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCount :: Int,
    stream :: TAC,
    nextLabel :: Maybe Label
}

-- genDeclarations :: AS.Declarations SS.Ann -> StateMonad ()

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
            addr1 <- genExpr e1
            out $ LabeledInstruction Nothing (NOp k addr1)
            label <- newLabel
            out $ LabeledInstruction Nothing (BoolJumpT k label)
            addr2 <- genExpr e2
            out $ LabeledInstruction Nothing (NOp k addr2)
            attachToNext label
            return k

        AS.BinOp AS.And e1 e2 _ _ -> do
            k <- newTmp
            addr1 <- genExpr e1
            out $ LabeledInstruction Nothing (NOp k addr1)
            label <- newLabel
            out $ LabeledInstruction Nothing (BoolJumpF k label)
            addr2 <- genExpr e2
            out $ LabeledInstruction Nothing (NOp k addr2)
            attachToNext label
            return k

        AS.BinOp binOp e1 e2 ann _ -> do
            k <- newTmp
            -- Left to right evaluation
            addr1 <- genExpr e1
            addr2 <- genExpr e2
            let tacOp = castBinOpToTACOp binOp (SS.expType ann)
            out $ LabeledInstruction Nothing (BOp k tacOp addr1 addr2)
            return k

        AS.RelOp relOp e1 e2 _ _ -> do
            addr1 <- genExpr e1
            addr2 <- genExpr e2
            let ty = SS.expType $ AS.expAnnot e1
            genRelOp relOp ty addr1 addr2

        AS.Ref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            let progName = castAddressToProgramName addr
            out $ LabeledInstruction Nothing (Ref k progName)
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
            let progName = castAddressToProgramName addr
            out $ LabeledInstruction Nothing (IndexGet k progName indexAddr)
            return k

        -- ConstCategory is handled by genExpr
        AS.ExpIdent ident (SS.IdentAnn _ _ SS.VarCategory) _ -> do
                return $ PName (ProgramName $ AS.identName ident)

        -- Routine calls with multiple return values are handled only where they can appear
        AS.ExpIdent ident (SS.ExpAnn {}) _ -> do
            genFunCall (AS.callFromIdent ident)

        AS.ExpIdent {} -> error "Unreachable"

        AS.Array elements ann _ -> error "TODO: How can we initialize an array in TAC?"

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

genFunCall :: AS.RoutineCall SS.Ann -> StateMonad Address
genFunCall = error "TODO: implement genRoutCall"

-- TODO: pay ATTENTION to relations between complex types
genRelOp :: AS.RelOp -> T.Type -> Address -> Address -> StateMonad Address
genRelOp = error "TODO: implement genRelOp"


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

