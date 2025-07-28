module TAC.Gen where

import qualified Control.Monad.Trans.State as St
import qualified AbsSyn                    as AS
import qualified StatSem.Utils             as SS
import qualified Types                     as T
import TAC.Def

type StateMonad = St.State TACState

data TACState = TACState {
    tempCount, labelCount :: Int,
    stream :: TAC
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

        AS.BinOp AS.Or e1 e2 ann _ -> error "TODO: handle Or operation separately, as it may short-circuit"

        AS.BinOp AS.And e1 e2 ann _ -> error "TODO: handle And operation separately, as it may short-circuit"

        AS.BinOp binOp e1 e2 ann _ -> do
            k <- newTmp
            -- Left to right evaluation
            addr1 <- genExpr e1
            addr2 <- genExpr e2
            let tacOp = castBinOpToTACOp binOp (SS.expType ann)
            out $ LabeledInstruction Nothing (BOp k tacOp addr1 addr2)
            return k

        AS.RelOp relOp e1 e2 _ _ -> genRelOp relOp e1 e2

        AS.Ref e _ _ -> do
            k <- newTmp
            addr <- genExpr e
            let progName = castAddressToProgramName addr
            out $ LabeledInstruction Nothing (Ref k progName)
            return k

        -- Already handled by genExpr
        AS.Literal {} -> error "Literal should not be passed to genUnknownExpr"

        -- Routine calls with multiple return values are handled only where they can appear
        AS.RoutCall call (SS.RoutAnn [_]) _ -> do
            addrs <- genRoutCall call
            -- TODO: In this case we always have a single address ?
            let addr = head addrs
            return addr

        AS.RoutCall {} -> error "Unreachable"

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
        AS.ExpIdent ident (SS.RoutAnn [_]) _ -> do
            addrs <- genRoutCall (AS.callFromIdent ident)
            -- TODO: In this case we always have a single address ?
            let addr = head addrs
            return addr

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

genRoutCall :: AS.RoutineCall SS.Ann -> StateMonad [Address]
genRoutCall = error "TODO: implement genRoutCall"

-- TODO: pay ATTENTION to relations between complex types
genRelOp :: AS.RelOp -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genRelOp op e1 e2 = let ann1 = AS.expAnnot e1
                        ann2 = AS.expAnnot e2
                    in
    case (ann1, ann2) of
    -- TODO: Other combinations are ruled out by static checks
    (SS.RoutAnn types, SS.RoutAnn _) -> genRelRout types e1 e2

    (SS.RoutAnn {}, _) -> do
        addrs <- genRoutCall (getRoutineCall e1)
        -- TODO: In this case we always have a single address ?
        let addr1 = head addrs
        addr2 <- genExpr e2
        genRelOp' op (SS.expType ann2) addr1 addr2

    (_, SS.RoutAnn {}) -> do
        addr1 <- genExpr e1
        addrs <- genRoutCall (getRoutineCall e2)
        -- TODO: In this case we always have a single address ?
        let addr2 = head addrs
        genRelOp' op (SS.expType ann1) addr1 addr2

    (_, _) -> do
        addr1 <- genExpr e1
        addr2 <- genExpr e2
        genRelOp' op (SS.expType ann1) addr1 addr2

    where
        getRoutineCall :: AS.Exp SS.Ann -> AS.RoutineCall SS.Ann
        getRoutineCall (AS.ExpIdent ident (SS.RoutAnn _) _) = AS.callFromIdent ident
        getRoutineCall (AS.RoutCall call _ _) = call
        getRoutineCall _ = error "Expected a routine call expression"

        genRelOp' :: AS.RelOp -> T.Type -> Address -> Address -> StateMonad Address
        genRelOp' = error "TODO: implement genRelOp'"

genRelRout :: [T.Type] -> AS.Exp SS.Ann -> AS.Exp SS.Ann -> StateMonad Address
genRelRout = error "TODO: implement genRelRout"


out :: LabeledInstruction -> StateMonad ()
out instr = do
    state <- St.get
    let newStream = instr : stream state
    St.put $ state { stream = newStream }

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


