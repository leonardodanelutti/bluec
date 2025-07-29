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
    return $ Loc k

attachToNext :: Label -> StateMonad ()
attachToNext label = do
    state <- St.get
    let newNext = Just label
    St.put $ state { nextLabel = newNext }

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
        out $ LabeledInstruction Nothing (NOp (PName $ ProgramName $ AS.identName ident) addr tyTAC)

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

        -- TODO: Quando DG mette le modalità di passaggio devo cambiare annotazione?
        AS.FunCall call ann@(SS.ExpAnn {}) _ -> do
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
        AS.ExpIdent ident (SS.IdentAnn _ _ SS.VarCategory) _ -> do
                return $ PName (ProgramName $ AS.identName ident)

        -- TODO: Quando DG mette le modalità di passaggio devo cambiare annotazione?
        AS.ExpIdent ident ann@(SS.ExpAnn {}) _ -> do
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
        T.TError {} -> error "Unreachable"
        _ -> let tacRelOp = castRelOpToTACOp relOp ty
             in do
                out $ simpleInstruction (BOp k tacRelOp addr1 addr2 TBoolean)
    return k

genFunCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad Address
genFunCall (AS.RoutineCall routName args _) ann = do
    k <- newTmp
    genParams args ann
    let returnType = SS.expType ann
        routLab = Function $ AS.identName routName
        numArgs = length args
        ty = castTypeToTACType returnType
    case returnType of
        T.TArray {} -> error "TODO: handle array return types"
        -- Ref cannot be returned by a function
        _ -> out $ simpleInstruction (FunCall k routLab numArgs ty)
    return k

genProcCall :: AS.RoutineCall SS.Ann -> SS.Ann -> StateMonad ()
genProcCall (AS.RoutineCall routName args _) ann = do
    genParams args ann
    let routLab = Function $ AS.identName routName
        numArgs = length args
    out $ simpleInstruction (ProcCall routLab numArgs)

-- copiare tutte le variabili con i nomi dei parametri locali
-- modalità di passaggio: se ref o valres crea una reference
-- se il tipo di ritorno è un array (o una stringa) devi crearlo prima e passarlo come riferimento
genParams ::  [AS.Exp SS.Ann] -> SS.Ann -> StateMonad ()
genParams args ann = 
    let params = getParams ann
        exprs = zip args params
    in
        mapM_ (\(expr, param) -> do
            let paramName = AS.identName $ AS.paramName param
                paramAddr = PName $ ProgramName paramName
                paramType = T.type_ $ AS.paramType param
                paramTACType = castTypeToTACType paramType

            addr <- genExpr expr

            case AS.mode param of
                AS.Mval -> do
                    copy paramAddr addr paramType
                    out $ simpleInstruction (Param paramAddr paramTACType)
                -- Create a reference for value-result and reference
                _ -> do
                    out $ simpleInstruction (Ref paramAddr addr)
                    out $ simpleInstruction (Param paramAddr paramTACType)
        ) exprs

getParams :: SS.Ann -> [AS.Param]
getParams = error "TODO: replace this when params are added to the annotation of RoutineCall"

copy :: Address -> Address -> T.Type -> StateMonad ()
copy newAddr prevAddr ty = case ty of
    T.TString -> error "TODO: implement copy for strings"
    T.TArray elemType len -> do
        out $ simpleInstruction (ArrayGen newAddr (T.getTypeSize ty))
        copyArray newAddr prevAddr elemType len
        where
            copyArray :: Address -> Address -> T.Type -> Int -> StateMonad ()
            copyArray newAddr prevAddr elemType len = 
                let elemSize = T.getTypeSize ty 
                    elemTACTy = castTypeToTACType elemType
                in
                mapM_ (\i -> do
                    k <- newTmp
                    out $ simpleInstruction (IndexGet k prevAddr (Lit $ LitInt (i * elemSize)) elemTACTy)
                    copy' newAddr k elemType
                )
                [0..len] 
            
            copy' :: Address -> Address -> T.Type -> StateMonad ()
            copy' newAddr prevAddr (T.TArray elemType len) = copyArray newAddr prevAddr elemType len
            copy' a b c = copy a b c

    _ -> do
        out $ simpleInstruction (NOp newAddr prevAddr (castTypeToTACType ty))


genRoutine :: AS.Routine SS.Ann -> StateMonad ()
genRoutine = error "TODO: implement genRoutine"

