{-# LANGUAGE LambdaCase #-}
module TAC.Printer where

import TAC.Def 

printProgram :: TAC -> String
printProgram instructions = unlines $ map show instructions


instance Show LabeledInstruction where
    show (LabeledInstruction [] inst) = "      " ++ show inst
    show (LabeledInstruction [label] inst) = 
        let labelStr = case label of
                Loc n -> "L" ++ show n ++ ":"
                Function name -> "\n" ++ name ++ ":\n      " 
        in labelStr ++ replicate (6 - length labelStr) ' ' ++ show inst
    show (LabeledInstruction labels inst) = 
        let labelStr = concatMap (\case
                Loc n -> "L" ++ show n ++ ", "
                Function name -> name ++ ", ") labels
        in labelStr ++ ": " ++ show inst

instance Show Instruction where
    show inst = case inst of
        BOp l op r1 r2 ty ->
            show l ++ " =(" ++ show ty ++ ") " ++ show r1 ++ " " ++ show op ++ " " ++ show r2
        UOp l op r ty ->
            show l ++ " =(" ++ show ty ++ ") " ++ show op ++ " " ++ show r
        NOp l r ty ->
            show l ++ " =(" ++ show ty ++ ") " ++ show r
        UnJump to ->
            "UnJump to: " ++ show to
        BoolJumpT cond to ->
            "if " ++ show cond ++ " goto " ++ show to
        BoolJumpF cond to ->
            "ifFalse " ++ show cond ++ " goto " ++ show to
        RelJump rel r1 r2 to ->
            "if " ++ show r1 ++ " " ++ show rel ++ " " ++ show r2 ++ " goto " ++ show to
        IndexGet l id idx ty ->
            show l ++ " =(" ++ show ty ++ ") " ++ show id ++ "[" ++ show idx ++ "]"
        IndexSet id idx r ty ->
            show id ++ "[" ++ show idx ++ "] =(" ++ show ty ++ ") " ++ show r
        Ref l id ->
            show l ++ " = &" ++ show id
        DerefGet l1 l2 ty ->
            show l1 ++ " =(" ++ show ty ++ ") *" ++ show l2
        DerefSet l r ty ->
            "*" ++ show l ++ " =(" ++ show ty ++ ") " ++ show r
        Param r ty ->
            "param " ++ "(" ++ show ty ++ ") " ++ show r
        ProcCall rout n ->
            "pcall " ++ show rout ++ ", " ++ show n
        FunCall l rout n ty ->
            show l ++ " =(" ++ show ty ++ ") " ++ "fcall " ++ show rout ++ ", " ++ show n
        ReturnEmpty ->
            "return"
        Return r ->
            "return " ++ show r
        ArrayGen id size ->
            "arraygen " ++ show id ++ ", " ++ show size
        Comment str ->
            "# " ++ str

instance Show Address where
    show (PName pName) = show pName
    show (Temp tmp) = show tmp
    show (Lit lit) = show lit

instance Show TACType where
    show TInteger = "int"
    show TReal = "float"
    show TChar = "char"
    show TBoolean = "bool"
    show Addr = "addr"

instance Show BinOp where
    show (Add ty) = "+(" ++ show ty  ++ ")"
    show (Sub ty) = "-(" ++ show ty  ++ ")"
    show (Mul ty) = "*(" ++ show ty  ++ ")"
    show Div = "/(float)"
    show DivInt = "/(int)"
    show (Mod) = "%(int)"
    show (Pow ty) = "^(" ++ show ty  ++ ")"
    show And = "&&"
    show Or = "||"
    show (Rel relOp) = show relOp

instance Show RelOp where
    show (RelEq ty) = "==(" ++ show ty ++ ")"
    show (RelNeq ty) = "!=(" ++ show ty ++ ")"
    show (RelLt ty) = "<(" ++ show ty ++ ")"
    show (RelLe ty) = "<=(" ++ show ty ++ ")"
    show (RelGt ty) = ">(" ++ show ty ++ ")"
    show (RelGe ty) = ">=(" ++ show ty ++ ")"

instance Show UnaryOp where
    show BoolNeg = "!"
    show (Sign ty) = "-(" ++ show ty ++ ")"
    show (Coer c) = show c

instance Show Coercion where
    show BoolToInt = "boolToInt"
    show IntToDouble = "intToDouble"
    show CharToInt = "charToInt"

instance Show Label where
    show (Loc n) = "L" ++ show n
    show (Function name) = name

instance Show Temporary where
    show (Temporary n) = "t" ++ show n

instance Show ProgramName where
    show (ProgramName name procedure) = name ++ "." ++ procedure

instance Show Literal where
    show (LitInt n) = show n
    show (LitReal n) = show n
    show (LitChar c) = "'" ++ [c] ++ "'"
    show (LitBool s) = show s
