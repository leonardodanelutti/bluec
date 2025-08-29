module PrettyPrinter where

import Data.List (intercalate)
import AbsSyn
import Types
import Data.Char (toLower)

class Print a where
  prt :: a -> String

instance (Show a) => Print (Declarations a) where
  prt = printer

printer :: (Show a) => Declarations a -> String
printer = declarationsPrinter 0

-- Indentazione
indent :: Int -> String
indent n = replicate n '\t'

-- Pretty print delle dichiarazioni
declarationsPrinter :: (Show a) => Int -> Declarations a -> String
declarationsPrinter n (ProgramDecl consts vars routines _) =
    unlines $
        (if not (null consts)
            then [indent n ++ "const"] ++ map (constPrinter (n+1)) consts
            else []) ++
        (if not (null vars)
            then [indent n ++ "var"] ++ map (varPrinter (n+1)) vars
            else []) ++
        (if not (null routines)
            then [indent n ++ "routines"] ++ map (routinePrinter (n+1)) routines
            else [])
-- Pretty print di una costante
constPrinter :: (Show a) => Int -> ConstA a -> String
constPrinter n (ConstAss idents typ values _ _) =
    indent n ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter values ++ ";"

-- Pretty print di una variabile
varPrinter :: (Show a) => Int -> VarA a -> String
varPrinter n (VarAss idents typ values _ _) =
    indent n ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter values ++ ";"

-- Pretty print di una routine (funzione o procedura)
routinePrinter :: (Show a) => Int -> Routine a -> String
routinePrinter n (Function ident params returnType body _ _) =
    indent n ++ identPrinter ident ++ "(" ++ paramsPrint params ++ ") -> " ++ typePrinter returnType ++ " is\n"
    ++ routineBodyPrinter (n+1) body
    ++ indent (n+1) ++ "end\n"
    ++ indent n ++ identPrinter ident ++ "\n"
-- caso senza valori di ritorno 
routinePrinter n (Procedure ident params body _ _) =
    indent n ++ identPrinter ident ++ "(" ++ paramsPrint params ++ ") is\n"
    ++ routineBodyPrinter (n+1) body
    ++ indent (n+1) ++ "end\n"
    ++ indent n ++ identPrinter ident ++ "\n"


-- Corpo della routine
routineBodyPrinter :: Show a => Int -> RoutBody a -> String
routineBodyPrinter n (RoutBody decls statements _) =
    (if null (declarationsPrinter n decls) 
        then "" 
        else declarationsPrinter n decls) ++
    (if null statements
        then ""
        else indent n ++ "do\n" ++ statementsPrinter (n+1) statements)

-- Routine call
routineCallPrinter :: Show a => RoutineCall a -> String
routineCallPrinter (RoutineCall ident args _) =
    identPrinter ident ++ "(" ++ expsPrinter args ++ ")"


-- Espressioni
expPrinter :: Show a => Exp a -> String
expPrinter exp =
    case exp of
        Literal val _ _ -> literalPrinter val
        ExpIdent (Ident name _) _ _ -> name
        BoolNeg e _ _ -> "not " ++ expPrinter e
        Sign e _ _ -> "-" ++ expPrinter e
        BinOp op e1 e2 _ _ -> "(" ++ expPrinter e1 ++ " " ++ binOpPrinter op ++ " " ++ expPrinter e2 ++ ")"
        RelOp op e1 e2 _ _ -> "(" ++ expPrinter e1 ++ " " ++ relOpPrinter op ++ " " ++ expPrinter e2 ++ ")"
        Ref e _ _ -> "&" ++ expPrinter e
        Deref e _ _ -> "(*" ++ expPrinter e ++ ")"
        ArrayIndex e index _ _ -> expPrinter e ++ "[" ++ expPrinter index ++ "]"
        Array e _ _ -> "[" ++ expsPrinter e ++ "]"
        FunCall call _ _ -> routineCallPrinter call
        Cast e _ _ -> expPrinter e
        ArrayExpr e indexes _ _ -> "[" ++ expPrinter e ++ ": " ++ identsPrinter indexes ++ "]"
        IfElse cond thenBranch elseBranch _ _ ->
            "if " ++ expPrinter cond ++ " then " ++ expPrinter thenBranch ++ " else " ++ expPrinter elseBranch ++ " end if"

expsPrinter :: Show a => [Exp a] -> String
expsPrinter exps = intercalate ", " (map expPrinter exps)

-- Identificatori
identPrinter :: Ident -> String
identPrinter (Ident name _) = name

identsPrinter :: [Ident] -> String
identsPrinter idents = intercalate ", " (map identPrinter idents)

-- Parametri routine
paramPrinter :: Param -> String
paramPrinter (Param mode ident typ _) =
    modePrinter mode ++ identPrinter ident ++ " : " ++ typePrinter typ

paramsPrint :: [Param] -> String
paramsPrint params = intercalate ", " (map paramPrinter params)


-- Statements
statementPrinter :: Show a => Int -> Statement a -> String
statementPrinter n stm =
    case stm of
        StmtAss l r _ _ ->
            indent n ++ expsPrinter l ++ " := " ++ expsPrinter r ++ ";"
        StmtRoutCall call _ ->
            indent n ++ routineCallPrinter call ++ ";"
        StmtReturn vals _ _ ->
            indent n ++ "return " ++ expPrinter vals ++ ";"
        StmtReturnEmpty _ _ ->
            indent n ++ "return;"
        StmtBreak _ _ ->
            indent n ++ "break;"
        StmtContinue _ _ ->
            indent n ++ "continue;"
        StmtIf cond body _ _ ->
            indent n ++ "if " ++ expPrinter cond ++ " then\n"
            ++ statementsPrinter (n+1) body
            ++ indent n ++ "end if"
        StmtIfElse cond bodyT bodyF _ _ ->
            indent n ++ "if " ++ expPrinter cond ++ " then\n"
            ++ statementsPrinter (n+1) bodyT
            ++ indent n ++ "else\n"
            ++ statementsPrinter (n+1) bodyF
            ++ indent n ++ "end if"
        StmtLoop body exits _ _ ->
            indent n ++ "loop\n"
            ++ statementsPrinter (n+1) body
            ++ (if null exits then "" else exitsPrinter (n+1) exits)
            ++ indent n ++ "end loop"
        StmtForEach varIterName varIterType arrayExp body _ _ ->
            indent n ++ "for " ++ identPrinter varIterName ++ ": " ++ typePrinter varIterType
            ++ " in " ++ expPrinter arrayExp ++ " do\n"
            ++ statementsPrinter (n+1) body
            ++ indent n ++ "end for"
        StmtTryCatch try catch _ _ ->
            indent n ++ "try\n"
            ++ statementsPrinter (n+1) try
            ++ indent n ++ "catch\n"
            ++ statementsPrinter (n+1) catch
            ++ indent n ++ "end try"
        StmtThrow _ _ ->
            indent n ++ "throw error;"

statementsPrinter :: Show a => Int -> [Statement a] -> String
statementsPrinter n statements = unlines (map (statementPrinter n) statements)

-- Exits
exitPrinter :: Show a => Int -> Exit a -> String
exitPrinter n (LoopExit e body _ _) =
    indent n ++ "exit on " ++ expPrinter e ++ ";" ++
    if null body 
        then ""
        else "\n" ++ statementsPrinter n body

exitsPrinter :: Show a => Int -> [Exit a] -> String
exitsPrinter n exits = unlines (map (exitPrinter n) exits)

-- Tipi
typePrinter :: TypePos -> String
typePrinter (TypePos typ _) =
    case typ of
        TBoolean -> "Boolean"
        TInteger -> "Integer"
        TReal    -> "Real"
        TString  -> "String"
        TChar    -> "Char"
        TArray elemType size ->
            "Array<" ++ typePrinter (TypePos elemType undefined) ++ " ; " ++ show size ++ ">"
        TReference refType ->
            "&" ++ typePrinter (TypePos refType undefined)
        TError msgs -> "Error: " ++ intercalate ", " msgs

-- Letterali
literalPrinter :: Literal -> String
literalPrinter l =
    case l of
        LitBool b _  -> map toLower (show b)
        LitInt i _   -> show i
        LitDouble r _-> show r
        LitString s _ -> show s
        LitChar c _   ->  show c

-- Operatori binari
binOpPrinter :: BinOp -> String
binOpPrinter op =
    case op of
        Add    -> "+"
        Sub    -> "-"
        Mul    -> "*"
        Div    -> "/"
        DivInt -> "div"
        Mod    -> "mod"
        Pow    -> "^"
        And    -> "and"
        Or     -> "or"

-- Operatori relazionali
relOpPrinter :: RelOp -> String
relOpPrinter op =
    case op of
        Eq  -> "="
        Neq -> "<>"
        Lt  -> "<"
        Le  -> "<="
        Gt  -> ">"
        Ge  -> ">="


-- Modalità di passaggio parametri
modePrinter :: Mode -> String
modePrinter m =
    case m of
        Mval    -> ""
        Mref    -> "ref "
        Mvalres -> "valres "
        Mconst  -> "const "