module PrettyPrinter where

import LexGrammar (Posn(..), PosnS(..))
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

-- Controllo inizio e terminazione di un nodo
getStartLine :: Posn -> Int
getStartLine (Pn (PnS _ l _) _) = l

getEndLine :: Posn -> Int
getEndLine (Pn _ (PnS _ l _)) = l

sameLine :: Posn -> Bool
sameLine pos = getStartLine pos == getEndLine pos

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
    indent n ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter (n+1) values ++ ";"

-- Pretty print di una variabile
varPrinter :: (Show a) => Int -> VarA a -> String
varPrinter n (VarAss idents typ values _ _) =
    indent n ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter (n+1) values ++ ";"

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
routineCallPrinter :: Show a => Int -> RoutineCall a -> String
routineCallPrinter n (RoutineCall ident args _) =
    identPrinter ident ++ "(" ++ expsPrinter (n+1) args ++ ")"


-- Espressioni
expPrinter :: Show a => Int -> Exp a -> String
expPrinter n exp =
    case exp of
        Literal val _ _ -> literalPrinter val
        ExpIdent (Ident name _) _ _ -> name
        BoolNeg e _ _ -> "not " ++ expPrinter n e
        Sign e _ _ -> "-" ++ expPrinter n e
        BinOp op e1 e2 _ _ -> "(" ++ expPrinter n e1 ++ " " ++ binOpPrinter op ++ " " ++ expPrinter n e2 ++ ")"
        RelOp op e1 e2 _ _ -> "(" ++ expPrinter n e1 ++ " " ++ relOpPrinter op ++ " " ++ expPrinter n e2 ++ ")"
        Ref e _ _ -> "&" ++ expPrinter n e
        Deref e _ _ -> "(*" ++ expPrinter n e ++ ")"
        ArrayIndex e index _ _ -> expPrinter n e ++ "[" ++ expPrinter n index ++ "]"
        Array e _ _ -> "[" ++ expsPrinter (n+1) e ++ "]"
        FunCall call _ _ -> routineCallPrinter n call
        Cast e _ _ -> expPrinter n e
        ArrayExpr e indexes _ _ -> "[" ++ expPrinter n e ++ ": " ++ identsPrinter indexes ++ "]"
        -- IfElse
        IfElse cond thenBranch elseBranch _ pos ->
            if (sameLine pos)
                then "if " ++ expPrinter n cond ++ " then " ++ expPrinter n thenBranch ++ " else " ++ expPrinter n elseBranch ++ " end if"
                else "if " ++ expPrinter n cond ++ "\n"
                    ++ indent n ++ "then\n"
                    ++ indent (n+1) ++ expPrinter n thenBranch ++ "\n"
                    ++ indent n ++ "else\n"
                    ++ indent (n+1) ++ expPrinter n elseBranch ++ "\n"
                    ++ indent n ++ "end if"

expsPrinter :: Show a => Int -> [Exp a] -> String
expsPrinter n exps = intercalate ", " (map (expPrinter n) exps)

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
            indent n ++ expsPrinter (n+1) l ++ " := " ++ expsPrinter n r ++ ";"
        StmtRoutCall call _ ->
            indent n ++ routineCallPrinter (n+1) call ++ ";"
        StmtReturn vals _ _ ->
            indent n ++ "return " ++ expPrinter (n+1) vals ++ ";"
        StmtReturnEmpty _ _ ->
            indent n ++ "return;"
        StmtBreak _ _ ->
            indent n ++ "break;"
        StmtContinue _ _ ->
            indent n ++ "continue;"
        StmtIf cond body _ _ ->
            indent n ++ "if " ++ expPrinter (n+1) cond ++ " then\n"
            ++ statementsPrinter (n+1) body
            ++ indent n ++ "end if"
        StmtIfElse cond bodyT bodyF _ _ ->
            indent n ++ "if " ++ expPrinter (n+1) cond ++ " then\n"
            ++ statementsPrinter (n+1) bodyT
            ++ indent n ++ "else\n"
            ++ statementsPrinter (n+1) bodyF
            ++ indent n ++ "end if"
        StmtLoop body exits _ _ ->
            indent n ++ "loop\n"
            ++ statementsPrinter (n+1) body
            ++ (if null exits then "" else exitsPrinter (n+1) exits)
            ++ indent n ++ "end loop"
        StmtForEach varIterName arrayExp body _ _ ->
            indent n ++ "for " ++ identPrinter varIterName
            ++ " in " ++ expPrinter (n+1) arrayExp ++ " do\n"
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
    indent n ++ "exit on " ++ expPrinter (n+1) e ++ ";" ++
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