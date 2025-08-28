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
printer = declarationsPrinter

-- Pretty print delle dichiarazioni
declarationsPrinter :: (Show a) => Declarations a -> String
declarationsPrinter (ProgramDecl consts vars routines _) =
    unlines $
        (if not (null consts)
            then "const" : map constPrinter consts
            else []) ++
        (if not (null vars)
            then "var" : map varPrinter vars
            else []) ++
        (if not (null routines)
            then "routines" : map routinePrinter routines
            else [])


-- Pretty print di una costante
constPrinter :: (Show a) => ConstA a -> String
constPrinter (ConstAss idents typ values _ _) =
    "\t" ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter values ++ ";"


-- Pretty print di una variabile
varPrinter :: (Show a) => VarA a -> String
varPrinter (VarAss idents typ values _ _) =
        "\t" ++ identsPrinter idents ++ " : " ++ typePrinter typ ++ " := " ++ expsPrinter values ++ ";"


-- Pretty print di una routine (funzione o procedura)
routinePrinter :: (Show a) => Routine a -> String
routinePrinter (Function ident params returnType body _ _) =
    unlines $
        [ "\t" ++ identPrinter ident ++ "(" ++ paramsPrint params ++ ") -> " ++ typePrinter returnType ++ " is" ] ++ 
        map ("\t" ++) (lines $ routineBodyPrinter body) ++ ["\tend\n\t" ++ identPrinter ident]
routinePrinter (Procedure ident params body _ _) =
    unlines $
        [ "\t" ++ identPrinter ident ++ "(" ++ paramsPrint params ++ ") is" ] ++ 
        map ("\t" ++) (lines $ routineBodyPrinter body) ++ ["\tend\n\t" ++ identPrinter ident]


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

expsPrinter :: Show a => [Exp a] -> String
expsPrinter exps = intercalate ", " (map expPrinter exps)



-- Identificatori
identPrinter :: Ident -> String
identPrinter (Ident name _) = name

identsPrinter :: [Ident] -> String
identsPrinter idents = intercalate ", " (map identPrinter idents)

paramPrinter :: Param -> String
paramPrinter (Param mode ident typ _) =
    modePrinter mode ++ identPrinter ident ++ " : " ++ typePrinter typ

paramsPrint :: [Param] -> String
paramsPrint params = intercalate ", " (map paramPrinter params)

exitPrinter :: Show a => Exit a -> String
exitPrinter (LoopExit e body _ _) =
  "exit on " ++ expPrinter e ++ ";" ++
  if (null body) then ""
    else "\n\t" ++ statementsPrinter body

exitsPrinter :: Show a =>[Exit a] -> String
exitsPrinter exits = intercalate "\n" (map exitPrinter exits)

routineCallPrinter :: Show a => RoutineCall a -> String
routineCallPrinter (RoutineCall ident args _) =
    identPrinter ident ++ "(" ++ expsPrinter args ++ ")"

caseBranchPrinter :: Show a => CaseBranch a -> String
caseBranchPrinter CaseBranch{pattern, branchBody} = 
    "when " ++ expPrinter pattern ++ ":\n\t" ++ statementsPrinter branchBody

caseBranchesPrinter :: Show a => [CaseBranch a] -> String
caseBranchesPrinter caseBranches =
    intercalate "\n" (map caseBranchPrinter caseBranches)

-- Corpo della routine
routineBodyPrinter :: Show a => RoutBody a -> String
routineBodyPrinter (RoutBody decls statements _) =
    unlines $
        lines (declarationsPrinter decls) ++
        (if not (null statements)
            then ["do", statementsPrinter statements]
            else [])


statementsPrinter :: Show a => [Statement a] -> String
statementsPrinter statements =
    intercalate "\n" (map statementPrinter statements)

statementPrinter :: Show a => Statement a -> String
statementPrinter stm = "\t" ++
    case stm of
        StmtAss l r _ _ -> expsPrinter l ++ " := " ++ expsPrinter r ++ ";"
        StmtRoutCall call _ -> routineCallPrinter call ++ ";" -- inserisci routinePrinter che prende solo la RoutineCall
        StmtReturn vals _ _ -> "return " ++ expPrinter vals ++ ";"
        StmtReturnEmpty _ _ -> "return;"
        StmtContinue _ _ -> "continue;"
        StmtIf cond body _ _ -> "if " ++ expPrinter cond ++ " then\n\t" ++ statementsPrinter body ++ "\n\tend if"
        StmtIfElse cond bodyT bodyF _ _ ->
            "if " ++ expPrinter cond ++ " then\n\t" ++ statementsPrinter bodyT ++
            "\n\telse\n\t" ++ statementsPrinter bodyF ++ "\n\tend if"
        StmtCase scrutinee cases def _ _ ->
            let defPrint = case def of
                    Just statements -> "\nelse\n\t" ++ statementsPrinter statements
                    Nothing -> ""
            in
            "case " ++ expPrinter scrutinee ++ "\n\t" ++ caseBranchesPrinter cases ++ defPrint ++ "\nend case"
        StmtLoop body exits _ _ ->
            "loop\n\t" ++ statementsPrinter body ++
            (if null exits then "" else "\n\t" ++ exitsPrinter exits) ++ "\n\tend loop"


-- Tipi
-- data Type
--  = TBoolean
--  | TInteger
--  | TReal
--  | TString
--  | TChar
--  | TArray
--      { elementsType :: Type,
--        size :: Integer
--      }
--  | TReference
--      { refType :: Type
--      }
--  | TError { msgs :: [String] }
--  deriving (Show, Eq)
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

-- Operatori
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

relOpPrinter :: RelOp -> String
relOpPrinter op =
    case op of
        Eq  -> "="
        Neq -> "<>"
        Lt  -> "<"
        Le  -> "<="
        Gt  -> ">"
        Ge  -> ">="


-- Passaggio parametri
modePrinter :: Mode -> String
modePrinter m =
    case m of
        Mval    -> ""
        Mref    -> "ref "
        Mvalres -> "valres "