-- | The abstract syntax of language grammar.
module AbsSyn where

import Data.String qualified
import GHC.Err (error)
import GHC.Internal.Show qualified
import LexGrammar (Posn)
import Prelude (Bool, Char, Double, Integer, Show, String, Eq, Ord)

data Declarations a = ProgramDecl
  { consts :: [ConstA a],
    vars :: [VarA a],
    routines :: [Routine a]
  }
  deriving (Show)

data ConstA a = ConstAss
  { constIdents :: [Ident],
    constType :: TypePos,
    constValues :: [Exp a],
    constAnnot :: a,
    constPos :: Posn
  }
  deriving (Show)

data VarA a = VarAss
  { varIdents :: [Ident],
    varType :: TypePos,
    varValues :: [Exp a],
    varAnnot :: a,
    varPos :: Posn
  }
  deriving (Show)

data Routine a = Routine
  { routName :: Ident,
    params :: [Param],
    returnTypes :: [TypePos],
    routBody :: RoutBody a,
    routAnnot :: a,
    routPos :: Posn
  }
  deriving (Show)

data Ident = Ident
  { identName :: String,
    identPos :: Posn
  }
  deriving (Show, Eq, Ord)

data Type
  = TBoolean
  | TInteger
  | TReal
  | TString
  | TChar
  | TArray
      { elementsType :: Type,
        size :: Integer
      }
  | TReference
      { refType :: Type
      }
  | TError { msgs :: [String] }
  deriving (Show)

-- TODO: Cosi non possiamo sapere la posizione dei tipi più interni
data TypePos = TypePos
  { type_ :: Type,
    typePosn :: Posn
  }
  deriving (Show)

data Exp a
  = BoolAnd    { e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expPos :: Posn }
  | BoolOr     { e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expPos :: Posn }
  | BoolNeg    { e :: Exp a, expAnnot :: a, expPos :: Posn }
  | Sign       { e :: Exp a, expAnnot :: a, expPos :: Posn }
  | BinOp      { binOp :: BinOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expPos :: Posn }
  | RelOp      { relOp :: RelOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expPos :: Posn }
  | Ref        { e :: Exp a, expAnnot :: a, expPos :: Posn }
  | Literal    { val :: Literal, expAnnot :: a, expPos :: Posn }
  | RoutCall   { expCall :: RoutineCall a, expAnnot :: a, expPos :: Posn }
  | Deref      { e :: Exp a, expAnnot :: a, expPos :: Posn }
  | ArrayIndex { e :: Exp a, index :: Exp a, expAnnot :: a, expPos :: Posn }
  | ExpIdent   { name :: Ident, expAnnot :: a, expPos :: Posn }
  | Array      { elements :: [Exp a], expAnnot :: a, expPos :: Posn }
  deriving (Show)

data BinOp = Add | Sub | Mul | Div | DivInt | Mod | Pow
  deriving (Show)

data RelOp = Eq | Neq | Lt | Le | Gt | Ge
  deriving (Show)

data Param = Param
  { mode :: Mode,
    paramName :: Ident,
    paramType :: TypePos,
    paramPos :: Posn
  }
  deriving (Show)

data Mode = Mval | Mvalres | Mref
  deriving (Show)

data RoutBody a = RoutBody
  { declarations :: Declarations a,
    statements :: [Statement a]
  }
  deriving (Show)

data Statement a
  = StmtAss      { left :: [Exp a], right :: [Exp a], annot :: a, pos :: Posn }
  | StmtRoutCall { call :: RoutineCall a, annot :: a }
  | StmtReturn   { vals :: [Exp a], annot :: a, pos :: Posn }
  | StmtContinue { annot :: a, pos :: Posn }
  | StmtIf       { cond :: Exp a, body :: [Statement a], annot :: a, pos :: Posn }
  | StmrIfElse   { cond :: Exp a, bodyT :: [Statement a], bodyF :: [Statement a], annot :: a, pos :: Posn }
  | StmtLoop     { body :: [Statement a], exit :: [Exit a], annot :: a, pos :: Posn }
  deriving (Show)

data RoutineCall a = RoutineCall
  { routCallName :: Ident,
    args :: [Exp a],
    routCallPos :: Posn
  }
  deriving (Show)

data Exit a = LoopExit
  { loopCond :: Exp a,
    loopBody :: [Statement a],
    loopExitAnnot :: a,
    loopExitPos :: Posn
  }
  deriving (Show)

data Literal
  = LitInt    { intVal    :: Integer, litPos :: Posn }
  | LitChar   { charVal   :: Char,    litPos :: Posn }
  | LitString { stringVal :: String,  litPos :: Posn }
  | LitBool   { boolVal   :: Bool,    litPos :: Posn }
  | LitDouble { doubleVal :: Double,  litPos :: Posn }
  deriving (Show)

getInt :: Literal -> Integer
getInt (LitInt i _) = i
getInt _ = error "Expected an integer literal"

class Print a where
  prt :: a -> String

-- TODO: pretty printer
instance (Show a) => Print (Declarations a) where
  prt = prettyPrinter

prettyPrinter :: (Show a) => Declarations a -> String
prettyPrinter _ = "TODO:"
