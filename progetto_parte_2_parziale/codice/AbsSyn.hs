-- | The abstract syntax of language grammar.
module AbsSyn where

import Data.String qualified
import GHC.Err (error)
import GHC.Internal.Show qualified
import Prelude (Bool, Char, Double, Integer, Show, String)
import LexGrammar (Posn)

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
    constLoc :: Posn
  }
  deriving (Show)

data VarA a = VarAss
  { varIdents :: [Ident],
    varType :: TypePos,
    varValues :: [Exp a],
    varAnnot :: a,
    varLoc :: Posn
  }
  deriving (Show)

data Routine a = Routine
  { routName :: Ident,
    params :: [Param],
    returnTypes :: [TypePos],
    routBody :: RoutBody a,
    routAnnot :: a,
    routLoc :: Posn
  }
  deriving (Show)

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
  deriving (Show)

-- TODO: Cosi non possiamo sapere la posizione dei tipi più interni
data TypePos = TypePos
  { type_ :: Type,
    typePosn :: Posn
  }
  deriving (Show)

data Param = Param
  { mode :: Mode,
    paramName :: Ident,
    paramType :: TypePos,
    paramLoc :: Posn
  }
  deriving (Show)

data Mode = Mval | Mvalres | Mref
  deriving (Show)

data RoutBody a = RoutBody
  { declarations :: Declarations a,
    statements :: [Statement a]
  }
  deriving (Show)

data RoutineCall a = RoutineCall
  { routCallName :: Ident,
    args :: [Exp a]
  }
  deriving (Show)

data Statement a
  = StmtAss {left :: [Exp a], right :: [Exp a], annot :: a, loc :: Posn}
  | StmtRoutCall {call :: RoutineCall a, annot :: a, loc :: Posn}
  | StmtReturn {vals :: [Exp a], annot :: a, loc :: Posn}
  | StmtContinue {annot :: a, loc :: Posn}
  | StmtIf {cond :: Exp a, body :: [Statement a], annot :: a, loc :: Posn}
  | StmrIfElse {cond :: Exp a, bodyT :: [Statement a], bodyF :: [Statement a], annot :: a, loc :: Posn}
  | StmtLoop {body :: [Statement a], exit :: [Exit a], annot :: a, loc :: Posn}
  deriving (Show)

data Exit a = LoopExit {loopCond :: Exp a, loopBody :: [Statement a], loopExitAnnot :: a, loopExitLoc :: Posn}
  deriving (Show)

data Exp a
  = BoolAnd {e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Posn}
  | BoolOr {e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Posn}
  | BoolNeg {e :: Exp a, expAnnot :: a, expLoc :: Posn}
  | Sign {e :: Exp a, expAnnot :: a, expLoc :: Posn}
  | BinOp {binOp :: BinOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Posn}
  | RelOp {relOp :: RelOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Posn}
  | Ref {e :: Exp a, expAnnot :: a, expLoc :: Posn}
  | Literal {val :: Literal, expAnnot :: a, expLoc :: Posn}
  | RoutCall {exprCall :: RoutineCall a, expAnnot :: a, expLoc :: Posn}
  | Deref {e :: Exp a, expAnnot :: a, expLoc :: Posn}
  | ArrayIndex {e :: Exp a, index :: Exp a, expAnnot :: a, expLoc :: Posn}
  | Var {name :: Ident, expAnnot :: a, expLoc :: Posn}
  | Array {elements :: [Exp a], expAnnot :: a, expLoc :: Posn}
  deriving (Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | DivInt
  | Mod
  | Pow
  deriving (Show)

data RelOp
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Show)

data Literal
  = LitInt Integer
  | LitChar Char
  | LitString String
  | LitBool Bool
  | LitDouble Double
  deriving (Show)

getInt :: Literal -> Integer
getInt (LitInt i) = i
getInt _ = error "Expected an integer literal"

newtype Ident = Ident String
  deriving (Data.String.IsString, Show)

class Print a where
  prt :: a -> String

-- TODO: pretty printer
instance (Show a) => Print (Declarations a) where
  prt = prettyPrinter

prettyPrinter :: (Show a) => Declarations a -> String
prettyPrinter _ = "TODO:"
