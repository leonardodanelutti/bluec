-- | The abstract syntax of language grammar.

module AbsSyn where

import Prelude (Char, Double, Integer, String, Bool)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Pos = Pos {
    fstLine, lstLine, fstCol, lstCol :: Integer
}

def = Pos 0 0 0 0

data Declarations a = ProgramDecl {
    consts :: [ConstA a],
    vars :: [VarA a],
    routines :: [Routine a]
}

data ConstA a = ConstAss {
    constIdents :: [Ident], 
    constType :: TypePos, 
    constValues :: [Exp a],
    constAnnot :: a,
    constLoc :: Pos
}

data VarA a = VarAss {
    varIdents :: [Ident], 
    varType :: TypePos,
    varValues :: [Exp a],
    varAnnot :: a,
    varLoc :: Pos
}

data Routine a = Routine {
    routName :: Ident,
    params :: [Param], 
    returnTypes :: [TypePos],
    routBody :: RoutBody a,
    routAnnot :: a,
    routLoc :: Pos
}

data Type
    = TBoolean
    | TInteger
    | TReal
    | TString
    | TChar
    | TArray {
      elementsType :: Type,
      size :: Integer
    }
    | TReference {
      refType :: Type
}

-- TODO: Cosi non possiamo sapere la posizione dei tipi più interni
data TypePos = TypePos {
    type_ :: Type,
    typePos :: Pos
}

data Param = Param {
  mode :: Mode,
  paramName :: Ident,
  paramType :: TypePos,
  paramLoc :: Pos
}

data Mode = Mval | Mvalres | Mref

data RoutBody a = RoutBody {
  declarations :: Declarations a,
  statements :: [Statement a]
}

data RoutineCall a = RoutineCall {
  routCallName :: Ident,
  args :: [Exp a]
}

data Statement a
    = StmtAss {left :: [Exp a], right :: [Exp a], annot :: a, loc :: Pos}
    | StmtRoutCall {call :: RoutineCall a, annot :: a, loc :: Pos}
    | StmtReturn {vals :: [Exp a], annot :: a, loc :: Pos}
    | StmtContinue {annot :: a, loc :: Pos}
    | StmtIf {cond :: Exp a, body :: [Statement a], annot :: a, loc :: Pos}
    | StmrIfElse {cond :: Exp a, bodyT :: [Statement a], bodyF :: [Statement a], annot :: a, loc :: Pos}
    | StmtLoop {body :: [Statement a], exit :: [Exit a], annot :: a, loc :: Pos}

data Exit a = LoopExit {loopCond :: Exp a, loopBody :: [Statement a], loopExitAnnot :: a, loopExitLoc :: Pos}

data Exp a
  = BoolAnd {e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Pos}
  | BoolOr {e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Pos}
  | BoolNeg {e :: Exp a, expAnnot :: a, expLoc :: Pos}
  | Sign {e :: Exp a, expAnnot :: a, expLoc :: Pos}
  | BinOp {binOp :: BinOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Pos}
  | RelOp {relOp :: RelOp, e1 :: Exp a, e2 :: Exp a, expAnnot :: a, expLoc :: Pos}
  | Ref {e :: Exp a, expAnnot :: a, expLoc :: Pos}
  | Literal { val :: Literal , expAnnot :: a, expLoc :: Pos}
  | RoutCall { exprCall :: RoutineCall a, expAnnot :: a, expLoc :: Pos} 
  | Deref {e :: Exp a, expAnnot :: a, expLoc :: Pos}  
  | ArrayIndex {e :: Exp a, index :: Exp a, expAnnot :: a, expLoc :: Pos}
  | Var {name :: Ident, expAnnot :: a, expLoc :: Pos}
  | Array { elements :: [Exp a], expAnnot :: a, expLoc :: Pos }

data BinOp
    = Add | Sub | Mul | Div | DivInt | Mod | Pow
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp
    = Eq | Neq | Lt | Le | Gt | Ge
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Literal
    = LitInt Integer
    | LitChar Char
    | LitString String
    | LitBool Bool
    | LitDouble Double
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
