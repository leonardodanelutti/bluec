-- | The abstract syntax of language grammar.

module AbsSyn where

import Prelude (Char, Double, Integer, String, Bool)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Pos = Pos {
    fstLine, lstLine, fstCol, lstCol :: Integer
}

data Declarations a = ProgramDecl {
    consts :: [ConstA a],
    vars :: [VarA a],
    routines :: [Routine a]
}

data ConstA a = ConstAss {
    constIdents :: [Ident], 
    constType :: TypeLoc, 
    constValues :: [RExp a],
    constAnnot :: a,
    constLoc :: Pos
}

data VarA a = VarAss {
    varIdents :: [Ident], 
    varType :: TypeLoc,
    varValues :: [RExp a],
    varAnnot :: a,
    varLoc :: Pos
}

data Routine a = Routine {
    routName :: Ident,
    params :: [Param], 
    returnTypes :: [TypeLoc],
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
data TypeLoc = TypeLoc {
    type_ :: Type,
    typeLoc :: Pos
}

data Param = Param {
  mode :: Mode,
  paramName :: Ident,
  paramType :: TypeLoc,
  paramLoc :: Pos
}

data Mode = Mval | Mvalres | Mref

data RoutBody a = RoutBody {
  declarations :: Declarations a,
  statements :: [Statement a]
}

data RoutineCall a = RoutineCall {
  routCallName :: Ident,
  args :: [RExp a],
  routCallAnnot :: a,
  routCallLoc :: Pos
}

data Statement a
    = StmtAss {left :: [LExp a], right :: [RExp a], annot :: a, loc :: Pos}
    | StmtRoutCall {call :: RoutineCall a, annot :: a, loc :: Pos}
    | StmtReturn {vals :: [RExp a], annot :: a, loc :: Pos}
    | StmtContinue {annot :: a, loc :: Pos}
    | StmtIf {cond :: RExp a, body :: [Statement a], annot :: a, loc :: Pos}
    | StmrIfElse {cond :: RExp a, bodyT :: [Statement a], bodyF :: [Statement a], annot :: a, loc :: Pos}
    | StmtLoop {body :: [Statement a], exit :: [Exit a], annot :: a, loc :: Pos}

data Exit a = LoopExit {loopCond :: RExp a, loopBody :: [Statement a], loopExitAnnot :: a, loopExitLoc :: Pos}

data RExp a
  = BoolAnd {e1 :: RExp a, e2 :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | BoolOr {e1 :: RExp a, e2 :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | BoolNeg {e :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | Sign {e :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | BinOp {binOp :: BinOp, e1 :: RExp a, e2 :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | RelOp {relOp :: RelOp, e1 :: RExp a, e2 :: RExp a, rexpAnnot :: a, rexLoc :: Pos}
  | Ref {eL :: LExp a, rexpAnnot :: a, rexLoc :: Pos}
  | Literal { val :: Literal , rexpAnnot :: a, rexLoc :: Pos}
  | RoutCall { exprCall :: RoutineCall a, rexpAnnot :: a, rexLoc :: Pos} 
  | LExp { eL :: LExp a }
  | Array { elements :: [RExp a] }

data BinOp
    = Add | Sub | Mul | Div
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

data LExp a
    = Deref {expr :: LExp a}
    | ArrayIndex {expr :: LExp a, index :: RExp a}
    | Var {name :: Ident}

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
