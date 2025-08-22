-- TAC definitions and utilities

module TAC.Def where

import qualified Types as T
import qualified AbsSyn as AS
import LexGrammar (Posn)

-- A program name
data ProgramName = ProgramName String Posn

-- A temporary variable
newtype Temporary = Temporary Int

-- Known values
data Literal
    = LitInt   Int
    | LitReal  Double
    | LitChar  Char
    | LitBool  Bool

-- An address can be a program name, a temporary variable, or a Literal
data Address
    = PName ProgramName 
    | Temp Temporary
    | Lit Literal
    | Stat Int

castAddressToProgramName :: Address -> ProgramName
castAddressToProgramName (PName name) = name
castAddressToProgramName _ = error "Expected a ProgramName address"

-- Possible kinds of coercions
data Coercion
    = IntToBool
    | IntToDouble
    | CharToInt

-- Types used in TAC
data TACType = TInteger | TReal | TChar | TBoolean | Addr

castTypeToTACType :: T.Type-> TACType
castTypeToTACType T.TInteger = TInteger
castTypeToTACType T.TReal    = TReal
castTypeToTACType T.TChar    = TChar
castTypeToTACType T.TBoolean = TBoolean
castTypeToTACType T.TReference { } = Addr
castTypeToTACType T.TArray { } = Addr
castTypeToTACType T.TString = Addr
castTypeToTACType T.TError {} = error "Error type should not be passed to TAC module"

-- Unary operations
data UnaryOp
    = BoolNeg
    | Sign TACType
    | Coer Coercion

-- Binary operations
data BinOp
    = Add TACType
    | Sub TACType
    | Mul TACType
    | Div
    | DivInt
    | Mod
    | Pow TACType
    | And
    | Or
    | Rel RelOp

castBinOpToTACOp :: AS.BinOp -> T.Type -> BinOp
castBinOpToTACOp AS.Add T.TInteger    = Add TInteger
castBinOpToTACOp AS.Add T.TReal       = Add TReal
castBinOpToTACOp AS.Sub T.TInteger    = Sub TInteger
castBinOpToTACOp AS.Sub T.TReal       = Sub TReal
castBinOpToTACOp AS.Mul T.TInteger    = Mul TInteger
castBinOpToTACOp AS.Mul T.TReal       = Mul TReal
castBinOpToTACOp AS.Div T.TReal       = Div
castBinOpToTACOp AS.DivInt T.TInteger = DivInt
castBinOpToTACOp AS.Mod T.TInteger    = Mod
castBinOpToTACOp AS.Pow T.TInteger    = Pow TInteger
castBinOpToTACOp AS.Pow T.TReal       = Pow TReal
castBinOpToTACOp AS.And T.TBoolean    = And
castBinOpToTACOp AS.Or  T.TBoolean    = Or
castBinOpToTACOp _ _                  = error "Unreachable: static checks should prevent this"

data RelOp
    = RelEq TACType
    | RelNeq TACType
    | RelLt TACType
    | RelLe TACType
    | RelGt TACType
    | RelGe TACType

negateRelOp :: RelOp -> RelOp
negateRelOp (RelEq ty) = RelNeq ty
negateRelOp (RelNeq ty) = RelEq ty
negateRelOp (RelLt ty) = RelGe ty
negateRelOp (RelLe ty) = RelGt ty
negateRelOp (RelGt ty) = RelLe ty
negateRelOp (RelGe ty) = RelLt ty

castRelOpToTACOp :: AS.RelOp -> T.Type -> BinOp
castRelOpToTACOp op ty = Rel $ castRelOpToTACRelOp op ty

castRelOpToTACRelOp :: AS.RelOp -> T.Type -> RelOp
castRelOpToTACRelOp AS.Eq ty     = RelEq  (castTypeToTACType ty)
castRelOpToTACRelOp AS.Neq ty    = RelNeq (castTypeToTACType ty)
castRelOpToTACRelOp AS.Lt ty     = RelLt  (castTypeToTACType ty)
castRelOpToTACRelOp AS.Le ty     = RelLe  (castTypeToTACType ty)
castRelOpToTACRelOp AS.Gt ty     = RelGt  (castTypeToTACType ty)
castRelOpToTACRelOp AS.Ge ty     = RelGe  (castTypeToTACType ty)

-- A label
data Label
    = Loc Int
    | Function ProgramName
    | FunctionUnique String

data Instruction
    = BOp { l :: Address, binOp :: BinOp,  r1, r2 :: Address, assignType :: TACType }
    | UOp { l :: Address, unOp :: UnaryOp, r :: Address, assignType :: TACType }
    | NOp { l :: Address, r  :: Address, assignType :: TACType }
    | UnJump { to :: Label }
    | BoolJumpT { cond :: Address, to :: Label }
    | BoolJumpF { cond :: Address, to :: Label }
    | RelJump { rel :: RelOp, r1, r2 :: Address, to :: Label }
    | IndexGet { l :: Address, id :: Address, idx :: Address, assignType :: TACType }
    | IndexSet { id :: Address, idx, r :: Address, assignType :: TACType }
    | Ref { l :: Address, id :: Address}
    | DerefGet { l1, l2 :: Address, assignType :: TACType }
    | DerefSet { l :: Address, r :: Address, assignType :: TACType }
    | Param { r :: Address, assignType :: TACType }
    | ProcCall { rout :: Label, n :: Int }
    | FunCall { l :: Address, rout :: Label, n :: Int, assignType :: TACType }
    | ReturnEmpty
    | Return { r :: Address }
    | ArrayGen { id :: Address, size :: Int }
    | Custom String
    | Comment String

data LabeledInstruction = LabeledInstruction
  { label :: [Label],
    instruction :: Instruction
  }

simpleInstruction :: Instruction -> LabeledInstruction
simpleInstruction = LabeledInstruction []

type Instructions = [LabeledInstruction]

data StaticData = Static {
    addr :: Address,
    value :: StaticValue
}

data StaticValue
    = STVString String
 -- | More static value types can be added

type TACStatic = [StaticData]

data TAC = TAC {
    instructions :: Instructions,
    staticData:: TACStatic
}
