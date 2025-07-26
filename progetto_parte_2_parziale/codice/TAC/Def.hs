-- TAC definitions and utilities

module TAC.Def where

import qualified Types as T
import qualified AbsSyn as AS

-- A program name
newtype ProgramName = ProgramName String

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

castAddressToProgramName :: Address -> ProgramName
castAddressToProgramName (PName name) = name
castAddressToProgramName _ = error "Expected a ProgramName address"

-- Possible kinds of coercions
data Coercion
    = BoolToInt
    | IntToDouble
    | CharToInt

-- Types used in TAC
data TACType = TInteger | TReal | TChar | TBoolean

castTypeToTACType :: T.Type-> TACType
castTypeToTACType T.TInteger = TInteger
castTypeToTACType T.TReal    = TReal
castTypeToTACType T.TChar    = TChar
castTypeToTACType T.TBoolean = TBoolean
castTypeToTACType _ = error "Unsupported type for TAC"


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

data RelOp
    = RelEq TACType
    | RelNeq TACType
    | RelLt TACType
    | RelLe TACType
    | RelGt TACType
    | RelGe TACType

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

-- A label
newtype Label = Label Int

data Instruction 
    = BOp { l :: Address, binOp :: BinOp,   r1, r2 :: Address }
    | UOp { l :: Address, unOp :: UnaryOp, r :: Address }
    | NOp { l :: Address, r  :: Address }
    | UnJump { to :: Label }
    | BoolJumpT { cond :: Address, to :: Label }
    | BoolJumpF { cond :: Address, to :: Label }
    | RelJump { rel :: RelOp, r1, r2 :: Address, to :: Label }
    | IndexGet { l :: Address, id :: ProgramName, idx :: Address }
    | IndexSet { id :: ProgramName, idx, r :: Address }
    | Ref { l :: Address, id :: ProgramName }
    | DerefGet { l1, l2 :: Address }
    | DerefSet { l :: Address, r :: Address }
    | Param {r :: Address}
    | ProcCall { rout :: Label, n :: Int }
    | FunCall { l :: Address, rout :: Label, n :: Int }
    | ReturnEmpty
    | Return { r :: Address }

data LabeledInstruction = LabeledInstruction
  { label :: Maybe Label,
    instruction :: Instruction
  }

type TAC = [LabeledInstruction]
