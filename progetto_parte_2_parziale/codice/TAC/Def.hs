-- TAC definitions and utilities

module TAC.Def where

-- A program name
newtype ProgramName = ProgramName String

-- A temporary variable
newtype Temporary = Temporary Int

-- Known values
data Literal 
    = LitInt   Int
    | LitFloat Float
    | LitChar  Char
    | LitBool  Bool

-- An address can be a program name, a temporary variable, or a Literal
data Address
    = PName ProgramName
    | Temp Temporary
    | Lit Literal

-- Possible kinds of coercions
data Coercion
    = BoolToInt
    | IntToFloat
    | CharToInt

-- Types used in TAC
data TACType = TInteger | TReal | TChar | TBoolean

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

-- A label
newtype Label = Label Int

data Instruction 
    = BOp { l :: Address, binOp :: BinOp,   r1, r2 :: Address }
    | UOp { l :: Address, relOp :: UnaryOp, r :: Address }
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
  { label :: Label,
    instruction :: Instruction
  }

type TAC = [LabeledInstruction]
