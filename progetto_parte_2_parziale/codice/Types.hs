module Types where

import qualified Errors     as E
import qualified LexGrammar as LG

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
  deriving (Show, Eq)

-- TODO: Cosi non possiamo sapere la posizione dei tipi più interni
data TypePos = TypePos
  { type_ :: Type,
    typePosn :: LG.Posn
  }
  deriving (Show)

-- Supertype relation for automatic type casting.
sup :: LG.Posn -> Type -> Type -> Type
-- TODO: implement the supertype relation
-- Error cases.
sup _ TError { msgs = msgs1 } TError { msgs = msgs2 } = 
    TError { msgs = msgs1 ++ msgs2 }
sup pos TError { msgs = msgs } t = TError { msgs = msgs }
sup pos t TError { msgs = msgs } = TError { msgs = msgs }
sup pos t1 t2 = TError { msgs = [E.err pos $ show t1 ++ " is not compatible with " ++ show t2] }

-- Supertype relation for lists of types.
supL :: LG.Posn -> [Type] -> [Type] -> [Type]
supL pos (t1:ts1) (t2:ts2) = sup pos t1 t2 : supL pos ts1 ts2
supL _ [] [] = []
supL pos ts1 [] = [TError { msgs = [E.err pos "Incompatible type number in supertype relation"] }]
supL pos [] ts2 = [TError { msgs = [E.err pos "Incompatible type number in supertype relation"] }]

getErrs :: [Type] -> [String]
getErrs [] = []
getErrs (TError { msgs = msgs } : ts) = msgs ++ getErrs ts
getErrs (_ : ts) = getErrs ts

mergeErrs :: [Type] -> Type
mergeErrs ts = TError { msgs = getErrs ts }