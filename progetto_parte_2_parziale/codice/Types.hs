-- Module containing type definitions and supertype hierarchy for type casting.
module Types where

import qualified Errors     as E
import qualified LexGrammar as LG

-- Type definitions for the language.
data Type
    = TBoolean
    | TInteger
    | TReal
    | TString
    | TChar
    | TArray
        {
            elementsType :: Type,
            size :: Int
        }
    | TReference { refType :: Type }
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
sup _ TError { msgs = msgs } _ = TError { msgs = msgs }
sup _ _ TError { msgs = msgs } = TError { msgs = msgs }
sup pos t1 t2 = TError { msgs = [E.err pos $ show t1 ++ " is not compatible with " ++ show t2] }

-- In Blue, routines can output lists of types. Thus expressions can have multiple types.
-- This function extends the supertype relation to lists of types.
--
-- If the lists are of different lengths, an error type is added to the return list.
supL :: LG.Posn -> [Type] -> [Type] -> [Type]
supL pos (t1:ts1) (t2:ts2) = sup pos t1 t2 : supL pos ts1 ts2
supL _ [] [] = []
supL pos _ [] = [TError { msgs = [E.err pos "Incompatible type number in supertype relation"] }]
supL pos [] _ = [TError { msgs = [E.err pos "Incompatible type number in supertype relation"] }]

-- Function that retrieves all error messages from a list of types. It recursively
-- traverses the list and collects messages from TError types.
getErrs :: [Type] -> [String]
getErrs [] = []
getErrs (TError { msgs = msgs } : ts) = msgs ++ getErrs ts
getErrs (_ : ts) = getErrs ts