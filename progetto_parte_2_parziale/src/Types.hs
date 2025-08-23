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
    deriving (Eq, Ord)

instance Show Type where
    show TBoolean = "Boolean"
    show TInteger = "Integer"
    show TReal    = "Real"
    show TString  = "String"
    show TChar    = "Char"
    show (TArray { elementsType = ty, size = s }) = "Array<" ++ show ty ++ "; " ++ show s ++ ">"
    show (TReference { refType = ty }) = "&" ++ show ty
    show TError { msgs = msgs } = "Error: " ++ unwords msgs

data TypePos = TypePos
  { type_ :: Type,
    typePosn :: LG.Posn
  }
  deriving (Show)

-- Supertype relation for automatic type casting.
sup :: LG.Posn -> Type -> Type -> Type
sup _ t1 t2 | t1 == t2 = t1
-- Integer -> Real
sup _ TInteger TReal = TReal
sup _ TReal TInteger = TReal
-- Integer -> Boolean
sup _ TBoolean TInteger = TBoolean
sup _ TInteger TBoolean = TBoolean
-- Char -> Integer
sup _ TChar TInteger = TInteger
sup _ TInteger TChar = TInteger
-- Char -> Real
sup _ TChar TReal = TReal
sup _ TReal TChar = TReal
-- Char -> Boolean
sup _ TChar TBoolean = TBoolean
sup _ TBoolean TChar = TBoolean
-- Error cases.
sup _ TError { msgs = msgs1 } TError { msgs = msgs2 } = 
    TError { msgs = msgs1 ++ msgs2 }
sup _ TError { msgs = msgs } _ = TError { msgs = msgs }
sup _ _ TError { msgs = msgs } = TError { msgs = msgs }
sup pos t1 t2 = TError { msgs = [E.err pos $ show t1 ++ " is not compatible with " ++ show t2 ++ "."] }

-- Function that retrieves all error messages from a list of types. It recursively
-- traverses the list and collects messages from TError types.
getErrs :: [Type] -> [String]
getErrs [] = []
getErrs (TError { msgs = msgs } : ts) = msgs ++ getErrs ts
getErrs (_ : ts) = getErrs ts

-- Get type size
getTypeSize :: Type -> Int
getTypeSize TBoolean = 1
getTypeSize TInteger = 4
getTypeSize TReal    = 8
getTypeSize TChar    = 1
getTypeSize TString  = 4 -- A string is represented by a pointer
getTypeSize (TArray { size = s, elementsType = ty }) = s * getTypeSize ty
getTypeSize TReference {} = 4
getTypeSize TError {} = error "TError size is not defined."

arrayInnerType :: Type -> Type
arrayInnerType (TArray { elementsType = ty }) = arrayInnerType ty
arrayInnerType ty = ty

arrayDim :: Type -> [Int]
arrayDim (TArray { size = s, elementsType = ty }) = s : arrayDim ty
arrayDim _ = []
