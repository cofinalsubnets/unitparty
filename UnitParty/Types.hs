module UnitParty.Types
( Dim(..)
, Dimensionality(..)
, Unit(..)
, ConversionError(..)
, Map
, UParseError(..)
) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.List (intercalate)

-- fundamental dimensions
data Dim = Mass | Distance | Time | Current
         | Temperature | Amount | LuminousIntensity
         deriving (Eq, Ord, Enum, Bounded)

instance Show Dim where
  show Distance    = "m"
  show Time        = "s"
  show Mass        = "kg"
  show Temperature = "K"
  show Current     = "A"
  show Amount      = "mol"
  show LuminousIntensity = "cd"

-- dimensions -> their degrees
newtype Dimensionality = D (Map Dim Int) deriving (Eq, Ord)

instance Show Dimensionality where
  show (D d) = intercalate "*" . map show' . M.toList $ M.filter (/=0) d
    where show' (d, p) = case p of 0 -> ""
                                   1 -> show d
                                   n -> show d ++ "^" ++ show n

instance Monoid Dimensionality where
  mempty  = D mempty
  mappend (D d1) (D d2) = D $ mappend d1 d2
  
-- dimensionalities -> their coefficients
newtype Unit = U (Map Dimensionality Double) deriving (Ord, Eq)

instance Show Unit where
  show (U m) = intercalate " + " . filter (not . null) . map show'
             . reverse . M.toList $ m
    where show' (t, c)
           | c == 0 = ""
           | c == 1 = show t
           | otherwise = show c ++ show t

instance Monoid Unit where
  mempty = U mempty
  mappend (U u1) (U u2) = U $ mappend u1 u2

-- type for algebraic (non-parsing related) conversion errors
data ConversionError = Incommensurable Dimensionality Dimensionality
                     | MixedDegrees Unit

instance Show ConversionError where
  show (Incommensurable d1 d2) = "Incommensurable units: "
                              ++ show d1 ++ ", " ++ show d2
  show (MixedDegrees u) = "Can't evaluate mixed nonzero degrees: " ++ show u

-- type for parsing errors
data UParseError = SyntaxError String | MismatchedParens String
                 | UnknownUnit String | QuantityError String

instance Show UParseError where
  show (SyntaxError e) = "Syntax error in `" ++ e ++ "'"
  show (MismatchedParens m) = "Mismatched parentheses in `" ++ m ++ "'"
  show (UnknownUnit u) = "Unknown unit: " ++ u
  show (QuantityError a) = "Can't parse amount: " ++ a

