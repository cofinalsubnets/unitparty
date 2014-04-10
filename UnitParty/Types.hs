module UnitParty.Types
( Dim(..)
, Dimensionality(..)
, Unit(..)
, ConversionError(..)
, Map
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
    where show' (i, p) = case p of 0 -> ""
                                   1 -> show i
                                   _ -> show i ++ "^" ++ show p

instance Monoid Dimensionality where
  mempty = D mempty
  mappend (D d1) (D d2) = D $ mappend d1 d2
  
-- dimensionalities -> their coefficients
newtype Unit = U (Map Dimensionality Double) deriving (Ord, Eq)

instance Show Unit where
  show (U m) = intercalate " + " . filter (not . null) . map show'
             . reverse $ M.toList m
    where show' (t, c) = case c of 0 -> ""
                                   1 -> show t
                                   _ -> show c ++ show t

instance Monoid Unit where
  mempty = U mempty
  mappend (U u1) (U u2) = U $ mappend u1 u2

data ConversionError = Incommensurable Dimensionality Dimensionality
                     | MixedDegrees Unit

instance Show ConversionError where
  show (Incommensurable d1 d2) = "Incommensurable units: "
                              ++ show d1 ++ ", " ++ show d2
  show (MixedDegrees u) = "Can't evaluate mixed nonzero degrees: " ++ show u

