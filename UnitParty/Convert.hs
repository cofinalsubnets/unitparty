module UnitParty.Convert
( convert
, equidimensional
) where

import UnitParty.Types

import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Function (on)
import Data.Maybe (isNothing, isJust)
import Data.List (find)
import Control.Monad (join)
import Control.Applicative ((<|>))

-- attempt to derive a function to convert between units. this can fail if
-- (a) the units do not have compatible dimensions; or
-- (b) one of the units is nonsense.
convert :: Unit -> Unit -> Either ConversionError (Double -> Double)
convert u1@(U a) u2@(U b) = maybe (Right (to . from)) Left checks
  where
    checks = mixCheck u1 <|> mixCheck u2 <|> dimCheck u1 u2
    from x = (x - constantTerm a) * coefficient a
    to y = y / coefficient b + constantTerm b
    constantTerm = M.findWithDefault 0 mempty
    coefficient = snd . M.findMax

-- boolean test for dimensional compatibility
equidimensional :: Unit -> Unit -> Bool
equidimensional = (isNothing .) . dimCheck

-- test for dimensional compatibility & return an informative error if the
-- test fails.
dimCheck :: Unit -> Unit -> Maybe ConversionError
dimCheck (U u1) (U u2) = join . find isJust $ zipWith check (ds u1) (ds u2)
  where ds = M.keys . M.delete mempty
        check d1@(D a) d2@(D b) | a =~= b   = Nothing
                                | otherwise = Just (Incommensurable d1 d2)
        (=~=) = (==) `on` M.filter (/=0)

-- check for mixed nonzero-degree polynomial dimensions like the you get when
-- computing fahrenheit^2. what does that even mean ??
mixCheck :: Unit -> Maybe ConversionError
mixCheck u@(U a) | M.size (M.delete mempty a) == 1 = Nothing
                 | otherwise = Just (MixedDegrees u)

