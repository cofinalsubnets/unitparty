module UnitParty.Convert
( convert
, equidimensional
) where

import UnitParty.Types
import UnitParty.Parser

import qualified Data.Map as M
import Data.Monoid (mempty)
import Data.Function (on)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad (join)
import Control.Applicative ((<|>))

-- attempt to derive a function to convert between units. this can fail if
-- (a) the units do not have compatible dimensions; or
-- (b) one of the units is nonsense.
convert :: Unit -> Unit -> Either ConversionError (Double -> Double)
convert (U a) (U b) = maybe (Right (convTo b . convFrom a)) Left doChecks
  where
    doChecks = mixCheck a <|> mixCheck b <|> dimCheck a b
    convFrom s = (* coefficient s) . (subtract $ constantTerm s)
    convTo s = (constantTerm s +) . (/ coefficient s)
    constantTerm = M.findWithDefault 0 mempty
    coefficient = snd . M.findMax

-- boolean test for dimensional compatibility
equidimensional :: Unit -> Unit -> Bool
equidimensional (U u1) (U u2) = maybe True (const False) (dimCheck u1 u2)

-- test for dimensional compatibility & return an informative error if the
-- test fails.
dimCheck :: Map Dimensionality Double -> Map Dimensionality Double -> Maybe ConversionError
dimCheck u1 u2 = join $ find isJust (zipWith check (ds u1) (ds u2))
  where ds = M.keys . M.delete mempty
        check d1@(D a) d2@(D b) | a =~= b   = Nothing
                                | otherwise = Just $ Incommensurable d1 d2
        (=~=) = (==) `on` M.filter (/=0)

-- check for mixed nonzero-degree polynomial dimensions like the you get when
-- computing fahrenheit^2. WHAT DOES THAT EVEN MEAN ???
mixCheck :: Map Dimensionality Double -> Maybe ConversionError
mixCheck a = case M.size (M.delete mempty a) of 1 -> Nothing
                                                _ -> Just . MixedDegrees $ U a

