module UnitParty.Convert
( convert
, equidimensional
) where

import qualified Data.Map as M
import Data.Monoid (mempty)
import UnitParty.Types
import Data.Function (on)

-- attempt to derive a function to convert between units. this can fail if
-- (a) the units do not have compatible dimensions; or
-- (b) one of the units is nonsense.
convert :: Unit -> Unit -> Either ConversionError (Double -> Double)
convert (U a) (U b) = mixCheck a >> mixCheck b >> dimCheck a b
                   >> return (convTo b . convFrom a)
  where
    convFrom s = (* coefficient s) . (subtract $ constantTerm s)
    convTo s = (constantTerm s +) . (/ coefficient s)
    constantTerm = M.findWithDefault 0 mempty
    coefficient = snd . M.findMax

-- boolean test for dimensional compatibility
equidimensional :: Unit -> Unit -> Bool
equidimensional (U u1) (U u2) = case dimCheck u1 u2 of { Right _ -> True; _ -> False }

-- test for dimensional compatibility & return an informative error if the
-- test fails.
dimCheck :: Map Dimensionality Double -> Map Dimensionality Double -> Either ConversionError ()
dimCheck u1 u2 = sequence_ $ zipWith check (ds u1) (ds u2)
  where ds = M.keys . M.delete mempty
        check d1@(D a) d2@(D b) = if a =~= b then return ()
                            else Left $ Incommensurable d1 d2
          where (=~=) = (==) `on` M.filter (/=0)

-- check for mixed nonzero-degree polynomial dimensions like the you get when
-- computing fahrenheit^2. WHAT DOES THAT EVEN MEAN ???
mixCheck :: Map Dimensionality Double -> Either ConversionError ()
mixCheck a = if M.size (M.delete mempty a) == 1 then return ()
              else Left . MixedDegrees $ U a

