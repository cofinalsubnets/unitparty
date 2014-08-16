module UnitParty.Ops
( (+~)
, (-~)
, (*~)
, (/~)
, (**~)
, constant
) where

import qualified Data.Map as M
import Data.Monoid
import UnitParty.Types

infixr 5 +~, -~
infixr 6 *~, /~
infixr 7 **~

-- unit addition. this is only really meaningful when one of the units is
-- dimensionless.
(+~) :: Unit -> Unit -> Unit
U u1 +~ U u2 = U $ M.unionWith (+) u1 u2

-- unit subtraction. ditto the comment re: dimensionless units.
(-~) :: Unit -> Unit -> Unit
u1 -~ U u2 = u1 +~ U (M.map negate u2)

-- unit multiplication, as in newton*metre
(*~) :: Unit -> Unit -> Unit
U s1 *~ U s2 = U . M.fromListWith (+) . concatMap mulTerms $ M.toList s1
  where mulTerms (t,c) = map (\(t',c') -> (dimMul t t', c * c')) $ M.toList s2
        dimMul (D d1) (D d2) =  D $ M.unionWith (+) d1 d2

-- unit division, as in metre/second
(/~) :: Unit -> Unit -> Unit
u1 /~ u2 = u1 *~ uInvert u2

-- unit exponentiation, as in metre/second^2
(**~) :: Unit -> Int -> Unit
s **~ x | x < 0     = uInvert s **~ abs x
        | otherwise = iterate (s *~) (constant 1) !! x

-- unit inversion, as in 1/second
uInvert :: Unit -> Unit
uInvert (U u) = U $ M.map (1/) $ M.mapKeys (\(D d) -> D $ M.map negate d) u

constant :: Double -> Unit
constant = U . M.fromList . return . (,) mempty
