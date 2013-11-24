#!/usr/bin/env runhaskell
import UnitParty.Types
import UnitParty.Units
import UnitParty.Parser
import UnitParty.Convert
import Test.QuickCheck
import Test.HUnit
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import Control.Monad (when)

-- this file contains tests running on both QuickCheck and HUnit.
main = do
  putStrLn "running QuickCheck ..."
  qcresults <- mapM quickCheckResult properties
  putStrLn "running HUnit ..."
  huresults <- runTestTT tests
  when (any isFailure qcresults || failures huresults /= 0) exitFailure
  where
    isFailure (Failure {}) = True
    isFailure _ = False


-- QuickCheck

properties = [
    commensurability
  , incommensurability
  , conversion
  ]

instance Arbitrary Unit where
  -- just pick one from the units list, we ain't fancy
  arbitrary = elements $ M.elems baseUnits

instance CoArbitrary Unit where
  coarbitrary = coarbitraryShow -- :P

commensurability u1 u2 = equidimensional u1 u2 ==>
  isRight (convert u1 u2)

incommensurability u1 u2 = not (equidimensional u1 u2) ==>
  isLeft (convert u1 u2)

conversion a@(U u1) b@(U u2) = equidimensional a b ==>
  let ((_,coeff1),(_,coeff2)) = (M.findMax u1, M.findMax u2)
      cterm           = M.findWithDefault 0 mempty
      (c1,c2)         = (cterm u1, cterm u2)
      Right conv      = convert a b
  in conv 100 == ((100 - c1) * coeff1) / coeff2 + c2

isRight (Right _) = True
isRight _ = False

isLeft (Left _) = True
isLeft _ = False




-- HUnit

tests = TestList [
    conv "fahrenheit" "celsius" 32 =~ 0 ~? "fahrenheit -> celsius"
  , "milligram*hours" -/> "gigawebers/micron" ~? "incommensurable units"
  , "shekels/coulomb" --> "kilotesla*fortnights" ~? "commensurable units"
  , "meters^6/second^2" --> "sverdrup^2" ~? "commensurables w/ exponentiation"
  ]


-- helpers

unit :: String -> Unit
unit s = case parseUnit s of Right u -> u

conv :: String -> String -> Double -> Double
conv f t = case convert (unit f) (unit t) of Right c -> c

-- this is haskell so let's make egregious use of infix operators

infixl 5 =~
infixl 6 -->, -/>

(=~) :: Double -> Double -> Bool
(=~) = inDelta (10 ** (-6)) -- arbitrary
  where inDelta d f1 f2 = abs (f1 - f2) < d

(-/>) :: String -> String -> Bool
f -/> t = case convert (unit f) (unit t) of
  Left _ -> True
  _ -> False

(-->) :: String -> String -> Bool
f --> t = not $ f -/> t

