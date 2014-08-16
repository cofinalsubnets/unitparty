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
import Data.Either

-- this file contains tests running on both QuickCheck and HUnit.
main = do

  putStrLn "running QuickCheck ..."
  quickcheckPasses <- runQuickCheck

  putStrLn "running HUnit ..."
  hunitPasses <- runHUnit

  if quickcheckPasses && hunitPasses then exitSuccess else exitFailure

runQuickCheck = mapM quickCheckResult properties >>= return . not . any failed
  where failed Failure {} = True; failed _ = False

runHUnit = runTestTT tests >>= return . (==0) . failures

-- QuickCheck

properties = [
    commensurability
  , incommensurability
  , conversion
  ]

instance Arbitrary Unit where arbitrary = elements $ M.elems baseUnits
instance CoArbitrary Unit where coarbitrary = coarbitraryShow

commensurability a b   =      equidimensional a b  ==> isRight (convert a b)
incommensurability a b = not (equidimensional a b) ==> isLeft  (convert a b)

conversion a@(U u1) b@(U u2) = equidimensional a b ==>
  let ((_,coeff1),(_,coeff2)) = (M.findMax u1, M.findMax u2)
      cterm      = M.findWithDefault 0 mempty
      (c1, c2)   = (cterm u1, cterm u2)
      Right conv = convert a b
  in conv 100 == ((100 - c1) * coeff1) / coeff2 + c2


-- HUnit

tests = TestList [
    conv "fahrenheit" "celsius" 32 =~ 0 ~? "fahrenheit -> celsius"
  , "milligram*hours" -/> "gigawebers/micron" ~? "incommensurable units"
  , "shekels/coulomb" --> "kilotesla*fortnights" ~? "commensurable units"
  , "meters^6/second^2" --> "sverdrup^2" ~? "commensurables w/ exponentiation"
  , "furlong*hour^-2" --> "metre/second^2" ~? "division"
  , "(meter/second)^2" --> "meter^2/second^2" ~? "grouping with parentheses"
  , "second*(grain*mile)" --> "gram*year*metre" ~? "more grouping"
  , "(((second/hour)))" --> "meter/(megafoot)" ~? "nested parens"
  , "second * meter * mile / watt" --> "years^2*inches^2/joule" ~? "multiple ops"
  , "watts / meter ^ 2" --> "btus / (hour*foot^2)" ~? "exponentiation precedes multiplication"
  , "watts / meter ^ 2" --> "btus / (foot^2*hour)" ~? "exponentiation precedes multiplication 2"
  , " watts / meter ^ 2 " --> "BTUs/foot^2/hour" ~? "division is left-associative"
  ]


-- helpers

unit :: String -> Unit
unit = either (error . show) id . parseUnit

conv :: String -> String -> Double -> Double
conv f t = either (error . show) id $ convert (unit f) (unit t)

-- this is haskell so let's make egregious use of infix operators

infixl 5 =~
infixl 6 -->, -/>

(=~) :: Double -> Double -> Bool
a =~ b = abs (a - b) < 1e-6

(-/>) :: String -> String -> Bool
f -/> t = isLeft $ convert (unit f) (unit t)

(-->) :: String -> String -> Bool
f --> t = not $ f -/> t
