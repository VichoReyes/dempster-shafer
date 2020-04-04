import Test.HUnit
import Data.Set (Set)
import qualified Data.Set as Set
import DS

main = runTestTT $ TestList $ map TestCase
  [ 1 @=? mass myVacuous numbers
  , alice @=? alice <> myVacuous
  , aliceAndBob @=? myVacuous <> aliceAndBob
  , mass aliceAndBob' [2] @=? mass aliceAndBob [2]
  , aliceAndBob' @=? aliceAndBob
  , aliceAndBob @=? (aliceAndBob >>= return)]

myVacuous = vacuous numbers

numbers = [1..5]
numbers' = fromMasses numbers

alice = numbers' [([1], 0.5), ([2], 0.5)]
bob = numbers' [([2], 0.2), ([3], 0.8)]
aliceAndBob = alice <> bob
aliceAndBob' = numbers' [([2], 1)]
