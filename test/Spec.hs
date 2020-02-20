import Test.HUnit
import Data.Set (Set)
import qualified Data.Set as Set
import DS

main = runTestTT $ TestList $ map TestCase
  [ 1 @=? mass mempty numbers numbers
  , alice @=? alice <> mempty
  , aliceAndBob @=? mempty <> aliceAndBob
  , mass aliceAndBob' numbers [2] @=? mass aliceAndBob numbers [2]
  , aliceAndBob' @=? aliceAndBob ]

numbers = Set.fromList [1..5] :: Set Integer
numbers' = fromMasses numbers

alice = numbers' [([1], 0.5), ([2], 0.5)]
bob = numbers' [([2], 0.2), ([3], 0.8)]
aliceAndBob = alice <> bob
aliceAndBob' = numbers' [([2], 1)]
