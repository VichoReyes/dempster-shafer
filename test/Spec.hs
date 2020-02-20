import Test.HUnit
import Data.Set as Set
import DS

main = runTestTT $ TestList [test1, test2, test3]

numbers = Set.fromList [1..5]
numbers' = fromMasses numbers

alice = numbers' [([1], 0.5), ([2], 0.5)]
bob = numbers' [([2], 0.2), ([3], 0.8)]
aliceAndBob = alice <> bob
aliceAndBob' = numbers' [([2], 1)]

test1 = TestCase (assertEqual "mempty" 1 (mass mempty numbers numbers))
test2 = TestCase (assertEqual "<>mempty" alice (alice<>mempty))
test3 = TestCase (assertEqual "alice and bob" aliceAndBob' aliceAndBob)
