import           Test.HUnit
import           Data.Set                       ( Set )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set
import           DS

main = runTestTT $ TestList $ map
  TestCase
  [ Just 1 @=? mass myVacuous numbers
  , alice @=? alice <> myVacuous
  , aliceAndBob @=? myVacuous <> aliceAndBob
  , mass aliceAndBob' [2] @=? mass aliceAndBob [2]
  , aliceAndBob' @=? aliceAndBob
  , aliceAndBob @=? (aliceAndBob >>= return)
  , mass (simplify diagnosesF) [Flu, Amigdalitis]
    @=? mass diagnosesF [Flu, Amigdalitis]
  , mass (simplify diagnosesM) [Flu, Amigdalitis]
    @=? mass diagnosesM [Flu, Amigdalitis]
  , simplify (simplify diagnosesM) @=? simplify diagnosesM
  ]

myVacuous = vacuous numbers

numbers = [1 .. 5]
numbers' = fromMasses numbers

alice = fromJust $ numbers' [([1], 0.5), ([2], 0.5)]
bob = fromJust $ numbers' [([2], 0.2), ([3], 0.8)]
aliceAndBob = alice <> bob
aliceAndBob' = fromJust $ numbers' [([2], 1)]

data Symptom = Fever | BreathingProblems | Headache | Inflammation
  deriving (Eq, Ord, Enum, Bounded, Show)

data Sickness = Flu | Amigdalitis | Cancer
  deriving (Eq, Ord, Enum, Bounded, Show)

mySymptoms :: DS Symptom
mySymptoms = fromMasses' [([Fever, Headache], 1)]

singleDiagnose :: Symptom -> Sickness
singleDiagnose Fever             = Flu
singleDiagnose BreathingProblems = Cancer
singleDiagnose Headache          = Amigdalitis
singleDiagnose Inflammation      = Amigdalitis

diagnosesF :: DS Sickness
diagnosesF = singleDiagnose <$> mySymptoms

multiDiagnose :: Symptom -> DS Sickness
multiDiagnose Fever             = fromMasses' [([Flu, Amigdalitis], 1)]
multiDiagnose BreathingProblems = vacuous'
multiDiagnose Headache          = fromMasses'
  [([Flu, Amigdalitis], 0.3), ([Cancer], 0.4), ([minBound .. maxBound], 0.3)]
multiDiagnose Inflammation = return Amigdalitis

diagnosesM :: DS Sickness
diagnosesM = mySymptoms >>= multiDiagnose
