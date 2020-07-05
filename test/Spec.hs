import           Control.Applicative            ( liftA2 )
import           Data.Set                       ( Set )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set
import           Test.HUnit
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
  , complexExample @=? liftA2 (++) fstArg sndArg
  , assert (fstArg /= fstArg') -- DS equality too easy
  , assert (complexExample /= liftA2 (++) fstArg' sndArg)
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

fm om ms = fromJust (fromMasses om ms)

fstArg = fm ["A", "B"] [(["A"], 0.1), (["B"], 0.2), (["A", "B"], 0.7)]
-- ++
sndArg = fm ["X", "Y"] [(["X"], 0.3), (["Y"], 0.2), (["X", "Y"], 0.5)]
-- should be
complexExample = fm
  ["AX", "AY", "BX", "BY"]
  [ (["AX", "BX"]      , 0.063)
  , (["AX", "BY"]      , 0.042)
  , (["AX", "BX", "BY"], 0.105)
  , (["AY", "BX"], 0.042)
  , (["AY", "BY"], 0.028)
  , (["AY", "BX", "BY"], 0.07)
  , (["AX", "AY", "BX"], 0.105)
  , (["AX", "AY", "BY"], 0.07)
  , (["AX", "AY", "BX", "BY"], 0.175)
  , (["AX"], 0.03)
  , (["AY"], 0.02)
  , (["AX", "AY"], 0.05)
  , (["BX"], 0.06)
  , (["BY"], 0.04)
  , (["BX", "BY"], 0.1)
  ]

fstArg' = fm ["A", "B"] [(["A"], 0.15), (["B"], 0.2), (["A", "B"], 0.65)]