module Main where

import           DS

main = do
  putStrLn "first the Functor version"
  print (diagnosesF <$> mySymptoms)
  putStrLn "then the Monad one"
  print (mySymptoms >>= diagnosesM)

data Symptom = Fever | BreathingProblems | Headache | Inflammation
  deriving (Eq, Ord, Enum, Bounded, Show)

data Sickness = Flu | Amigdalitis | Cancer
  deriving (Eq, Ord, Enum, Bounded, Show)

mySymptoms :: DS Symptom
mySymptoms = fromMasses' [([Fever, Headache], 1)]

diagnosesF :: Symptom -> Sickness
diagnosesF Fever             = Flu
diagnosesF BreathingProblems = Cancer
diagnosesF Headache          = Amigdalitis
diagnosesF Inflammation      = Amigdalitis

diagnosesM :: Symptom -> DS Sickness
diagnosesM Fever             = fromMasses' [([Flu, Amigdalitis], 1)]
diagnosesM BreathingProblems = vacuous'
diagnosesM Headache          = fromMasses'
  [([Flu, Amigdalitis], 0.3), ([Cancer], 0.4), ([minBound .. maxBound], 0.3)]
diagnosesM Inflammation = return Amigdalitis
