module DS where
  -- ( someFunc
  -- , DS
  -- , mass
  -- ) where

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import DefaultMap (DefaultMap, (!))
import qualified DefaultMap as DM
import Data.Function ((&))

newtype DS a = DS (DefaultMap (Set a) Double)
  deriving (Eq, Ord, Show)

-- The set of all possibilities
-- newtype Omega a = Omega {
--   getOmega :: Set a
-- } deriving (Eq)

-- The mass is the value of the complement of the set in the map
mass (DS defMap) omega set = defMap ! (omega\\set)

-- Dempster's combination rule works over the sum of all sets
-- whose intersection is the set we're looking the mass of.
-- Because we have the complements of said sets, we use the union
-- as per DeMorgan's property
dempsterCombination :: Ord a => DS a -> DS a -> DS a
dempsterCombination (DS m1) (DS m2) =
  let possibilities = Set.cartesianProduct (DM.keysSet m1) (DM.keysSet m2)
      subnormal = foldr sumByUnion (DM.empty 0) possibilities
      kConstant = calculateK subnormal
  in DS $ fmap (*kConstant) subnormal
    where sumByUnion (x, y) m = let set = Set.union x y
                                    value = m1!x * m2!y
                                in DM.insertWith (+) set value m

fromMasses :: Ord a => Set a -> [([a], Double)] -> DS a
fromMasses omega l = DS $ DM.fromList 0 $ map complement l
  where complement (as, d) = (omega \\ Set.fromList as, d)

-- The sum is a measure of the compatibility of the
-- evidences.
calculateK :: DefaultMap a Double -> Double
calculateK m = 1 -- sum m

instance Ord a => Semigroup (DS a) where
  (<>) = dempsterCombination

instance Ord a => Monoid (DS a) where
  mempty = DS $ DM.singleton 0 Set.empty 1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
