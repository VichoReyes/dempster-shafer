module DS where
  -- ( someFunc
  -- , DS
  -- , mass
  -- ) where

-- The following module is an implementation of Dempster Shafer
-- Theory. It would be interesting to make a Transferable Belief Model
-- implementation (https://en.wikipedia.org/wiki/Transferable_belief_model)

import Data.Set (Set, (\\))
import qualified Data.Set as Set
import DefaultMap (DefaultMap, (!))
import qualified DefaultMap as DM
import Data.Foldable (toList)

-- DS has the domain and a mass assignment map
-- In the case of mempty, everything will be empty
-- and should be replaced with the others when
-- combining
data DS a = DS [a] (DefaultMap (Set a) Double)
  deriving (Eq, Ord, Show)

-- The mass is simply the value of a in the set
mass :: (Foldable b, Foldable c, Ord a) => DS a -> b a -> c a -> Double
mass (DS _ defMap) _ a = defMap ! (Set.fromList . toList) a

-- The DS combination rule works over the sum of all sets
-- whose intersection is the set we're looking the mass of.
-- Therefore, we sum every mass of an intersection
dempsterCombination :: Ord a => DS a -> DS a -> DS a
dempsterCombination ds1@(DS om1 _) ds2@(DS om2 _)
  | null om1 = ds2
  | null om2 = ds1
  | otherwise = dempsterCombination' ds1 ds2

dempsterCombination' :: Ord a => DS a -> DS a -> DS a
dempsterCombination' (DS om1 m1) (DS om2 m2) =
  let possibilities = Set.cartesianProduct (DM.keysSet m1) (DM.keysSet m2)
      subnormal = foldr sumByIntersection (DM.empty 0) possibilities
      kConstant = calculateK subnormal
  in DS omega $ DM.delete Set.empty $ fmap (kConstant*) subnormal
    where
      omega = if null om1 then om2 else om1
      sumByIntersection (x, y) m = let set = Set.intersection x y
                                       value = m1!x * m2!y
                                   in DM.insertWith (+) set value m

fromMasses :: Ord a => Set a -> [([a], Double)] -> DS a
fromMasses omega l = DS (toList omega) $ DM.fromList 0 $ map go l
  where go (as, d) = (Set.fromList as, d)

-- The sum is a measure of the compatibility of the
-- evidences.
calculateK :: Ord a => DefaultMap (Set a) Double -> Double
calculateK m = 1 / (1 - (m!Set.empty))

instance Ord a => Semigroup (DS a) where
  (<>) = dempsterCombination

instance Ord a => Monoid (DS a) where
  mempty = DS [] $ DM.singleton 0 Set.empty 1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
