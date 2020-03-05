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
import MassMap (MassMap, (!))
import qualified MassMap as MM
import Data.Foldable (toList)

-- DS has the domain and a mass assignment map
-- In the case of mempty, everything will be empty
-- and should be replaced with the others when
-- combining
data DS k = DS [k] (MassMap k)
  deriving (Eq, Ord, Show)

-- The mass is simply the value of a in the set
mass :: (Foldable b, Foldable c, Ord k) => DS k -> b k -> c k -> Double
mass (DS _ map) _ a = map ! (Set.fromList (toList a))

dempsterCombination :: Ord k => DS k -> DS k -> DS k
dempsterCombination (DS om1 m1) (DS om2 m2) =
  let possibilities = Set.cartesianProduct (MM.keysSet m1) (MM.keysSet m2)
      subnormal = foldr sumByIntersection (MM.empty omega) possibilities
  in DS omega $ MM.normalize subnormal
    where
      omega = if null om1 then om2 else om1
      sumByIntersection (x, y) m = let set = Set.intersection x y
                                       value = m1!x * m2!y
                                   in MM.insertWith (+) set value m

fromMasses :: Ord k => Set k -> [([k], Double)] -> DS k
fromMasses omega l = DS (toList omega) $ MM.fromList (toList omega) $ map go l
  where go (as, d) = (Set.fromList as, d)

instance Ord k => Semigroup (DS k) where
  (<>) = dempsterCombination

instance Ord k => Monoid (DS k) where
  mempty = DS [] MM.vacuous

someFunc :: IO ()
someFunc = putStrLn "someFunc"
