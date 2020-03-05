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
import MassMap (MassMap)
import qualified MassMap as MM
import Data.Foldable (toList)

-- DS has the domain and a mass assignment map
newtype DS k = DS (MassMap k)
  deriving (Eq, Ord, Show)

-- The mass is simply the value of a in the set
mass :: (Foldable c, Ord k) => DS k -> Set k -> c k -> Double
mass (DS m) omega a = MM.domainLookup omega (Set.fromList (toList a)) m

fromMasses :: Ord k => Set k -> [([k], Double)] -> DS k
fromMasses omega l = DS $ MM.fromList (toList omega) $ map go l
  where go (as, d) = (Set.fromList as, d)

instance Ord k => Semigroup (DS k) where
  (DS x) <> (DS y) = DS $ MM.dempsterCombination x y

instance Ord k => Monoid (DS k) where
  mempty = DS MM.vacuous

someFunc :: IO ()
someFunc = putStrLn "someFunc"
