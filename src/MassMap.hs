module MassMap
  ( MassMap
  , vacuous
  , dempsterCombination
  , domainLookup
  , blindLookup
  , fromList
  ) where

import Data.List (intersect, elemIndex)
import Data.Maybe (fromJust)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A MassMap is a Map where the keys are
-- | always subsets of a domain omega
-- | and the default value is 0
-- TODO make Eq, Ord ignore omega?
-- TODO auto-normalize?
data MassMap k = Vacuous | MM {
  getOmega :: [k],
  getIM :: Map [Int] Double
} deriving (Eq, Show)

type Switches = [Int]

normalize :: Ord k => MassMap k -> MassMap k
normalize (MM omega m) =
  let withoutEmpty = Map.delete emptySet m
      inverseK = Map.foldr (+) 0 withoutEmpty
  in MM omega (fmap (/inverseK) withoutEmpty)

emptySet :: Switches
emptySet = []

-- This is going to cause a bug
-- fullSet :: Switches
-- fullSet = [0..]

-- | The vacuous mass assignment function. Everything is assigned to
-- | the uncertainty
vacuous :: MassMap k
vacuous = Vacuous

-- | Dempster's Combination Rule for different MassMaps representing
-- | sources of evidence.
dempsterCombination :: Ord k => MassMap k -> MassMap k -> MassMap k
dempsterCombination Vacuous mm2 = mm2
dempsterCombination mm1 Vacuous = mm1
dempsterCombination (MM om1 m1) (MM om2 m2) =
  let possibilities = Set.cartesianProduct (switchSet m1) (switchSet m2)
      subnormal = foldr sumByIntersection Map.empty possibilities
  in normalize $ MM omega subnormal
    where
      omega = if null om1 then om2 else om1
      sumByIntersection (x, y) m =
        let set = x `intersect` y
            value = m1!x * m2!y
        in Map.insertWith (+) set value m

set2switches :: Ord k => [k] -> Set k -> Switches
set2switches omega chosen = Set.toAscList $ Set.map (fromJust . flip elemIndex omega) chosen

-- Maybe version:
-- set2switches :: Ord k => [k] -> Set k -> Maybe Switches
-- set2switches omega s = foldr (.|.) Bits.zeroBits <$> traverse flipBit (toList s)
--   where flipBit int = Bits.bit <$> elemIndex el omega

switches2set :: Ord k => [k] -> Switches -> Set k
switches2set omega = foldMap f
  where f i = Set.singleton $ omega!!i

-- | domainLookup returns the mass of a set
-- | the first argument is the universe (needed in the vacuous case)
domainLookup :: Ord k => Set k -> Set k -> MassMap k -> Double
domainLookup om1 a Vacuous = if om1 == a then 1 else 0
domainLookup om1 a mm@(MM om2 m)
  | om1 /= Set.fromList om2 = error "different universe sets given"
  | otherwise = blindLookup a mm

-- | blindLookup is like domainLookup but it doesn't need the domain.
-- | For this reason, it doesn't work on the vacuous case.
blindLookup :: Ord k => Set k -> MassMap k -> Double
blindLookup a (MM om2 m) = Map.findWithDefault 0 a' m
  where a' = set2switches om2 a

insert :: Ord k => Set k -> Double -> MassMap k -> MassMap k
insert k v (MM omega m) = MM omega (Map.insert k' v m)
  where k' = set2switches omega k

-- | fromList creates a MassMap from an Omega set of possibilities
-- | and a list of (omega subset, corresponding mass) where the masses
-- | should add up to one.
fromList :: Ord k => [k] -> [(Set k, Double)] -> MassMap k
fromList omega l = MM omega $ Map.fromList l'
  where l' = map (\(s, d) -> (set2switches omega s, d)) l

insertWith :: Ord k => (Double -> Double -> Double) -> Set k -> Double -> MassMap k -> MassMap k
insertWith f k v (MM omega m) = MM omega (Map.insertWith f k' v m)
  where k' = set2switches omega k

switchSet :: Map Switches a -> Set Switches
switchSet m = Set.fromList $ Map.keys m

instance Functor MassMap where
  fmap f (MM omega im) = MM (fmap f omega) im
  fmap _ Vacuous = Vacuous
