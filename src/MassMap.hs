module MassMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- A MassMap is a Map where the keys are
-- always subsets of a domain omega
-- and the default value is 0
-- TODO make Eq, Ord ignore omega?
-- TODO auto-normalize?
data MassMap k = MM [k] (Map (Set k) Double)
  deriving (Eq, Ord, Show)

normalize :: Ord k => MassMap k -> MassMap k
normalize (MM omega m) = 
  let withoutEmpty = Map.delete Set.empty m
      inverseK = Map.foldr (+) 0 withoutEmpty
  in MM omega (fmap (/inverseK) withoutEmpty)

empty :: [k] -> MassMap k
empty omega = MM omega Map.empty

delete :: Ord k => Set k -> MassMap k -> MassMap k
delete k (MM omega m) = MM omega $ Map.delete k m

singleton :: [k] -> Set k -> Double -> MassMap k
singleton omega k v = MM omega $ Map.singleton k v

(!) :: Ord k => MassMap k -> Set k -> Double
(MM omega m) ! k = Map.findWithDefault 0 k m

insert :: Ord k => Set k -> Double -> MassMap k -> MassMap k
insert k v (MM omega m) = MM omega (Map.insert k v m)

fromList :: Ord k => [k] -> [(Set k, Double)] -> MassMap k
fromList omega = MM omega . Map.fromList

insertWith :: Ord k => (Double -> Double -> Double) -> Set k -> Double -> MassMap k -> MassMap k
insertWith f k v (MM omega m) = MM omega (Map.insertWith f k v m)

keysSet :: MassMap k -> Set (Set k)
keysSet (MM _ m) = Map.keysSet m
