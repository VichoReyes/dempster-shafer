module MassMap where

import Data.List
import Data.Maybe (fromJust)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))

-- A MassMap is a Map where the keys are
-- always subsets of a domain omega
-- and the default value is 0
-- TODO make Eq, Ord ignore omega?
-- TODO auto-normalize?
data MassMap k = MM [k] (IntMap Double)
  deriving (Eq, Ord, Show)

normalize :: Ord k => MassMap k -> MassMap k
normalize (MM omega m) = 
  let withoutEmpty = IM.delete emptySet m
      inverseK = IM.foldr (+) 0 withoutEmpty
  in MM omega (fmap (/inverseK) withoutEmpty)

emptySet :: Int
emptySet = Bits.zeroBits

fullSet :: Int
fullSet = Bits.complement emptySet

vacuous :: MassMap k
vacuous = MM [] $ IM.singleton fullSet 1

dempsterCombination :: Ord k => MassMap k -> MassMap k -> MassMap k
dempsterCombination (MM om1 m1) (MM om2 m2) =
  let possibilities = Set.cartesianProduct (intSet m1) (intSet m2)
      subnormal = foldr sumByIntersection IM.empty possibilities
  in normalize $ MM omega subnormal
    where
      omega = if null om1 then om2 else om1
      sumByIntersection (x, y) m =
        let set = x .&. y
            value = fromJust (IM.lookup x m1) * fromJust (IM.lookup y m2)
        in IM.insertWith (+) set value m

-- these 2 functions shouldn't be exported
-- TODO return Maybe values
set2int :: Ord k => [k] -> Set k -> Int
set2int omega s = foldr acc Bits.zeroBits s
  where acc el int = int .|. Bits.bit (fromJust (elemIndex el omega))

int2set :: Ord k => [k] -> Int -> Set k
int2set omega is = foldMap f [0..Bits.finiteBitSize is]
  where f i = if Bits.testBit is i
                then Set.singleton $ omega!!i
                else Set.empty

empty :: [k] -> MassMap k
empty omega = MM omega IM.empty

delete :: Ord k => Set k -> MassMap k -> MassMap k
delete k (MM omega m) = MM omega $ IM.delete k' m
  where k' = set2int omega k

singleton :: Ord k => [k] -> Set k -> Double -> MassMap k
singleton omega k v = MM omega $ IM.singleton k' v
  where k' = set2int omega k

(!) :: Ord k => MassMap k -> Set k -> Double
(MM omega m) ! k = IM.findWithDefault 0 k' m
  where k' = set2int omega k

insert :: Ord k => Set k -> Double -> MassMap k -> MassMap k
insert k v (MM omega m) = MM omega (IM.insert k' v m)
  where k' = set2int omega k

fromList :: Ord k => [k] -> [(Set k, Double)] -> MassMap k
fromList omega l = MM omega $ IM.fromList l'
  where l' = map (\(s, d) -> (set2int omega s, d)) l

insertWith :: Ord k => (Double -> Double -> Double) -> Set k -> Double -> MassMap k -> MassMap k
insertWith f k v (MM omega m) = MM omega (IM.insertWith f k' v m)
  where k' = set2int omega k

intSet :: IntMap a -> Set Int
intSet m = Set.fromList $ IM.keys m
