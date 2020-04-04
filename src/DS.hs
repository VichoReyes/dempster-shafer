module DS
  ( DS
  , vacuous
  , dempsterCombination
  , mass
  , blindMass
  , fromMasses
  ) where

import Data.List (union, intersect, elemIndex)
import Control.Monad (ap)
import Data.Maybe (fromJust)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- The following module is an implementation of Dempster Shafer
-- Theory. It would be interesting to make a Transferable Belief Model
-- implementation (https://en.wikipedia.org/wiki/Transferable_belief_model)

data DS k = Vacuous | MM {
  getOmega :: [k],
  getIM :: Map [Int] Double
} deriving (Eq, Show)

type Switches = [Int]

normalize :: Ord k => DS k -> DS k
normalize (MM omega m) =
  let withoutEmpty = Map.delete emptySet m
      inverseK = Map.foldr (+) 0 withoutEmpty
  in MM omega (fmap (/inverseK) withoutEmpty)

emptySet :: Switches
emptySet = []

-- | The vacuous mass assignment function. Everything is assigned to
-- | the uncertainty
vacuous :: DS k
vacuous = Vacuous

-- | Dempster's Combination Rule for different MassMaps representing
-- | sources of evidence.
dempsterCombination :: Ord k => DS k -> DS k -> DS k
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

-- | mass returns the mass of a set
-- | the first argument is the universe (needed in the vacuous case)
mass :: Ord k => DS k -> [k] -> [k] -> Either String Double
mass Vacuous om1 a = Right $ if om1 == a then 1 else 0
mass mm@(MM om2 m) om1 a
  | om1 /= om2 = Left "different universe sets given"
  | otherwise = Right $ Map.findWithDefault 0 a' m
  where a' = set2switches om2 (Set.fromList a)

-- | blindMass is like mass but it doesn't need the domain.
-- | For this reason, it doesn't work on the vacuous case.
blindMass :: Ord k => DS k -> [k] -> Either String Double
blindMass Vacuous _ = Left "blindMass: tried to get mass from a Vacuous"
blindMass ds@(MM omega im) a = mass ds omega a

insert :: Ord k => Set k -> Double -> DS k -> DS k
insert k v (MM omega m) = MM omega (Map.insert k' v m)
  where k' = set2switches omega k

-- | fromMasses creates a DS from an Omega set of possibilities
-- | and a list of (omega subset, corresponding mass) where the masses
-- | should add up to one.
fromMasses :: Ord k => [k] -> [([k], Double)] -> DS k
fromMasses omega l = MM omega $ Map.fromList l'
  where l' = map (\(s, d) -> (set2switches omega (Set.fromList s), d)) l

insertWith :: Ord k => (Double -> Double -> Double) -> Set k -> Double -> DS k -> DS k
insertWith f k v (MM omega m) = MM omega (Map.insertWith f k' v m)
  where k' = set2switches omega k

switchSet :: Map Switches a -> Set Switches
switchSet m = Set.fromList $ Map.keys m

instance Functor DS where
  fmap f (MM omega im) = MM (fmap f omega) im
  fmap _ Vacuous = Vacuous

instance Applicative DS where
  pure x = MM [x] $ Map.singleton [0] 1
  (<*>) = ap

instance Monad DS where
  m >>= f = join $ fmap f m

instance Ord k => Semigroup (DS k) where
  (<>) = dempsterCombination

instance Ord k => Monoid (DS k) where
  mempty = Vacuous

-- warning: zero will not work
join :: DS (DS k) -> DS k
join Vacuous = error "stop messing with me"
join (MM innerMMs outerIM)
  | null innerMMs = error "idk what to do here"
  | otherwise = MM (concat innerOmegas)
      (foldr1 addByKeys $ map (oneMass offsets innerMMs) (Map.assocs outerIM))
    where
      innerOmegas = map getOmega innerMMs -- PARTIAL error when a vacuous one
      offsets = scanl (+) 0 $ map length innerOmegas

addByKeys = Map.unionWith (+)

-- oneMass :: [DS k] -> ([Int], Double) -> Map Switches Double
oneMass offsets finals (indices, scale) = Map.map (scale*) $ joinMassMaps (finals `indexes` indices) (offsets `indexes` indices)

indexes :: [a] -> [Int] -> [a]
indexes as = map (as!!)

joinMassMaps :: [DS k] -> [Int] -> Map Switches Double
joinMassMaps mms offsets = foldr1 combineIMs (padOffsets (map getIM mms) offsets)

padOffsets = zipWith (\im o -> Map.mapKeysWith (error "shouldn't happen") (map (+o)) im)

combineIMs :: Map Switches Double -> Map Switches Double -> Map Switches Double
combineIMs im1 im2 = Map.fromList $ do
  (e1, v1) <- Map.assocs im1
  (e2, v2) <- Map.assocs im2
  return (e1 `union` e2, v1 * v2)
