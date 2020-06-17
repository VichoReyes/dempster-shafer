{-# LANGUAGE TupleSections #-}
module DS
  ( DS
  , vacuous
  , vacuous'
  , dempsterCombination
  , mass
  , simplify
  , fromMasses
  , fromMasses'
  )
where

import           Data.List                      ( union
                                                , intersect
                                                , elemIndex
                                                , nub
                                                )
import           Control.Monad                  ( ap )
import           Data.Maybe                     ( fromJust )
import           Data.Map                       ( Map
                                                , (!)
                                                , (!?)
                                                )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

-- The following module is an implementation of Dempster Shafer
-- Theory. It would be interesting to make a Transferable Belief Model
-- implementation (https://en.wikipedia.org/wiki/Transferable_belief_model)

data DS k = MM {
  getOmega :: [k],
  getIM :: Map [Int] Double
} deriving (Eq, Show)

type Switches = [Int]

normalize :: Ord k => DS k -> DS k
normalize (MM omega m) =
  let withoutEmpty = Map.delete emptySet m
      inverseK     = Map.foldr (+) 0 withoutEmpty
  in  MM omega (fmap (/ inverseK) withoutEmpty)

emptySet :: Switches
emptySet = []

-- | The vacuous mass assignment function. Everything is assigned to
-- | the uncertainty. Needs an omega set
vacuous :: Ord k => [k] -> DS k
vacuous omega = fromJust $ fromMasses omega [(omega, 1)]

-- | Convenience vacuous function for Bounded enums. Uses the whole
-- | enumeration as omega.
vacuous' :: (Ord k, Enum k, Bounded k) => DS k
vacuous' = vacuous [minBound .. maxBound]

-- | Dempster's Combination Rule for different MassMaps representing
-- | sources of evidence.
dempsterCombination :: Ord k => DS k -> DS k -> DS k
dempsterCombination (MM om1 m1) (MM om2 m2) =
  let possibilities = Set.cartesianProduct (switchSet m1) (switchSet m2)
      subnormal     = foldr sumByIntersection Map.empty possibilities
  in  normalize $ MM omega subnormal
 where
  omega = if null om1 then om2 else om1
  sumByIntersection (x, y) m =
    let set   = x `intersect` y
        value = m1 ! x * m2 ! y
    in  Map.insertWith (+) set value m

set2switches :: Ord k => [k] -> Set k -> Maybe Switches
set2switches omega s = traverse (`elemIndex` omega) (Set.toList s)

switches2set :: Ord k => [k] -> Switches -> Set k
switches2set omega = foldMap f where f i = Set.singleton $ omega !! i

-- | mass returns the mass of a set in a DS
-- | the result will be Nothing if the set contains
-- | any element not in the DS's Omega set.
mass :: Ord k => DS k -> [k] -> Maybe Double
mass = go . simplify
 where
  go (MM om im) a =
    let a' = set2switches om (Set.fromList a)
    in  flip (Map.findWithDefault 0) im <$> a'

-- | fromMasses creates a DS from an Omega set of possibilities
-- | and a list of (omega subset, corresponding mass) where the masses
-- | should add up to one.
-- TODO check that the masses add up to one
fromMasses :: Ord k => [k] -> [([k], Double)] -> Maybe (DS k)
fromMasses omega l = MM omega . Map.fromList <$> traverse adaptKey l
  where adaptKey (set, val) = (, val) <$> set2switches omega (Set.fromList set)

fromMasses' :: (Bounded k, Enum k, Ord k) => [([k], Double)] -> DS k
fromMasses' = fromJust . fromMasses [minBound .. maxBound]

simplify :: Ord k => DS k -> DS k
simplify (MM omega im) =
  let omega2     = nub omega
      newIndices = map (fromJust . flip elemIndex omega2) omega
      im2        = Map.mapKeysWith (+) (nub . map (newIndices !!)) im
  in  MM omega2 im2

switchSet :: Map Switches a -> Set Switches
switchSet m = Set.fromList $ Map.keys m

instance Functor DS where
  fmap f (MM omega im) = MM (fmap f omega) im

instance Applicative DS where
  pure x = MM [x] $ Map.singleton [0] 1
  (<*>) = ap

instance Monad DS where
  m >>= f = join $ fmap f m

instance Ord k => Semigroup (DS k) where
  (<>) = dempsterCombination

join :: DS (DS k) -> DS k
join (MM innerMMs outerIM)
  | null innerMMs = error "idk what to do here"
  | otherwise = MM
    (concat innerOmegas)
    (foldr1 addByKeys $ map (oneMass offsets innerMMs) (Map.assocs outerIM))
 where
  innerOmegas = map getOmega innerMMs
  offsets     = scanl (+) 0 $ map length innerOmegas

addByKeys = Map.unionWith (+)

oneMass :: [Int] -> [DS k] -> ([Int], Double) -> Map Switches Double
oneMass offsets finals (indices, scale) = Map.map (scale *)
  $ joinMassMaps (finals `indexes` indices) (offsets `indexes` indices)

indexes :: [a] -> [Int] -> [a]
indexes as = map (as !!)

joinMassMaps :: [DS k] -> [Int] -> Map Switches Double
joinMassMaps mms offsets =
  foldr1 combineIMs (padOffsets (map getIM mms) offsets)

padOffsets :: [Map Switches Double] -> [Int] -> [Map Switches Double]
padOffsets =
  zipWith (\im o -> Map.mapKeysWith (error "shouldn't happen") (map (+ o)) im)

combineIMs :: Map Switches Double -> Map Switches Double -> Map Switches Double
combineIMs im1 im2 = Map.fromList $ do
  (e1, v1) <- Map.assocs im1
  (e2, v2) <- Map.assocs im2
  return (e1 `union` e2, v1 * v2)
