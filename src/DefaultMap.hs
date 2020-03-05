module DefaultMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

data DefaultMap k a = DM a (Map k a)
  deriving (Eq, Ord, Show)

instance Functor (DefaultMap k) where
  fmap f (DM d map) = DM (f d) (fmap f map)

empty :: a -> DefaultMap k a
empty d = DM d Map.empty

delete :: Ord k => k -> DefaultMap k a -> DefaultMap k a
delete k (DM d m) = DM d $ Map.delete k m

-- | The first argument is the default value
-- | The next two are the only element
singleton :: a -> k -> a -> DefaultMap k a
singleton d k v = DM d $ Map.singleton k v

(!) :: Ord k => DefaultMap k a -> k -> a
(DM d map) ! k = Map.findWithDefault d k map

insert :: Ord k => k -> a -> DefaultMap k a -> DefaultMap k a 
insert k v (DM d map) = DM d (Map.insert k v map)

fromList :: Ord k => a -> [(k, a)] -> DefaultMap k a
fromList d = DM d . Map.fromList

insertWith :: Ord k => (a -> a -> a) -> k -> a -> DefaultMap k a -> DefaultMap k a 
insertWith f k v (DM d map) = DM d (Map.insertWith f k v map)

keysSet :: DefaultMap k a -> Set k
keysSet (DM _ map) = Map.keysSet map
