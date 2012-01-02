module PointMap where

import Prelude hiding (map)

import Point
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

data PointMap a = PointMap
  { intMap :: IntMap a }

empty :: PointMap a
empty = PointMap I.empty

singleton :: Point -> a -> PointMap a
singleton k v = PointMap $ I.singleton (fromIntegral k) v

null :: PointMap a -> Bool
null = (I.null.intMap)

lookup :: PointMap a -> Point -> Maybe a
lookup (PointMap pm) p = I.lookup (fromIntegral p) pm

delete :: Point -> PointMap a -> PointMap a
delete k (PointMap p) = PointMap (I.delete (fromIntegral k) p)

insert :: Point -> a -> PointMap a -> PointMap a
insert k v (PointMap p) = PointMap (I.insert (fromIntegral k) v p)

update :: (a -> Maybe a) -> Point -> PointMap a -> PointMap a
update fn p (PointMap pm) = PointMap (I.update fn (fromIntegral p) pm) 

map :: (a -> b) -> PointMap a -> PointMap b
map f (PointMap pm) = PointMap (I.map f pm)
