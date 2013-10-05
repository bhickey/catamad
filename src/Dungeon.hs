module Dungeon (get, realizePoints, circularRoom, Dungeon, hash) where

import Point
import Point.Metric
import Terrain

import Prelude hiding (lookup)
import Data.Map hiding (foldl)

import Data.Hash.MD5

data Dungeon a = Dungeon
  { dungeonBasis :: (Point -> a)
  , dungeonCache :: Map Point a
  }

get :: Dungeon a -> Point -> a
get d pt =
  case lookup pt (dungeonCache d) of
    Just a -> a
    Nothing -> (dungeonBasis d) pt

realizePoints :: Dungeon a -> [Point] -> Dungeon a
realizePoints t d = foldl realizePoint t d

realizePoint :: Dungeon a -> Point -> Dungeon a
realizePoint d@(Dungeon b c) pt =
  Dungeon b (insert pt (get d pt) c)

circularRoom :: Point -> Dungeon Terrain
circularRoom p = Dungeon (dungeonFunction p) empty

dungeonFunction :: Point -> Point -> Terrain
dungeonFunction _ (Point (0,0)) = Floor Stone
dungeonFunction _ (Point (0,5)) = Stairs
dungeonFunction _ (Point (5,0)) = Door Closed
dungeonFunction offset p =
    let h = hash (offset + p) in
      if (euclideanDistance p zeroPoint) > (5 + (fromIntegral $ h `mod` 5))
        then Wall Bedrock
        else Floor Stone

hash :: Point -> Integer
hash p = md5i $ Str (show p)
