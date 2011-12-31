module Dungeon (unconditionalGet, get, cache, circularRoom, Dungeon) where

import Point
import Point.Metric
import Terrain

import Types

import Prelude hiding (lookup)
import Data.Map

data Dungeon a = Dungeon
  { dungeonBasis :: (Point -> a)
  , dungeonCache :: Map Point (a, Visibility)
  }

unconditionalGet :: Dungeon a -> Point -> a
unconditionalGet d pt =
  case get d pt of
    Just (a, _) -> a
    Nothing -> (dungeonBasis d) pt

get :: Dungeon a -> Point -> Maybe (a, Visibility)
get (Dungeon _ c) pt =
  lookup pt c

cache :: Turn -> Dungeon a -> (Point, a) -> Dungeon a
cache t (Dungeon b c) (pt,e) =
  Dungeon b (insert pt (e, Visibility t) c)

circularRoom :: Dungeon Terrain
circularRoom = Dungeon
  (\ p ->
      if (withinRadius 10 (Point (15,15))) p
      then Wall Stone
      else Floor Stone)
  empty
  where withinRadius d p0 p1 =
          chessDistance p0 p1 >= d
