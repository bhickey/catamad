module Dungeon (unconditionalGet, get, cache, circularRoom, Dungeon) where

import Point
import Point.Metric
import Terrain
import Turn

import Math.Log

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
get d pt =
  lookup pt (dungeonCache d)

cache :: Turn -> Dungeon a -> (Point, a) -> Dungeon a
cache t (Dungeon b c) (pt,e) =
  Dungeon b (insert pt (e, Visibility t) c)

circularRoom :: Dungeon Terrain
circularRoom = Dungeon
  (\ p -> if cellCenterDist p < cellSize p
    then Wall Stone
    else Floor Stone)
  empty
  where scale = 7
        rescale a = (a `div` scale)
        cell (Point (x, y)) = Point (rescale x, rescale y)
        cellSize (Point (x, y)) = (discreteLg 45 257 !! (abs $ (y + (discreteLg 45 257 !! (abs $ x `mod` 257)) `mod` 257))) `mod` 3
        cellCenter (Point (x, y)) = (Point (x * (scale `div` 2 + 1), y * (scale `div` 2 + 1)))
        cellCenterDist p =
          let c = cell p in
            manhattanDistance p (cellCenter c)

