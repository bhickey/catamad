module Dungeon (unconditionalGet, get, cache, circularRoom, Dungeon) where

import Point
import Point.Metric
import Terrain
import Turn

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
circularRoom = Dungeon dungeonFunction empty

dungeonFunction :: Point -> Terrain
dungeonFunction (Point (0,0)) = Floor Stone
dungeonFunction p@(Point (x, y)) =
    let x' = x `mod` 5
        y' = y `mod` 5 in
    if (euclideanDistance p zeroPoint > 24)
      then Wall Bedrock
      else if (x' < 2 || y' < 2)
           then Floor Stone
           else if (x' == 2 && y' == 2)
                || (x' == 2 && y' == 4)
                || (x' == 4 && y' == 2)
                || (x' == 4 && y' == 4)
                then Pillar Stone
                else Wall Stone
