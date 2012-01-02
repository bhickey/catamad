module Dungeon (unconditionalGet, get, cache, circularRoom, Dungeon) where

import Point
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
get d pt =
  lookup pt (dungeonCache d)

cache :: Turn -> Dungeon a -> (Point, a) -> Dungeon a
cache t (Dungeon b c) (pt,e) =
  Dungeon b (insert pt (e, Visibility t) c)

circularRoom :: Dungeon Terrain
circularRoom = Dungeon
  (\ (Point (x,y)) -> 
    if x < 6 && x > -6 && y < 5 && y > -5
    then Floor Stone
    else if x < 2 && x > -2
         then Floor Stone
         else Wall Bedrock)
  empty
