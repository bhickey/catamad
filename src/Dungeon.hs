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
  (\ p@(Point (x,y)) -> 
    let dist = euclideanDistance p zeroPoint
        w = doMagic p in
      if dist < 8 || (dist > 12 && dist < 20 - w)
      then Floor Stone
      else if x < 2 && x > -2 && y < 15 && y > -15
           then Floor Stone
           else Wall (if dist > 100 then Bedrock else Stone))
  empty
  where doMagic p@(Point (x,y)) = ((discreteLg 45 257) !! 
          ((x * 17 + y * 31 + 1337 * (fromIntegral $ toInteger p)) 
          `mod` 257)) `mod` 3
