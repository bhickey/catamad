module Dungeon (unconditionalGet, get, cache, circularRoom, Dungeon, hash) where

import Point
import Point.Metric
import Terrain
import Turn

import Prelude hiding (lookup)
import Data.Map

import Data.Hash.MD5

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

circularRoom :: Point -> Dungeon Terrain
circularRoom p = Dungeon (dungeonFunction p) empty

dungeonFunction :: Point -> Point -> Terrain
dungeonFunction _ (Point (0,0)) = Floor Stone
dungeonFunction _ (Point (5,0)) = Stairs
dungeonFunction offset p =
    let h = hash (offset + p) in
      if (euclideanDistance p zeroPoint) > (5 + (fromIntegral $ h `mod` 5))
        then Wall Bedrock
        else Floor Stone

hash :: Point -> Integer
hash p = md5i $ Str (show p)
