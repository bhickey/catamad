module Grid 
  (Grid(..)
  , sample
  , charGrid
  , circularRoom) 
where

import Point
import Point.Metric
import Terrain

import Prelude hiding (lookup)
import Data.Map

data Grid a = Grid
  { basis :: (Point -> a)
  , override :: Map Point a
  }

sample :: Grid a -> Point -> a
sample (Grid b o) pos = 
  case lookup pos o of
    Just a -> a
    Nothing -> b pos

charGrid :: Point -> Terrain
charGrid = sample $ Grid
  (\ (Point (x,y)) ->
      if (x `mod` 5 == 0 && y `mod` 5 == 0)
      then Wall Stone
      else Floor Stone)
  empty

circularRoom :: Point -> Terrain
circularRoom = sample $ Grid
  (\ p ->
      if (withinRadius 10 (Point (15,15))) p
      then Wall Stone
      else Floor Stone)
  empty
  where withinRadius d p0 p1 =
          chessDistance p0 p1 >= d
