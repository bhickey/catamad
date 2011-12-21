module Grid 
  (Grid
  , sample
  , charGrid) 
where

import Point

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

charGrid :: Point -> Char
charGrid = sample $ Grid
  (\ (Point (x,y)) ->
      if (x `mod` 5 == 0 && y `mod` 5 == 0)
      then '#'
      else '.')
  empty
