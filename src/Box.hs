module Box where

import Point

newtype Box = Box { box :: (Int, Int) }

size :: Box -> Int
size (Box (x,y)) = (x * y)

toIndex :: Box -> Point -> Int
toIndex (Box (xm,ym)) (Point (x,y)) = (x + y * xm)

toPoint :: Box -> Int -> Point
toPoint (Box (xm,_)) ind = Point (ind `mod` xm, ind `div` xm)

indices :: Box -> [Point]
indices (Box (x,y)) = [Point (x',y') | x' <- [0..x], y' <- [0..y]]