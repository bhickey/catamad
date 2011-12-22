module Point where

import Direction

newtype Point = Point { point :: (Int, Int) } deriving (Show, Eq, Ord)

instance Num Point where
  fromInteger = (\ x -> Point (fromIntegral x, 0))::(Integer-> Point)
  (Point (x1,y1)) + (Point (x2,y2)) = (Point ((x1 + x2), (y1 + y2)))
  (Point (x1,y1)) * (Point (x2,y2)) = (Point ((x1 * x2), (y1 * y2))) -- This makes no sense
  (Point (x1,y1)) - (Point (x2,y2)) = (Point ((x1 - x2), (y1 - y2)))
  abs (Point (x,y)) = (Point (abs x, abs y))
  signum (Point (x,y)) = (Point (signum x, signum y))

zeroPoint :: Point
zeroPoint = Point (0,0)

move :: Point -> Direction -> Point
move (Point (x,y)) North = Point (x, y - 1)
move (Point (x,y)) NorthEast = Point (x + 1, y - 1)
move (Point (x,y)) East = Point (x + 1, y)
move (Point (x,y)) SouthEast = Point (x + 1, y + 1)
move (Point (x,y)) South = Point (x, y + 1)
move (Point (x,y)) SouthWest = Point (x - 1, y + 1)
move (Point (x,y)) West = Point (x - 1, y)
move (Point (x,y)) NorthWest = Point (x - 1, y - 1)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point (x1,y1)) (Point (x2,y2)) = 
  (abs $ x1 - x2) + (abs $ y1 - y2)

euclideanDistance :: Point -> Point -> Int
euclideanDistance (Point (x1,y1)) (Point (x2,y2)) =
  round $ sqrt $ (fromIntegral $ x1 - x2) ** 2 
  + (fromIntegral $ y1 - y2) ** 2 + 0.5
