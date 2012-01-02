module Point.Metric where

import Point

type Metric = (Point -> Point -> Int)

manhattanDistance :: Metric
manhattanDistance (Point (x1,y1)) (Point (x2,y2)) = 
  (abs $! x1 - x2) + (abs $! y1 - y2)

euclideanDistance :: Metric
euclideanDistance (Point (x1,y1)) (Point (x2,y2)) =
  round $! sqrt $! ((fromIntegral $! x1 - x2) ** 2 
  + (fromIntegral $! y1 - y2) ** 2 :: Double)

chessDistance :: Metric
chessDistance (Point (x1,y1)) (Point (x2,y2)) =
  max (abs $! x1 - x2) (abs $! y1 - y2) 
