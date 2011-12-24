module FOV where

import Point
import Point.Metric
import Terrain

isVisible :: Point -> (Point -> Terrain) -> Point -> Bool
isVisible center tfn pt =
  let visible (Point (0,0)) = True
      visible p =
        if chessDistance p zeroPoint > 9
        then False
        else any (\ ip -> (visible ip) && (allowsVisibility $ tfn (ip + center))) (inwardPoints p) in
    visible (pt - center)

inwardPoints :: Point -> [Point]
inwardPoints p = 
  filter (\ n -> chessDistance zeroPoint n < chessDistance zeroPoint p) 
    [inward (Point (1,1))
    ,inward (Point (1,0))
    ,inward (Point (0,1))]
  where inward d = signum p * (abs p - d)
