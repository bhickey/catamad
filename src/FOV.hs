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

towardOrigin :: Point -> Point
towardOrigin pt = (signum pt) * (abs pt - (Point (1,1))) 

inwardPoints :: Point -> [Point]
inwardPoints p = 
  filter (\ n -> chessDistance zeroPoint n < chessDistance zeroPoint p) 
    [towardOrigin p
    ,signum p * (abs p - (Point (1,0)))
    ,signum p * (abs p - (Point (0,1)))]
