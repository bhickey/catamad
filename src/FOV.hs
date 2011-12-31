module FOV where

import Box
import Point
import Point.Metric
import Terrain        
import Dungeon
                                     
import Types

import Data.Maybe

doFov :: Turn -> Dungeon Terrain -> Box -> Point -> Dungeon Terrain
doFov turn dungeon bx offset =
  let viewFn pt = isJust (visible pt) && allowsVisibility (getFn pt)
      getFn pt = (unconditionalGet dungeon (pt + offset))
      visible p@(Point (0,0)) = Just $ getFn p
      visible p =
        if chessDistance p zeroPoint > 9
        then Nothing
        else if any viewFn (inwardPoints p)
             then Just $ getFn p 
             else Nothing in
    foldl (cache turn) dungeon [(i, fromJust $ visible i) | i <- indices bx, isJust $ visible i]

inwardPoints :: Point -> [Point]
inwardPoints p = 
  filter (\ n -> chessDistance zeroPoint n < chessDistance zeroPoint p) 
    [inward (Point (1,1))
    ,inward (Point (1,0))
    ,inward (Point (0,1))]
  where inward d = signum p * (abs p - d)
