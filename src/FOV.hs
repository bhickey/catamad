module FOV (doFov) where

import Box
import Point
import Point.Metric
import Terrain        
import Dungeon
                                     
import Types

import Data.Maybe

viewRadius :: Int
viewRadius = 6

doFov :: Turn -> Dungeon Terrain -> Box -> Point -> Dungeon Terrain
doFov trn d bx cursor =
  let getTerrain p = unconditionalGet d (p + cursor)
      visible (Point (0,0)) = Just (getTerrain zeroPoint)
      visible i = 
        if chessDistance i zeroPoint < viewRadius
        then if any (\ p -> (isJust.visible) p && (allowsVisibility.fromJust.visible) p) (inwardPoints i)
             then Just $ getTerrain i
             else Nothing
        else Nothing in
    foldl (cache trn) d [(i + cursor, (fromJust.visible) i) | i <- centerIndices bx, (isJust.visible) i]

inwardPoints :: Point -> [Point]
inwardPoints p = 
  filter (\ n -> chessDistance zeroPoint n < chessDistance zeroPoint p) 
    [inward (Point (1,1))
    ,inward (Point (1,0))
    ,inward (Point (0,1))]
  where inward d = signum p * (abs p - d)
