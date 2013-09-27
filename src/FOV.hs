module FOV (doFov) where

import Box
import Point
import Point.Metric
import Terrain        
import Dungeon
              
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S

viewRadius :: Int
viewRadius = 5

viewBox :: Box
viewBox =
  let x = viewRadius * 2 + 1 in
    Box (x, x)


doFov :: Point -> Dungeon Terrain -> Set Point
doFov cursor d = do
  let getTerrain p = unconditionalGet d (p + cursor)
      visible (Point (0,0)) = Just (Floor Stone)
      visible i = 
        if chessDistance i zeroPoint < viewRadius
        then if any (\ p -> (isJust.visible) p && (allowsVisibility.fromJust.visible) p) (inwardPoints i)
             then Just $ getTerrain i
             else Nothing
        else Nothing in
    S.fromList [i + cursor | i <- centerIndices viewBox, (isJust.visible) i]
 
inwardPoints :: Point -> [Point]
inwardPoints p = 
  filter (\ n -> chessDistance zeroPoint n < chessDistance zeroPoint p) 
    [inward (Point (1,1))
    ,inward (Point (1,0))
    ,inward (Point (0,1))]
  where inward d = signum p * (abs p - d)
