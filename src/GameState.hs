module GameState (GameState(..)) where

import Cursor
import PointMap
import Dungeon
import Terrain
import Turn
import Mob.Monster

data GameState = GameState
  { player :: Cursor
  , monsters :: PointMap Monster
  , dungeon :: Dungeon Terrain
  , turn :: Turn }
