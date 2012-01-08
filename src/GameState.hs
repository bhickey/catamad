module GameState (LevelState(..), newState) where

import Dungeon
import Monster
import Point
import Turn
import Terrain

import Data.Map (Map)
import qualified Data.Map as M

data LevelState = LevelState
  { levelPlayer :: (Point, Monster)
  , levelMonsters :: Map Point Monster
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

newState :: LevelState
newState = LevelState 
  (zeroPoint, (Monster '@'))
  (M.singleton (Point (0,3)) (Monster 'B'))
  circularRoom
  firstTurn
