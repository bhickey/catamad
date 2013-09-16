module GameState (GameState(..), GameEvent(..), changeSchedule, mkState, newLevel) where

import Dungeon
import Monster
import Point
import Turn
import Terrain

import qualified Schedule as S

import Data.Map (Map)
import qualified Data.Map as M

data GameEvent = GameEvent (GameState -> IO GameState)

data GameState = GameState
  { levelSchedule :: S.Schedule GameEvent 
  , levelPlayer :: (Point, Monster)
  , levelMonsters :: Map Point Monster
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

changeSchedule :: GameState -> S.Schedule GameEvent -> GameState
changeSchedule (GameState _ p m b t) s = GameState s p m b t

mkState :: GameEvent -> GameState
mkState g = GameState 
  (S.singleton g)
  (zeroPoint, (Monster '@'))
  (M.singleton (Point (0,3)) (Monster 'B'))
  (circularRoom zeroPoint)
  firstTurn

newLevel :: Point -> GameEvent -> GameState -> GameState
newLevel p g (GameState _ (_, player) _ _ turn) = GameState
  (S.singleton g)
  (zeroPoint, player)
  (M.singleton (Point (0,3)) (Monster 'B'))
  (circularRoom p)
  (nextTurn turn)
