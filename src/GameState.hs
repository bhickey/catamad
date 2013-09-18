module GameState (GameState(..), GameEvent(..), changeSchedule, mkState, newLevel) where

import Actor
import Dungeon
import Point
import Turn
import Terrain

import qualified Schedule as S

import Data.Map (Map)
import qualified Data.Map as M

data GameEvent = GameEvent (GameState -> IO GameState)

data GameState = GameState
  { levelSchedule :: S.Schedule GameEvent 
  , levelPlayer :: (Point, Actor)
  , levelMonsters :: Map Point Actor
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

changeSchedule :: GameState -> S.Schedule GameEvent -> GameState
changeSchedule (GameState _ p m b t) s = GameState s p m b t

mkState :: GameEvent -> GameState
mkState g = GameState 
  (S.singleton g)
  (zeroPoint, (Actor '@'))
  (M.singleton (Point (0,3)) (Actor 'B'))
  (circularRoom zeroPoint)
  firstTurn

newLevel :: Point -> GameEvent -> GameState -> GameState
newLevel p g (GameState _ (_, player) _ _ turn) = GameState
  (S.singleton g)
  (zeroPoint, player)
  (M.singleton (Point (0,3)) (Actor 'B'))
  (circularRoom p)
  (nextTurn turn)
