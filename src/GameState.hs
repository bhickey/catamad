module GameState (GameState(..), GameEvent(..), changeSchedule, mkState, newLevel) where

import Actor
import Actor.Map (ActorMap)
import qualified Actor.Map as AM
import Dungeon
import Point
import Turn
import Terrain

import qualified Schedule as S

import Data.Maybe (fromJust)

data GameEvent = GameEvent (GameState -> IO GameState)

data GameState = GameState
  { levelSchedule :: S.Schedule GameEvent
  , actorMap :: ActorMap
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

changeSchedule :: GameState -> S.Schedule GameEvent -> GameState
changeSchedule (GameState _ a b t) s = GameState s a b t

mkState :: GameEvent -> GameState
mkState g = GameState 
  (S.singleton g)
  (fromJust $ AM.fromList [(Actor PlayerId '@', zeroPoint)])
  (circularRoom zeroPoint)
  firstTurn

newLevel :: Point -> GameEvent -> GameState -> GameState
newLevel p g (GameState _ _ _ turn) = GameState
  (S.singleton g)
  (fromJust $ AM.fromList [(Actor PlayerId '@', zeroPoint)])
  (circularRoom p)
  (nextTurn turn)
