module GameState (
  GameSchedule,
  GameState(..),
  GameEvent(MonsterEvent, PlayerEvent),
  TimedEvent,
  initialState,
  initialSchedule,
  newLevel) where

import Actor
import qualified Knowledge as K
import Dungeon
import Point
import Time
import Turn
import Terrain

import Schedule (Schedule)
import qualified Schedule as S

import Entity.Map (EntityMap)
import qualified Entity.Map as EM
import Data.Maybe (fromJust)

type TimedEvent = (Time, GameEvent)

data GameEvent =
    MonsterEvent (GameState -> (GameState, Maybe TimedEvent))
  | PlayerEvent

type GameSchedule = Schedule GameEvent

data GameState = GameState
  { actorMap :: EntityMap Actor
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

initialSchedule :: GameSchedule
initialSchedule = S.singleton timeZero PlayerEvent

initialState :: GameState
initialState = GameState (fromJust $ EM.fromList [(Actor PlayerId '@' K.empty, zeroPoint)])
                       (circularRoom zeroPoint)
                       firstTurn

newLevel :: Point -> Time -> GameEvent -> GameState -> (GameState, GameSchedule)
newLevel p t g (GameState _ _ turn) =
  (GameState (fromJust $ EM.fromList [(Actor PlayerId '@' K.empty, zeroPoint)])
             (circularRoom p)
             (nextTurn turn),
  S.singleton t g)
