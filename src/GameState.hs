module GameState (
  GameSchedule,
  GameState(..),
  GameEvent(MonsterEvent, PlayerEvent),
  TimedEvent,
  initialState,
  initialSchedule,
  newLevel) where

import Actor
import Actor.Map (ActorMap)
import qualified Actor.Map as AM
import Dungeon
import Point
import Time
import Turn
import Terrain

import Schedule (Schedule)
import qualified Schedule as S

import Data.Maybe (fromJust)

type TimedEvent = (Time, GameEvent)

data GameEvent =
    MonsterEvent (GameState -> (GameState, Maybe TimedEvent))
  | PlayerEvent (GameState -> (GameState, Maybe TimedEvent))

type GameSchedule = Schedule GameEvent

data GameState = GameState
  { actorMap :: ActorMap
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

initialSchedule :: GameSchedule
initialSchedule = S.singleton timeZero (PlayerEvent undefined)

initialState :: GameState
initialState = GameState (fromJust $ AM.fromList [(Actor PlayerId '@', zeroPoint)])
                       (circularRoom zeroPoint)
                       firstTurn

newLevel :: Point -> Time -> GameEvent -> GameState -> (GameState, GameSchedule)
newLevel p t g (GameState _ _ turn) =
  (GameState (fromJust $ AM.fromList [(Actor PlayerId '@', zeroPoint)])
             (circularRoom p)
             (nextTurn turn),
  S.singleton t g)
