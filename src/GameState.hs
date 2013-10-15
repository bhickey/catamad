module GameState (
  GameSchedule,
  GameState(..),
  GameEvent(MonsterEvent, PlayerEvent),
  TimedEvent,
  ActionResult,
  initialState,
  initialSchedule,
  newLevel) where

import Action
import Actor
import qualified Belief as B
import Dungeon
import qualified Knowledge as K
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

type GameUpdate = (GameState, Maybe TimedEvent)
type ActionResult = Either GameUpdate GameAction

data GameEvent =
    MonsterEvent (GameState -> GameUpdate)
  | PlayerEvent

type GameSchedule = Schedule GameEvent

data GameState = GameState
  { actorMap :: EntityMap Actor
  , levelBasis :: Dungeon Terrain
  , levelTurn :: Turn }

initialSchedule :: GameSchedule
initialSchedule = S.singleton timeZero PlayerEvent

initialState :: GameState
initialState = GameState
  (fromJust $ EM.fromList [(Actor PlayerId '@' K.empty B.empty, zeroPoint)])
  (circularRoom zeroPoint)
  firstTurn

newLevel :: Point -> GameState -> GameState
newLevel p (GameState _ _ turn) = GameState
  (fromJust $ EM.fromList [(Actor PlayerId '@' K.empty B.empty, zeroPoint)])
  (circularRoom p)
  (nextTurn turn)
