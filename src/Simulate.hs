module Simulate where

import GameState
import Player
import Schedule (getEvent, Event(..))

simulate :: IO ()
simulate = do
  simulation_loop (mkState initEvent)

simulation_loop :: GameState -> IO ()
simulation_loop game = do 
  case getEvent $ levelSchedule game of
    Nothing -> return ()
    Just ((Event _ act), _) -> do
      game' <- runEvent act game
      simulation_loop game'

runEvent :: GameEvent -> GameState -> IO GameState
runEvent (GameEvent act) ls = act ls

initEvent :: GameEvent
initEvent = GameEvent playerAction


