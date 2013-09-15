module Simulate where

import GameState
import Player (playerAction)
import Schedule (getEvent, Event(..), Schedule(..))

simulate :: IO ()
simulate = do
  simulation_loop (mkState initEvent)

simulation_loop :: GameState -> IO ()
simulation_loop game = do 
  case getEvent $ levelSchedule game of
    Nothing -> return ()
    Just ((Event _ act), s) -> do
      game' <- runEvent act (updateSchedule game s)
      simulation_loop game'

updateSchedule :: GameState -> Schedule GameEvent -> GameState
updateSchedule (GameState _ p m b t) s = (GameState s p m b t)

runEvent :: GameEvent -> GameState -> IO GameState
runEvent (GameEvent act) ls = act ls

initEvent :: GameEvent
initEvent = GameEvent playerAction


