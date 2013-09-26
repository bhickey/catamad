module Simulate where

import GameState
import Player
import Schedule (getEvent, addEvent, Event(..))

simulate :: IO ()
simulate = do
  simulation_loop initialSchedule initialState

simulation_loop :: GameSchedule -> GameState -> IO ()
simulation_loop schedule game = do 
  case getEvent $ schedule of
    Nothing -> return ()
    Just ((Event _ act), s) -> do
      (game', maybeEvent) <- runEvent act game
      simulation_loop (updateSchedule maybeEvent s) game'

updateSchedule :: Maybe TimedEvent -> GameSchedule -> GameSchedule
updateSchedule Nothing s = s
updateSchedule (Just (t, e)) s = addEvent t e s

runEvent :: GameEvent -> GameState -> IO (GameState, Maybe TimedEvent)
runEvent (MonsterEvent act) gs = return $ act gs
runEvent (PlayerEvent _) gs = do
	gs' <- showDungeon gs
	return (gs', Nothing)