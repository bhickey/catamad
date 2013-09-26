module Simulate where

import GameState
import Player
import Schedule --(getEvent, addEvent, Event(..), time)
import Canvas
import Draw (draw)
import FOV
import Time

import System.Random

simulate :: IO ()
simulate = do
  simulation_loop initialSchedule initialState

simulation_loop :: GameSchedule -> GameState -> IO ()
simulation_loop schedule@(Schedule t _) game = do 
  case getEvent $ schedule of
    Nothing -> return ()
    Just ((Event _ act), s) -> do
      (game', maybeEvent) <- runEvent t act game
      simulation_loop (updateSchedule maybeEvent s) game'

updateSchedule :: Maybe TimedEvent -> GameSchedule -> GameSchedule
updateSchedule Nothing s = s
updateSchedule (Just (t, e)) s = addEvent t e s

runEvent :: Time -> GameEvent -> GameState -> IO (GameState, Maybe TimedEvent)
runEvent _ (MonsterEvent act) gs = return $ act gs
runEvent t PlayerEvent gs = do
	gs' <- showDungeon t gs
	gen <- getStdGen
	repl gs' gen

showDungeon :: Time -> GameState -> IO GameState 
showDungeon t gs = do
  canvas <- stdCanvas
  let gs' = fov canvas gs in
    draw t gs' >> return gs'

fov :: Canvas -> GameState -> GameState
fov c g@(GameState am _ t) =
  let dungeon = doFov c g in
    (GameState am dungeon t)
