module Simulate where

import Actor.Map

import Dungeon
import GameState
import Player
import Schedule --(getEvent, addEvent, Event(..), time)
import Draw (draw)
import FOV
import Time

import Data.Set (elems)

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
  let gs' = applyFov gs in
    draw t gs' >> return gs'

applyFov :: GameState -> GameState
applyFov (GameState am d t) =
  let p = snd $ getPlayer am
      pts = doFov p d
      d' = realizePoints t d (elems pts) in
    GameState am d' t
