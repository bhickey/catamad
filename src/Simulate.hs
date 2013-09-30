module Simulate where

import Actor
import Dungeon
import GameState
import Player
import Point
import Schedule --(getEvent, addEvent, Event(..), time)
import Draw (draw)
import FOV
import Time

import Data.Set (Set, elems)

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
runEvent _ PlayerEvent gs = do
	gs' <- showDungeon gs
	gen <- getStdGen
	repl gs' gen

showDungeon :: GameState -> IO GameState 
showDungeon gs = do
  let (gs', pts) = applyFov gs in
    draw pts gs' >> return gs'

applyFov :: GameState -> (GameState, Set Point)
applyFov (GameState am d t) =
  let p = snd $ getPlayer am
      pts = doFov p d
      d' = realizePoints d (elems pts) in
    (GameState am d' t, pts)
