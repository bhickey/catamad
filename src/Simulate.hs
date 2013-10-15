module Simulate where

import Actor
import Dungeon
import GameState
import Player
import Point
import Schedule
import Draw (draw)
import FOV
import Time

import Entity.Map (updateEntity)

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
      result <- runEvent t act game
      case result of
        Left (game', maybeEvent) -> simulation_loop (updateSchedule maybeEvent s) game'
        Right _ -> return () -- Don't do anything with game actions yet.

updateSchedule :: Maybe TimedEvent -> GameSchedule -> GameSchedule
updateSchedule Nothing s = s
updateSchedule (Just (t, e)) s = addEvent t e s

runEvent :: Time -> GameEvent -> GameState -> IO ActionResult
runEvent _ (MonsterEvent act) gs = return $ Left $ act gs
runEvent _ PlayerEvent gs = do
	gs' <- showDungeon gs
	gen <- newStdGen
	repl gs' gen

showDungeon :: GameState -> IO GameState 
showDungeon gs = do
  let (gs', pts) = applyFov gs in
    draw pts gs' >> return gs'

applyFov :: GameState -> (GameState, Set Point)
applyFov (GameState am d t) =
  let d' = realizePoints d (elems pts) in
    (GameState am' d' t, pts)
  where p = snd $ getPlayer am
        pts = doFov p d
        info = map (\ x -> (x, get d x)) (elems pts)
        actor = teach (fst $ getPlayer am) info
        am' = updateEntity am actor
