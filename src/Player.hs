module Player where

import Action
import Draw (draw)
import Dungeon
import GameState
import Keyboard
import Point
import Schedule
import Terrain
import Turn

import UI.HSCurses.Curses (getCh, Key(..))

playerAction :: GameState -> IO GameState 
playerAction gs = do 
  (draw gs) >> repl gs

repl :: GameState -> IO GameState
repl gs = do
  act <- processKey
  case runAction act gs of
    Just gs' -> return gs'
    Nothing -> repl gs

runAction :: Action -> GameState -> Maybe GameState
runAction (MoveAttack dir) (GameState schedule (pt, player) mobs dungeon turn) =
  let pt' = move pt dir in
    if traversable $! (unconditionalGet dungeon pt')
      then Just (GameState (updateSchedule schedule) (pt', player) mobs dungeon (nextTurn turn))
      else Nothing
runAction Quit gs = Just gs
runAction _ _ = Nothing

updateSchedule :: Schedule GameEvent -> Schedule GameEvent
updateSchedule = addEvent 0 (GameEvent playerAction)

processKey :: IO Action
processKey = do
  keypress <- getCh
  case keypress of
    KeyChar x -> return $ getAction defaultKeymap x
    _ -> processKey
