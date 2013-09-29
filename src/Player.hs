module Player where

import Action
import Actor
import Dungeon
import GameState
import Keyboard
import Point
import Terrain
import Time
import Turn

import Entity.Map

import Data.Maybe (fromJust)

import System.Random

import UI.HSCurses.Curses (getCh, Key(..))

repl :: GameState -> StdGen -> IO (GameState, Maybe TimedEvent)
repl gs gen = do
  act <- processKey
  let (gen', gen'') = split gen in
    case runAction act gs gen' of
      Just gs' -> return gs'
      Nothing -> repl gs gen''

runAction :: Action -> GameState -> StdGen -> Maybe (GameState, Maybe TimedEvent)
runAction (MoveAttack dir) (GameState am dungeon turn) _ =
  let pt = snd $ getPlayer am
      pt' = move pt dir in
    if traversable $! (unconditionalGet dungeon pt')
    then Just (GameState (fromJust $ moveEntity am player pt') dungeon (nextTurn turn), Just (mkTime 100, PlayerEvent))
    else Nothing

{-
runAction UseStairs gs gen =
  if isStairs $ unconditionalGet dungeon pt
  then Just $ newLevel (fst $ random gen) timeZero (PlayerEvent undefined) gs
  else Nothing
  where pt = snd $ AM.getPlayer $ actorMap gs 
        dungeon = levelBasis gs
        isStairs Stairs = True
        isStairs _ = False
runAction Quit gs _ = Just gs
--}
runAction _ _ _ = Nothing


processKey :: IO Action
processKey = do
  keypress <- getCh
  case keypress of
    KeyChar x -> return $ getAction defaultKeymap x
    _ -> processKey
