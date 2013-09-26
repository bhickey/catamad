module Player where

--import Action
--import Actor
--import qualified Actor.Map as AM
import Canvas
import Draw (draw)
--import Dungeon
import FOV
import GameState
--import Keyboard
--import Point
--import Schedule
--import Terrain
--import Time
--import Turn

--import Data.Maybe (fromJust)

import UI.HSCurses.Curses (getCh)

showDungeon :: GameState -> IO GameState 
showDungeon gs = do
  canvas <- stdCanvas
  let gs' = fov canvas gs in
    draw gs' >> getCh >> return gs'

fov :: Canvas -> GameState -> GameState
fov c g@(GameState am _ t) =
  let dungeon = doFov c g in
    (GameState am dungeon t)
{-
repl :: GameState -> StdGen -> IO GameState
repl gs gen = do
  act <- processKey
  let (gen', gen'') = split gen in
    case runAction act gs gen' of
      Just gs' -> return gs'
      Nothing -> repl gs gen''

runAction :: Action -> GameState -> StdGen -> Maybe (GameState, Maybe TimedEvent)
runAction (MoveAttack dir) (GameState am dungeon turn) _ =
  let pt = snd $ AM.getPlayer am
      pt' = move pt dir in
    if traversable $! (unconditionalGet dungeon pt')
    then Just undefined
    else Nothing

runAction UseStairs gs gen =
  if isStairs $ unconditionalGet dungeon pt
  then Just $ newLevel (fst $ random gen) timeZero (PlayerEvent undefined) gs
  else Nothing
  where pt = snd $ AM.getPlayer $ actorMap gs 
        dungeon = levelBasis gs
        isStairs Stairs = True
        isStairs _ = False
runAction Quit gs _ = Just gs
runAction _ _ _ = Nothing

processKey :: IO Action
processKey = do
  keypress <- getCh
  case keypress of
    KeyChar x -> return $ getAction defaultKeymap x
    _ -> processKey
-}