module Player where

import Action
import Actor
import qualified Dungeon as D
import Entity.Map
import GameState
import Keyboard
import Point
import Terrain
import Time
import Turn

import Data.Maybe (fromJust)
import System.Random
import UI.HSCurses.Curses (getCh, Key(..))

repl :: GameState -> StdGen -> IO ActionResult
repl gs gen = do
  act <- processKey
  let (gen', gen'') = split gen in
    case runAction act gs gen' of
      Just gs' -> return gs'
      Nothing -> repl gs gen''

runAction :: Action -> GameState -> StdGen -> Maybe ActionResult
runAction (Left act) _ _ = Just (Right act)
runAction (Right act) gs rng = runActorAction act gs rng

runActorAction :: ActorAction -> GameState -> StdGen -> Maybe ActionResult
runActorAction (MoveAttack dir) gs@(GameState am dungeon turn) gen =
  if hasMob
  then runActorAction (Attack dir) gs gen
  else if traversable $! D.get dungeon pt'
       then Just $ Left $
         (GameState
           (fromJust $ moveEntity am player pt')
           dungeon (nextTurn turn),
         Just (mkTime 100, PlayerEvent))
       else Nothing
  where pt = snd $ getPlayer am
        pt' = move pt dir
        hasMob = hasEntityAt am pt'

runActorAction (Attack dir) gs _ =
  case mob of
    Nothing -> Nothing
    Just m -> Just $ Left $
      (GameState
        (fromJust $ rmEntity am m)
        (levelBasis gs)
        (nextTurn $ levelTurn gs),
      Just (mkTime 100, PlayerEvent))
  where am = actorMap gs
        pt = snd $ getPlayer am
        pt' = move pt dir
        mob = entityAt am pt'

runActorAction UseStairs gs gen =
  if isStairs $ D.get dungeon pt
  then Just $ Left $ ((newLevel (fst $ random gen) gs), Just (mkTime 100, PlayerEvent))
  else Nothing
  where pt = snd $ getPlayer (actorMap gs)
        dungeon = levelBasis gs
        isStairs Stairs = True
        isStairs _ = False

runActorAction _ _ _ = Nothing

processKey :: IO Action
processKey = do
  keypress <- getCh
  case keypress of
    KeyChar x -> return $ getAction defaultKeymap x
    _ -> processKey
