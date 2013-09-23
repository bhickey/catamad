module Actor.Map where

import Actor
import Point
import Data.Map (Map)
import qualified Data.Map as M

data ActorMap = ActorMap {
  posToActor :: Map Point ActorId,
  actorToPos :: Map ActorId Point,
  actors :: Map ActorId Actor
}

actorAt :: ActorMap -> Point -> Maybe Actor
actorAt (ActorMap pos _ act) pt = do
  M.lookup pt pos >>= (flip M.lookup $ act)

moveActor :: ActorMap -> ActorId -> Point -> Maybe ActorMap
moveActor am@(ActorMap p2a a2p actors) id np =
  case M.lookup np p2a of
    Just _ -> Nothing
    Nothing ->
      case (M.lookup id a2p) >>= (rekey p2a np) of
        Just p2a' -> Just $ ActorMap p2a' a2p' actors
        Nothing -> Nothing
  where a2p' = M.insert id np a2p

rekey mp nk ok =
  case M.lookup ok mp of
    Nothing -> Nothing
    Just a -> Just $ M.insert nk a $ M.delete ok mp
