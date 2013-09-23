module Actor.Map (ActorMap, addActor, get, getPlayer, empty, fromList, rmActor, actorAt, moveActor) where

import Actor
import Point

import Control.Monad
import Data.Map (Map, (!))
import qualified Data.Map as M

data ActorMap = ActorMap
  (Map Point ActorId)
  (Map ActorId Point)
  (Map ActorId Actor)

get :: ActorMap -> ActorId -> (Actor, Point)
get (ActorMap _ a2p am) i = (am ! i, a2p ! i)

getPlayer :: ActorMap -> (Actor, Point)
getPlayer = (flip get) PlayerId

addActor :: ActorMap -> (Actor, Point) -> Maybe ActorMap
addActor (ActorMap p2a a2p am) (actor@(Actor i _), pt) =
  if M.member pt p2a || M.member i am
  then Nothing
  else Just $ ActorMap
         (M.insert pt i p2a)
         (M.insert i pt a2p)
         (M.insert i actor am)

empty :: ActorMap
empty = ActorMap M.empty M.empty M.empty

fromList :: [(Actor, Point)] -> Maybe ActorMap
fromList = foldM addActor empty 

rmActor :: ActorMap -> Actor -> Maybe ActorMap
rmActor (ActorMap p2a a2p am) (Actor i _) =
  if M.notMember i am
  then Nothing
  else Just $ ActorMap
         (M.delete (a2p ! i) p2a)
         (M.delete i a2p)
         (M.delete i am)

actorAt :: ActorMap -> Point -> Maybe Actor
actorAt (ActorMap pos _ act) pt = do
  M.lookup pt pos >>= (flip M.lookup $ act)

moveActor :: ActorMap -> ActorId -> Point -> Maybe ActorMap
moveActor (ActorMap p2a a2p am) i np =
  if M.member np p2a 
  then Nothing
  else case (M.lookup i a2p) >>= (rekey p2a np) of
         Just p2a' -> Just $ ActorMap p2a' a2p' am
         Nothing -> Nothing
  where a2p' = M.insert i np a2p

rekey :: (Ord k) => Map k v -> k -> k -> Maybe (Map k v)
rekey mp nk ok =
  case M.lookup ok mp of
    Nothing -> Nothing
    Just a -> Just $ M.insert nk a $ M.delete ok mp
