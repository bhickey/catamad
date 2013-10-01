module Entity.Map where

import Prelude hiding (id)

import Data.Identifiable
import Point

import Control.Monad
import Data.Map (Map, (!))
import qualified Data.Map as M

data EntityMap a = EntityMap
  (Map Point (Identifier a))
  (Map (Identifier a) Point)
  (Map (Identifier a) a)

get :: (Identifiable a) =>
  EntityMap a
  -> Identifier a
  -> (a, Point)
get (EntityMap _ e2p em) i = (em ! i, e2p ! i)

addEntity :: (Identifiable a) =>
  EntityMap a 
  -> (a, Point)
  -> Maybe (EntityMap a)
addEntity (EntityMap p2e e2p am) (entity, pt) =
  if M.member pt p2e || M.member id am
  then Nothing
  else Just $ EntityMap
         (M.insert pt id p2e)
         (M.insert id pt e2p)
         (M.insert id entity am)
    where id = mkIdentifier entity

empty :: (Identifiable a) => EntityMap a
empty = EntityMap M.empty M.empty M.empty

fromList :: (Identifiable a) =>
  [(a, Point)]
  -> Maybe (EntityMap a)
fromList = foldM addEntity empty 

rmEntity :: (Identifiable a) =>
  EntityMap a
  -> a
  -> Maybe (EntityMap a)
rmEntity (EntityMap p2e e2p em) entity =
  if M.notMember id em
  then Nothing
  else Just $ EntityMap
         (M.delete (e2p ! id) p2e)
         (M.delete id e2p)
         (M.delete id em)
    where id = mkIdentifier entity

updateEntity :: (Identifiable a) =>
  EntityMap a
  -> a
  -> EntityMap a
updateEntity (EntityMap p2e e2p em) entity =
  EntityMap p2e e2p (M.insert id entity em)
  where id = mkIdentifier entity
entityAt :: (Identifiable a) =>
  EntityMap a
  -> Point
  -> Maybe a
entityAt (EntityMap pos _ entities) pt = do
  M.lookup pt pos >>= (flip M.lookup $ entities)

moveEntity :: (Identifiable a) =>
  EntityMap a
  -> (Identifier a)
  -> Point
  -> Maybe (EntityMap a)
moveEntity (EntityMap p2e e2p am) i np =
  if M.member np p2e 
  then Nothing
  else case (M.lookup i e2p) >>= (rekey p2e np) of
         Just p2e' -> Just $ EntityMap p2e' e2p' am
         Nothing -> Nothing
  where e2p' = M.insert i np e2p

rekey :: (Ord k) => Map k v -> k -> k -> Maybe (Map k v)
rekey mp nk ok =
  case M.lookup ok mp of
    Nothing -> Nothing
    Just a -> Just $ M.insert nk a $ M.delete ok mp
