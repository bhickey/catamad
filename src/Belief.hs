module Belief where

import Point
import Data.Identifiable
import qualified Data.Map as M
import Data.Map (Map)

data Beliefs a = Beliefs
  (Map Point (Identifier a))
  (Map (Identifier a) Point)

empty :: (Identifiable a) =>
  Beliefs a
empty = Beliefs M.empty M.empty

get :: (Identifiable a) =>
  Beliefs a ->
  Point ->
  Maybe (Identifier a)
get (Beliefs pm _) = flip M.lookup pm

find :: (Identifiable a) =>
  Beliefs a ->
  Identifier a ->
  Maybe Point
find (Beliefs _ im) = flip M.lookup im

put :: (Identifiable a) =>
  Beliefs a ->
  (Identifier a, Point) ->
  Beliefs a
put (Beliefs pm im) (i, p) =
  let pm' = case M.lookup i im of
              Nothing -> M.insert p i pm
              Just op -> M.insert p i (M.delete op pm) in
    Beliefs pm' (M.insert i p im)
