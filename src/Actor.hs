{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Data.Identifiable
import Entity.Map
import Point

data Actor = Actor ActorId Char
data ActorId = PlayerId | MonsterId Integer
type ActorMap = EntityMap Actor

player :: Identifier Actor
player = mkIdentifier $ Actor PlayerId '@'

getPlayer :: ActorMap -> (Actor, Point)
getPlayer am = get am player

instance Identifiable Actor where
  identify (Actor PlayerId _) = -1
  identify (Actor (MonsterId i) _) = i

instance Mob Actor where
  glyph (Actor _ sym) = sym

class Mob a where
  glyph :: a -> Char
