{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Data.Identifiable
import Knowledge as K
import Terrain
import Entity.Map
import Point

data Actor = Actor ActorId Char (Knowledge Terrain)
data ActorId = PlayerId | MonsterId Integer
type ActorMap = EntityMap Actor

player :: Identifier Actor
player = mkIdentifier $ Actor PlayerId '@' K.empty

getPlayer :: ActorMap -> (Actor, Point)
getPlayer am = get am player

teach :: Actor -> [(Point, Terrain)] -> Actor
teach (Actor i char knowledge) info =
  (Actor i char (learnAll knowledge info))

instance Identifiable Actor where
  identify (Actor PlayerId _ _) = -1
  identify (Actor (MonsterId i) _ _) = i

instance Mob Actor where
  glyph (Actor _ sym _) = sym

class Mob a where
  glyph :: a -> Char
