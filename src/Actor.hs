{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Data.Identifiable
import Belief (Beliefs)
import qualified Belief as B
import Knowledge (Knowledge)
import qualified Knowledge as K
import Terrain
import Entity.Map
import Point

data Actor = Actor ActorId Char (Knowledge Terrain) (Beliefs Actor)
data ActorId = PlayerId | MonsterId Integer
type ActorMap = EntityMap Actor

player :: Identifier Actor
player = mkIdentifier $ Actor PlayerId '@' K.empty B.empty

getPlayer :: ActorMap -> (Actor, Point)
getPlayer am = get am player

teach :: Actor -> [(Point, Terrain)] -> Actor
teach (Actor i char knowledge belief) info =
  Actor i char (K.learnAll knowledge info) belief

instance Identifiable Actor where
  identify (Actor PlayerId _ _ _) = -1
  identify (Actor (MonsterId i) _ _ _) = i

instance Mob Actor where
  glyph (Actor _ sym _ _) = sym

class Mob a where
  glyph :: a -> Char
