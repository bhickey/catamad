{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Data.Unique

data Actor = Actor
  { sym :: Char,
    id :: ActorId }

data ActorId = 
    PlayerId
  | MonsterId Unique
  deriving (Eq, Ord)

instance Mob Actor where
  glyph actor = sym actor

class Mob a where
  glyph :: a -> Char
