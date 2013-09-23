{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Data.Unique

data Actor = Actor
  { id :: ActorId,
    sym :: Char }

data ActorId = 
    PlayerId
  | MonsterId Unique
  deriving (Eq, Ord)

instance Mob Actor where
  glyph actor = sym actor

class Mob a where
  glyph :: a -> Char
