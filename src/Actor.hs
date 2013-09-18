{-# LANGUAGE ExistentialQuantification #-}
module Actor where

data Actor = Actor
  { sym :: Char }

instance Mob Actor where
  glyph actor = sym actor

class Mob a where
  glyph :: a -> Char
