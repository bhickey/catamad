{-# LANGUAGE ExistentialQuantification #-}
module Monster where

data Monster = Monster 
  { sym :: Char }

instance Mob Monster where
  glyph (Monster s) = s

class Mob a where
  glyph :: a -> Char
