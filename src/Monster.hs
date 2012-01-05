{-# LANGUAGE ExistentialQuantification #-}
module Monster where

data Monster = forall a . Mob a => Monster a

class Mob a where
  symbol :: a -> Char
