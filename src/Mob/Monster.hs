module Mob.Monster where

data Monster = Monster
  { monName :: String
  , monSym :: Char }

instance Show Monster where
  show (Monster _ sym) = [sym]

blubBlub :: Monster
blubBlub = Monster "Blub-blub" 'b'
