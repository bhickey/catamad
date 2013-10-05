module Keyboard where

import Data.Map (Map)
import qualified Data.Map as M

import Action
import Direction

type KeyboardMap a = Map Char a

getAction :: KeyboardMap Action -> Char -> Action
getAction m k = M.findWithDefault (Right NoAction) k m

defaultKeymap :: KeyboardMap Action
defaultKeymap = M.fromList $
  [('y', Right $ MoveAttack NorthWest),
   ('k', Right $ MoveAttack North),
   ('u', Right $ MoveAttack NorthEast),
   ('l', Right $ MoveAttack East),
   ('n', Right $ MoveAttack SouthEast),
   ('j', Right $ MoveAttack South),
   ('b', Right $ MoveAttack SouthWest),
   ('h', Right $ MoveAttack West),
   ('>', Right $ UseStairs),
   ('.', Right $ Wait),
   ('q', Left $ SaveGame)]
