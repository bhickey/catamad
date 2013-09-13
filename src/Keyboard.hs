module Keyboard where

import Data.Map (Map)
import qualified Data.Map as M

import Action
import Direction

type KeyboardMap a = Map Char a

getAction :: KeyboardMap Action -> Char -> Action
getAction m k = M.findWithDefault NoAction k m

defaultKeymap :: KeyboardMap Action
defaultKeymap = M.fromList $
  [('y', MoveAttack NorthWest),
   ('k', MoveAttack North),
   ('u', MoveAttack NorthEast),
   ('l', MoveAttack East),
   ('n', MoveAttack SouthEast),
   ('j', MoveAttack South),
   ('b', MoveAttack SouthWest),
   ('h', MoveAttack West),
   ('.', Wait),
   ('q', Quit)]
