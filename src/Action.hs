module Action where

import Direction

data Action =
    Attack Direction
  | Move Direction
  | MoveAttack Direction
  | Wait
  | Quit
  | NoAction
  deriving (Eq, Show)
