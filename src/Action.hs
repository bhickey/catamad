module Action where

import Direction

type Action = Either GameAction ActorAction

data GameAction = SaveGame
data ActorAction =
    Attack Direction
  | Move Direction
  | MoveAttack Direction
  | UseStairs
  | Wait
  | NoAction
  deriving (Eq, Show)
