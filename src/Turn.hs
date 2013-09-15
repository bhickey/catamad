module Turn (
  Visibility (..)
  ,Turn
  ,never
  ,firstTurn
  ,nextTurn)
where

data Turn = Turn Integer | Never deriving (Eq, Ord)
newtype Visibility = Visibility { seenOn :: Turn }

instance Show Turn where
  show Never = "Never"
  show (Turn t) = "Turn " ++ show t

firstTurn :: Turn
firstTurn = Turn 0

never :: Turn
never = Never

nextTurn :: Turn -> Turn
nextTurn Never = Never
nextTurn (Turn t) = Turn (succ t)
