module Types (
  Visibility (..)
  ,Turn
  ,firstTurn
  ,nextTurn)
where

newtype Turn = Turn { turn :: Integer } deriving (Eq, Ord)
newtype Visibility = Visibility { seenOn :: Turn }

firstTurn :: Turn
firstTurn = Turn { turn = 0 }

nextTurn :: Turn -> Turn
nextTurn (Turn t) = Turn (t + 1)
