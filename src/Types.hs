module Types (
  Visibility (..)
  ,Turn
  ,nextTurn)
where

newtype Turn = Turn { turn :: Integer }
newtype Visibility = Visibility { seenOn :: Turn }

nextTurn :: Turn -> Turn
nextTurn (Turn t) = Turn (t + 1)
