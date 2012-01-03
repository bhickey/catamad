 module Direction 
   (Direction (..)
   , turnLeft
   , turnRight
   , turnAround)
where

data Direction = 
    North 
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  deriving (Bounded, Eq, Enum, Ord)

turnLeft :: Direction -> Direction
turnLeft North = NorthWest
turnLeft d = pred d

turnRight :: Direction -> Direction
turnRight NorthWest = North
turnRight d = succ d

turnAround :: Direction -> Direction
turnAround d = (turnLeft.turnLeft.turnLeft.turnLeft) d

instance Show Direction where
  show North     = "N"
  show NorthEast = "NE"
  show East      = "E"
  show SouthEast = "SE"
  show South     = "S"
  show SouthWest = "SW"
  show West      = "W"
  show NorthWest = "NW"
