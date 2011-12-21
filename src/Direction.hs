 module Direction 
   (Direction (..))
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

instance Show Direction where
  show North     = "N"
  show NorthEast = "NE"
  show East      = "E"
  show SouthEast = "SE"
  show South     = "S"
  show SouthWest = "SW"
  show West      = "W"
  show NorthWest = "NW"
