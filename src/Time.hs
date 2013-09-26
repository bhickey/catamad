module Time (Time, timeZero, incTime, mkTime, addTime) where

data Time = Time Integer deriving (Eq, Ord, Show)

timeZero :: Time
timeZero = Time 0

mkTime :: Integer -> Time
mkTime t = Time t

incTime :: Time -> Integer -> Time
incTime (Time x) y = (Time $ x + y)

addTime :: Time -> Time -> Time
addTime (Time x) (Time y) = Time (x + y)
