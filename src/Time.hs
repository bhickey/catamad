module Time (Time, timeZero, incTime, addTime) where

data Time = Time Integer deriving (Eq, Ord)

timeZero :: Time
timeZero = Time 0

incTime :: Time -> Integer -> Time
incTime (Time x) y = (Time $ x + y)

addTime :: Time -> Time -> Time
addTime (Time x) (Time y) = Time (x + y)
