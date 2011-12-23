module Point where

import Direction

newtype Point = Point { point :: (Int, Int) } deriving (Show, Eq, Ord)

instance Num Point where
  fromInteger 0 = Point (0,0)
  fromInteger x' = 
    let x = fromIntegral x'
        r = ((floor (((sqrt.fromIntegral) x)::Double)) + 1) `div` 2 
        ring_start = (2 * r - 1)^(2::Int)
        top_bnd = ring_start + (2 * r + 1)
        rgt_bnd = ring_start + (4 * r + 1)
        bot_bnd = ring_start + (6 * r + 1)
        offset = x - ring_start in
      if x < top_bnd
      then Point (-r + offset,r)
      else if x < rgt_bnd
           then Point (r, top_bnd - x + r - 1)
           else if x < bot_bnd
                then Point (r - (x - rgt_bnd) - 1, -r)
                else Point (-r, x - bot_bnd - 1) 
  (Point (x1,y1)) + (Point (x2,y2)) = (Point ((x1 + x2), (y1 + y2)))
  (Point (x1,y1)) * (Point (x2,y2)) = (Point ((x1 * x2), (y1 * y2))) -- This makes no sense
  (Point (x1,y1)) - (Point (x2,y2)) = (Point ((x1 - x2), (y1 - y2)))
  abs (Point (x,y)) = (Point (abs x, abs y))
  signum (Point (x,y)) = (Point (signum x, signum y))

instance Real Point where
  toRational = toRational.toInteger

instance Enum Point where
  toEnum = fromInteger.fromIntegral
  fromEnum = fromIntegral.toInteger

instance Integral Point where
  quotRem (Point (x1,y1)) (Point (x2,y2)) =  -- do you have a better idea?
      (Point (quot x1 x2, quot y1 y2),
       Point (rem x1 x2, rem y1 y2))
  toInteger (Point (0,0)) = 0
  toInteger (Point (x,y)) = 
    let x' = fromIntegral x
        y' = fromIntegral y
        ring = max (abs x') (abs y')
        off = (2 * ring - 1)^(2::Int) in
      if ring == y'
      then x' + ring + off
      else if ring == (abs y')
           then (4 * ring) + off - x'
           else if x > 0
                then (2 * ring) + x' - y' + off
                else (5 * ring) + y' - (2 * x') + off

zeroPoint :: Point
zeroPoint = Point (0,0)

move :: Point -> Direction -> Point
move (Point (x,y)) North = Point (x, y - 1)
move (Point (x,y)) NorthEast = Point (x + 1, y - 1)
move (Point (x,y)) East = Point (x + 1, y)
move (Point (x,y)) SouthEast = Point (x + 1, y + 1)
move (Point (x,y)) South = Point (x, y + 1)
move (Point (x,y)) SouthWest = Point (x - 1, y + 1)
move (Point (x,y)) West = Point (x - 1, y)
move (Point (x,y)) NorthWest = Point (x - 1, y - 1)
