module Health where

data Health = Health
  { hpMax :: Int
  , hp :: Int
  }

data HealthStatus =
    Dead
  | AlmostDead
  | BadlyInjured
  | ModeratelyInjured
  | LightlyInjured
  | Uninjured
  deriving (Eq, Ord)

mkHealth :: Int -> Int -> Health
mkHealth mx n = Health mx n

heal :: Int -> Health -> Health
heal n (Health mx curr) =
  (Health mx (min mx (curr + n)))

hurt :: Int -> Health -> Health
hurt n (Health mx curr) =
  (Health mx (curr - n))

isDead :: Health -> Bool
isDead (Health _ n) = n <= 0

status :: Health -> HealthStatus
status (Health _ n) =
  if n <= 0
  then Dead
  else Uninjured
