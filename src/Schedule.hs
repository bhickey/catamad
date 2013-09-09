module Schedule where

import Prelude hiding (null)
import Data.PQueue.Min

data Event a = Event
  { time :: Int,
    action :: a }

instance Eq (Event a) where
  (Event t1 _) == (Event t2 _) = t1 == t2

instance Ord (Event a) where
  (Event t1 _) `compare` (Event t2 _) = t1 `compare` t2

data Schedule a = Schedule 
  { queue :: MinQueue (Event a) }

addEvent :: Int -> a -> Schedule a -> Schedule a
addEvent t a q = Schedule $ insert (Event t a) (queue q)

getEvent :: Schedule a -> Maybe (Event a, Schedule a)
getEvent (Schedule q) = 
  if (null q)
  then Nothing
  else let (v, q') = deleteFindMin q in
    Just (v, Schedule q')
