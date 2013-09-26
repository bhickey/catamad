module Schedule where

import Time

import Prelude hiding (null)
import Data.PQueue.Min (MinQueue, insert, deleteFindMin, null)
import qualified Data.PQueue.Min as PQ

data Event a = Event
  { time :: Time,
    action :: a }

instance Eq (Event a) where
  (Event t1 _) == (Event t2 _) = t1 == t2

instance Ord (Event a) where
  (Event t1 _) `compare` (Event t2 _) = t1 `compare` t2

data Schedule a = Schedule 
  { now :: Time,
    queue :: MinQueue (Event a) }

empty :: Schedule a
empty = (Schedule timeZero PQ.empty)

singleton :: Time -> a -> Schedule a
singleton t x = Schedule t (PQ.singleton (Event t x))

addEvent :: Time -> a -> Schedule a -> Schedule a
addEvent t a q = Schedule (now q) $ insert (Event (addTime t (now q)) a) (queue q)

getEvent :: Schedule a -> Maybe (Event a, Schedule a)
getEvent (Schedule t q) = 
  if (null q)
  then Nothing
  else let (e@(Event t' _), q') = deleteFindMin q in
    Just (e, Schedule (max t t') q')
