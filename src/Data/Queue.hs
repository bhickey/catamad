{- |
Author       : Brendan Hickey
License      : PublicDomain
Stability    : experimental
-}

module Data.Queue 
  ( Queue
  , enqueue
  , enqueueAll
  , dequeue
  , peek
  , null
  , empty
  , singleton
  , fromList 
  , fromSequence
  ) where

import Prelude hiding (seq, drop, null)
import Data.Sequence ((|>), (><), Seq, drop, index)
import qualified Data.Sequence as S

newtype Queue a = Queue { seq :: Seq a }

enqueue :: Queue a -> a -> Queue a
enqueue (Queue s) e = Queue (s |> e)

enqueueAll :: Queue a -> [a] -> Queue a
enqueueAll q l = Queue (seq q >< (S.fromList l))

dequeue :: Queue a -> Queue a
dequeue (Queue s) = (Queue $ drop 1 s)

peek :: Queue a -> a
peek (Queue s) = index s 0

null :: Queue a -> Bool
null = S.null . seq

empty :: Queue a
empty = Queue S.empty

singleton :: a -> Queue a
singleton e = Queue (S.singleton e)

fromList :: [a] -> Queue a
fromList l = Queue (S.fromList l)

fromSequence :: Seq a -> Queue a
fromSequence = Queue
