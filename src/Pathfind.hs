module Pathfind where

import Direction
import Knowledge
import Point
import Point.Metric
import Terrain

import Prelude hiding (null)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.PQueue.Min (MinQueue, singleton, insert, deleteFindMin, null) 

type Search = Map Point Direction

backtrace :: Search -> Point -> Maybe Direction
backtrace s pt =
  if M.member next s
  then backtrace s next
  else case M.lookup pt s of
         Nothing -> Nothing
         Just d -> Just $ turnAround d
  where next = move pt (s ! pt)

pathfind :: Knowledge Terrain -> Point -> Point -> Maybe Direction 
pathfind k p1 p2 =
  if p1 == p2
  then Nothing
  else search k p2 (singleton (0, p1)) M.empty

search :: Knowledge Terrain
  -> Point
  -> MinQueue (Int, Point)
  -> Search
  -> Maybe Direction
search k target q result =
  if (null q)
  then Nothing
  else let ((_, p), q') = deleteFindMin q in
    if (p == target)
    then backtrace result p
    else let pts = makeNeighbors p in 
           search k target
                  (enqueueAllNeighbors pts target q')
                  (searchAllNeighbors p pts result)
    where makeNeighbors p = filter canPass $ filter (flip M.notMember result) (neighbors p)
          canPass p =
            case remember k p of
              Nothing -> False
              Just feat -> (traversable feat) 

enqueueAllNeighbors :: 
     [Point]
  -> Point
  -> MinQueue (Int, Point) 
  -> MinQueue (Int, Point) 
enqueueAllNeighbors pts t q =
  foldl insert' q $ map est $ pts
  where est pt = (estimate pt t, pt)
        insert' = flip insert

searchAllNeighbors :: Point -> [Point] -> Search -> Search
searchAllNeighbors p pts s =
  foldl insert' s $ map withDirection pts
  where withDirection k = (k, (directionTo k p))
        insert' acc (_, Nothing) = acc
        insert' acc (k, Just d) = M.insert k d acc

estimate :: Metric
estimate = chessDistance
