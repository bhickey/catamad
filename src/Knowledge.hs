module Knowledge (Knowledge, empty, isKnown, learn, learnAll, remember) where

import Point

import Data.Map (Map)
import qualified Data.Map as M

data Knowledge a = Knowledge (Map Point a)

empty :: Knowledge a
empty = Knowledge $ M.empty

learn :: Knowledge a -> (Point, a) -> Knowledge a
learn (Knowledge m) (p, a) =
  Knowledge $ M.insert p a m

learnAll :: Knowledge a -> [(Point, a)] -> Knowledge a
learnAll knowledge = foldl learn knowledge

isKnown :: Knowledge a -> Point -> Bool
isKnown (Knowledge m) p = M.member p m

remember :: Knowledge a -> Point -> Maybe a
remember (Knowledge m) p = M.lookup p m
