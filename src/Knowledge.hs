module Knowledge where

import Point

import Data.Map (Map)
import qualified Data.Map as M

data Knowledge a = Knowledge (Map Point a)

learn :: Knowledge a -> Point -> a -> Knowledge a
learn (Knowledge m) p a =
  Knowledge $ M.insert p a m

lookup :: Knowledge a -> Point -> Maybe a
lookup (Knowledge m) p = M.lookup p m
