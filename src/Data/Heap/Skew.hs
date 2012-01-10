module Data.Heap.Skew
(SkewHeap, head, tail, merge, singleton, empty, null, insert)
where

import Prelude hiding (head, tail, null)

data (Ord k) => SkewHeap k v =
    SkewLeaf
  | SkewHeap 
  { key :: k
  , val :: v
  , left :: (SkewHeap k v)
  , right :: (SkewHeap k v)
  }
  deriving (Eq, Ord)

empty :: (Ord k) => SkewHeap k v
empty = SkewLeaf

null :: (Ord k) => SkewHeap k v -> Bool
null SkewLeaf = True
null _ = False

singleton :: (Ord k) => k -> v -> SkewHeap k v
singleton k v = SkewHeap k v SkewLeaf SkewLeaf

insert :: (Ord k) => k -> v -> SkewHeap k v -> SkewHeap k v
insert k v h = merge h (singleton k v)

merge :: (Ord k) => SkewHeap k v -> SkewHeap k v -> SkewHeap k v
merge SkewLeaf n = n
merge n SkewLeaf = n
merge h1 h2 = foldl1 assemble $ reverse $ listMerge (cutRight h1) (cutRight h2)

listMerge :: (Ord k) => [SkewHeap k v] -> [SkewHeap k v] -> [SkewHeap k v]
listMerge [] s = s
listMerge f [] = f
listMerge f@(h1:t1) s@(h2:t2) =
  if (fst.top) h1 <= (fst.top) h2
  then h1 : listMerge t1 s
  else h2 : listMerge f t2

cutRight :: (Ord k) => SkewHeap k v -> [SkewHeap k v]
cutRight SkewLeaf = []
cutRight (SkewHeap a v l r) = SkewHeap a v l SkewLeaf : cutRight r

-- assumes h1 >= h2, merge relies on this
assemble :: (Ord k) => SkewHeap k v -> SkewHeap k v -> SkewHeap k v
assemble h1 (SkewHeap k v l SkewLeaf) = SkewHeap k v h1 l
assemble _ _ = error "invalid heap assembly"

top :: (Ord k) => SkewHeap k v -> (k,v)
top SkewLeaf = error "head of empty heap"
top (SkewHeap k v _ _) = (k,v)

head :: (Ord k) => SkewHeap k v -> v
head = snd.top

tail :: (Ord k) => SkewHeap k v -> SkewHeap k v
tail SkewLeaf = error "tail of empty heap"
tail (SkewHeap _ _ l r) = merge l r
