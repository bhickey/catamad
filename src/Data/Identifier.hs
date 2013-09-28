{-# LANGUAGE GADTs #-}
module Data.Identifier where

import Data.Unique

data Identifier a where
  Identifier :: Identifiable a => a -> Identifier a

instance Eq (Identifier a) where
  (Identifier x) == (Identifier y) = (identify x) == (identify y)

instance Ord (Identifier a) where
  compare (Identifier x) (Identifier y) = compare (identify x) (identify y)

class Identifiable a where
  identify :: a -> Unique
