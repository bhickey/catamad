module Data.Identifiable (Identifier, Identifiable(..), mkIdentifier) where

data Identifier a = Identifier a

mkIdentifier :: (Identifiable a) => a -> Identifier a
mkIdentifier = Identifier

instance Identifiable a => Eq (Identifier a) where
  (Identifier x) == (Identifier y) = (identify x) == (identify y)

instance Identifiable a => Ord (Identifier a) where
  compare (Identifier x) (Identifier y) = compare (identify x) (identify y)

class Identifiable a where
  identify :: a -> Integer
