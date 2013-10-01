module Data.Identifiable (Identifier, Identifiable(..), mkIdentifier) where

data Identifier a = Identifier Integer

mkIdentifier :: (Identifiable a) => a -> Identifier a
mkIdentifier = Identifier . identify

instance Identifiable a => Eq (Identifier a) where
  (Identifier x) == (Identifier y) = x == y

instance Identifiable a => Ord (Identifier a) where
  compare (Identifier x) (Identifier y) = compare x y

class Identifiable a where
  identify :: a -> Integer
