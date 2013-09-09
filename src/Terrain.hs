module Terrain where

data Material =
    Stone
  | Bedrock deriving (Eq, Ord)

data Terrain =
    Floor Material
  | Pillar Material
  | Wall Material deriving (Eq, Ord)

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Pillar _) = 'O'
renderTile (Floor _) = '"'

traversable :: Terrain -> Bool
traversable (Floor _) = True
traversable _ = False

allowsVisibility :: Terrain -> Bool
allowsVisibility (Floor _) = True
allowsVisibility _ = False
