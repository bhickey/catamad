module Terrain where

data Material =
    Stone
  | Bedrock deriving (Eq, Ord)

data Terrain =
    Floor Material
  | Stairs
  | Pillar Material
  | Wall Material deriving (Eq, Ord)

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Pillar _) = 'O'
renderTile (Floor _) = '"'
renderTile Stairs = '>'

traversable :: Terrain -> Bool
traversable (Floor _) = True
traversable Stairs = True
traversable _ = False

allowsVisibility :: Terrain -> Bool
allowsVisibility (Wall _) = False
allowsVisibility _ = True
