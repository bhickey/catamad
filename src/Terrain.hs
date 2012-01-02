module Terrain where

data Material =
    Stone
  | Bedrock

data Terrain =
    Wall Material
  | Floor Material
  | Pillar Material

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Pillar _) = 'O'
renderTile (Floor _) = '.'

traversable :: Terrain -> Bool
traversable (Wall _) = False
traversable (Pillar _) = False
traversable (Floor _) = True

allowsVisibility :: Terrain -> Bool
allowsVisibility (Wall _) = False
allowsVisibility (Pillar _) = False
allowsVisibility (Floor _) = True
