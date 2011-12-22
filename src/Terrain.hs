module Terrain where

data Material =
    Stone

data Terrain =
    Wall Material
  | Floor Material

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Floor _) = '.'

traversable :: Terrain -> Bool
traversable (Wall _) = False
traversable (Floor _) = True
