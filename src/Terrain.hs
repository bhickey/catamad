module Terrain where

data DoorState = Open | Closed deriving (Eq, Ord)

data Material =
    Stone
  | Bedrock deriving (Eq, Ord)

data Terrain =
    Floor Material
  | Door DoorState
  | Stairs
  | Pillar Material
  | Wall Material deriving (Eq, Ord)

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Pillar _) = 'O'
renderTile (Floor _) = '"'
renderTile (Door _) = '+'
renderTile Stairs = '>'

traversable :: Terrain -> Bool
traversable (Door _) = True
traversable (Floor _) = True
traversable Stairs = True
traversable _ = False

allowsVisibility :: Terrain -> Bool
allowsVisibility (Wall _) = False
allowsVisibility (Door Closed) = False
allowsVisibility _ = True
