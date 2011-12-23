module Terrain where

data Material =
    Stone

data Terrain =
    Wall Material
  | Floor Material
  | Pillar Material
  | Door Material DoorState

data DoorState =
    ClosedDoor
  | OpenDoor

renderTile :: Terrain -> Char
renderTile (Wall _) = '#'
renderTile (Pillar _) = 'O'
renderTile (Door _ OpenDoor) = '\''
renderTile (Door _ ClosedDoor) = '+'
renderTile (Floor _) = '.'

traversable :: Terrain -> Bool
traversable (Wall _) = False
traversable (Pillar _) = False
traversable (Floor _) = True
