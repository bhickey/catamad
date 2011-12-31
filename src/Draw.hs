module Draw where

import Box
import Canvas
import Point
import FOV
import Cursor
import Dungeon
import Direction
import Terrain
import UI.HSCurses.Curses (refresh, update, getCh, Key(..))

dungeon :: (Point -> Terrain)
dungeon = cavern

draw :: IO ()
draw = do
  draw_loop Nothing

draw_loop :: Maybe Cursor -> IO ()
draw_loop Nothing = do
  draw_loop (Just $! Point (15,15))

draw_loop (Just cr) = do 
  cv@(Canvas _ bx) <- stdCanvas
  let 
      lCenter = center bx
      offset = applyOffset (cr - lCenter) in
    printCanvas cv (\ p -> if isVisible cr dungeon (offset p)
                          then renderTile (dungeon $! offset p)
                          else ' ') >>
    writeTo cv lCenter >>
    refresh >> update >>
    do maybeCr <- (handleChar cr) 
       case maybeCr of
         Nothing -> return ()
         _ -> draw_loop maybeCr
  where applyOffset p1 p2 = p1 + p2
       
handleChar :: Cursor -> IO (Maybe Cursor)
handleChar cr = do
  keypress <- getCh
  case keypress of
    KeyChar 'q' -> return Nothing
    KeyChar 'k' -> (mv North)
    KeyChar 'u' -> (mv NorthEast)
    KeyChar 'l' -> (mv East)
    KeyChar 'n' -> (mv SouthEast)
    KeyChar 'j' -> (mv South)
    KeyChar 'b' -> (mv SouthWest)
    KeyChar 'h' -> (mv West)
    KeyChar 'y' -> (mv NorthWest)
    _ -> handleChar cr
  where mv d = let cr' = move cr d in
                 if traversable $! dungeon cr' 
                 then return $! Just cr'
                 else return $! Just cr
                
 
