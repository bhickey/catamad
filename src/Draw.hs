module Draw where

import Box
import Canvas
import Point
import Cursor
import Grid
import Direction
import Terrain
import Data.Maybe
import UI.HSCurses.Curses (refresh, update, getCh, Key(..))

dungeon :: (Point -> Terrain)
dungeon = circularRoom

draw :: IO ()
draw = do
  draw_loop Nothing

draw_loop :: Maybe Cursor -> IO ()
draw_loop Nothing = do
  (Canvas _ bx) <- stdCanvas
  draw_loop (Just $! Point (15,15))

draw_loop (Just cr) = do 
  cv <- stdCanvas
  let (t,b) = splitRow'  6 cv
      (l@(Canvas _ bx),r) = splitCol' 40 t 
      lCenter = center bx
      offset = applyOffset (cr - lCenter) in
    printCanvas l (\ p -> renderTile (dungeon $! offset p)) >>
    printCanvas r (\ _ -> 'R') >>
    printCanvas b (\ _ -> 'B') >>
    writeTo l lCenter >>
    refresh >> update >>
    do maybeCr <- (handleChar l cr) 
       case maybeCr of
         Nothing -> return ()
         _ -> draw_loop maybeCr
  where applyOffset p1 p2 = p1 + p2
       
handleChar :: Canvas -> Cursor -> IO (Maybe Cursor)
handleChar cv cr = do
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
    _ -> handleChar cv cr
  where mv d = let cr' = move cr d in
                 if traversable $! dungeon cr' 
                 then return $! Just cr'
                 else return $! Just cr
                
 
