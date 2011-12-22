module Draw where

import Canvas
import Point
import Cursor
import Grid
import Direction
import Terrain
import Data.Maybe
import UI.HSCurses.Curses (refresh, update, getCh, Key(..))

draw :: IO ()
draw = do
  cv <- stdCanvas
  draw_loop cv (Point (0,0))

draw_loop :: Canvas -> Cursor -> IO ()
draw_loop cv cr = do 
  let (t,b) = splitRow'  6 cv
      (l,r) = splitCol' 40 t in
    printCanvas l (\ p -> renderTile (charGrid p)) >>
    printCanvas r (\ _ -> 'R') >>
    printCanvas b (\ _ -> 'B') >>
    writeTo l cr >>
    refresh >> update >>
    do maybeCr <- (handleChar l cr) 
       case maybeCr of
         Nothing -> return ()
         Just cr' -> draw_loop cv cr'
       
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
  where mv d = let cr' = moveIn cv cr d in
                 if traversable $ charGrid cr' 
                 then return $ Just cr'
                 else return $ Just cr
                
 
