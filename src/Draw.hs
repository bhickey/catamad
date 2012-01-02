module Draw where

import Box
import Canvas
import Point
import FOV
import Cursor
import Dungeon
import Direction
import Terrain
import Types
import UI.HSCurses.Curses (refresh, update, getCh, Key(..))

draw :: IO ()
draw = do
  draw_loop firstTurn circularRoom Nothing

draw_loop :: Turn -> Dungeon Terrain -> Maybe Cursor -> IO ()
draw_loop t d Nothing = do
  draw_loop t d (Just $! Point (0,0))

draw_loop t d (Just cr) = do 
  cv@(Canvas _ bx) <- stdCanvas
  let center = centerPt bx
      offset = cr - center
      d' = doFov t d bx cr in
    printCanvas cv (\ p -> case get d' (p + offset) of 
                             Just (g, turn) -> if t == seenOn turn
                                             then renderTile g
                                             else ' '
                             Nothing -> ' ') >>
    writeTo cv center >>
    print_string 0 0 (show cr) >>
    refresh >> update >>
    do maybeCr <- (handleChar d' cr) 
       case maybeCr of
         Nothing -> return ()
         _ -> draw_loop (nextTurn t) d' maybeCr
       
handleChar :: Dungeon Terrain -> Cursor -> IO (Maybe Cursor)
handleChar dungeon cr = do
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
    _ -> handleChar dungeon cr
  where mv d = let cr' = move cr d in
                 if traversable $! (unconditionalGet dungeon cr' )
                 then return $! Just cr'
                 else return $! Just cr
