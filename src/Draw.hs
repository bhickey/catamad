module Draw where

import Box
import Canvas
import Point
import FOV
import Cursor
import Dungeon
import Direction
import Monster
import Terrain
import Turn
import GameState
import UI.HSCurses.Curses (refresh, update, getCh, Key(..))

import Data.Map (member, (!))

draw :: IO ()
draw = do
  draw_loop newState

draw_loop :: LevelState -> IO ()
draw_loop (LevelState (cr, you) them dgn now) = do 
  cv@(Canvas _ bx) <- stdCanvas
  let center = centerPt bx
      offset = cr - center
      dgn' = doFov now dgn bx cr in
    printCanvas cv (\ p -> renderFn dgn' (p + offset)) >>
    writeTo cv center >>
    print_string 0 0 (show cr) >>
    refresh >> update >>
    do maybeCr <- (handleChar dgn' cr) 
       case maybeCr of
         Nothing -> return ()
         Just cr' -> draw_loop (LevelState (cr', you) them dgn' (nextTurn now))
    where renderFn d p =
            let isVis = case get d p of
                          Just (_, t) -> now == seenOn t
                          _ -> False
                isYou = p == cr
                isThem = member p them in
              if isYou
              then (glyph you, True)
              else if isVis && isThem
                   then (glyph $ them ! p, True)
                   else case get d p of
                          Just (g, _) -> (renderTile g, isVis)
                          Nothing -> (' ', False)
 
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
