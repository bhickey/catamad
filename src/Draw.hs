module Draw where

import Box
import Canvas
import Cursor
import Dungeon
import GameState
import Terrain
import Turn

import Data.Map (member, (!))
import Monster
import UI.HSCurses.Curses (refresh, update)

draw :: GameState -> IO ()
draw (GameState _ (cr, you) them dgn now) = do 
  cv@(Canvas _ bx) <- stdCanvas
  let center = centerPt bx
      offset = cr - center in
    printCanvas cv (\ p -> renderFn dgn (p + offset)) >>
    writeTo cv center >>
    print_string 0 0 (show now) >>
    refresh >> update >>
    return ()
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
