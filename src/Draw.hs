module Draw where

import Actor
import Box
import Canvas
import Cursor
import qualified Dungeon as D
import GameState
import Point
import Terrain

import Entity.Map
import Data.Set
import UI.HSCurses.Curses (refresh, update)

draw :: Set Point -> GameState -> IO ()
draw pts (GameState am dgn now) = do 
  cv@(Canvas _ bx) <- stdCanvas
  let (_, cr) = getPlayer am
      center = centerPt bx
      offset = cr - center in
    printCanvas cv (\ p -> renderFn (p + offset)) >>
    writeTo cv center >>
    print_string 0 0 (show now) >>
    refresh >> update >>
    return ()
    where renderFn p =
            let isVis = member p pts
                asGlyph = (renderTile $ D.get dgn p, isVis)
                mob = entityAt am in
              if not isVis
              then asGlyph
              else case mob p of
                     Nothing -> asGlyph
                     Just a -> (glyph a, True)
