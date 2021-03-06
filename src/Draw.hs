module Draw where

import Actor
import Box
import Canvas
import Cursor
import qualified Dungeon as D
import GameState
import Point
import Terrain
import Knowledge

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
  where knowledge = actorKnow $ fst $ getPlayer am
        renderFn p =
          let isVis = member p pts
              known = isKnown knowledge p
              asGlyph = (renderTile $ D.get dgn p, isVis)
              mob = entityAt am in
            if not isVis
            then if known 
                 then asGlyph
                 else (' ', False)
            else case mob p of
                   Nothing -> asGlyph
                   Just a -> (glyph a, True)
