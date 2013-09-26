module Draw where

import Actor.Map

import Box
import Canvas
import Cursor
import qualified Dungeon as D
import GameState
import Terrain
import Turn

import Actor
import UI.HSCurses.Curses (refresh, update)

draw :: GameState -> IO ()
draw (GameState am dgn now) = do 
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
            let isVis = case D.get dgn p of
                          Just (_, t) -> now == seenOn t
                          _ -> False
                asGlyph = case D.get dgn p of
                            Just (g, _) -> (renderTile g, isVis)
                            Nothing -> (' ', isVis)
                mob = actorAt am in
              if not isVis
              then asGlyph
              else case mob p of
                     Nothing -> asGlyph
                     Just a -> (glyph a, True)
