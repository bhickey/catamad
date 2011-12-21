module Cursor where

import Canvas
import Direction
import Point
import UI.HSCurses.Curses (mvAddCh)

type Cursor = Point

moveIn :: Canvas -> Cursor -> Direction -> Cursor
moveIn cv cr dir =
  let cr' = move cr dir in
    if cv `contains` cr'
    then cr'
    else cr

writeTo :: Canvas -> Cursor -> IO ()
writeTo (Canvas off bx _) cr = do
  let Point (x,y) = cr + off in
    mvAddCh y x (toEnum.fromEnum $ '@')

