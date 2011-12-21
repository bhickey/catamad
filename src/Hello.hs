module Hello where

import Canvas
import Grid
import UI.HSCurses.Curses (refresh, update, getCh)

helloWorld :: IO ()
helloWorld = 
  do cv <- stdCanvas
     let (t,b) = splitRow'  6 cv
         (l,r) = splitCol' 40 t
         l' = paint l charGrid
         r' = overwrite r 'R'
         b' = overwrite b 'B' in
      printCanvas l' >>
      printCanvas r' >>
      printCanvas b' >>
      refresh >> update >> getCh >> update
