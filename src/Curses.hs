module Curses where

import qualified UI.HSCurses.Curses as C
print_string :: Int -> Int -> String -> IO ()
print_string x y s = do
  (ymax, xmax) <- C.scrSize
  if y < 0 || y >= ymax || x >= xmax then return () else
    let x' = max 0 x
        s' = if x < 0 then drop (-x) s else s
        s''= take (xmax - x) s' in
      if y + 1 == ymax && x + length s'' == xmax
      then C.mvWAddStr C.stdScr y x' (init s'') >>
           C.mvAddCh y (xmax - 1) (cast $ last s'')
      else C.mvWAddStr C.stdScr y x' s''      

cast :: (Enum a, Enum b) => a -> b
cast = toEnum . fromEnum     

runMain :: IO () -> IO ()
runMain main = do
    C.initCurses
    C.keypad C.stdScr True
    C.echo False
    _ <- C.cursSet C.CursorInvisible
    C.cBreak True
    main
    C.endWin
