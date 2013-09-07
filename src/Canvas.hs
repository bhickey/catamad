module Canvas where

import Box
import Point

import Control.Monad

import UI.HSCurses.Curses (mvAddCh, scrSize, mvWAddStr, stdScr, attrDimOn, attrDimOff, attrBoldOn, attrBoldOff)

data Canvas = Canvas
  { canvasOffset :: Point
  , canvasSize :: Box
  }

contains :: Canvas -> Point -> Bool
contains (Canvas _ (Box (xm, ym))) (Point (x,y)) =
  (x >= 0 && x < xm && y >= 0 && y < ym)

splitRow :: Int -> Canvas -> (Canvas, Canvas)
splitRow r (Canvas o@(Point (x_off, y_off)) (Box (x_lim, y_lim))) =
  (Canvas o (Box (x_lim, r))
  ,Canvas (Point (x_off, y_off + r)) (Box (x_lim, y_lim - r))
  )

splitRow' :: Int -> Canvas -> (Canvas, Canvas)
splitRow' r cv@(Canvas _ (Box (_, y))) = splitRow (y - r) cv

splitCol :: Int -> Canvas -> (Canvas, Canvas)
splitCol c (Canvas o@(Point (x_off, y_off)) (Box (x_lim, y_lim))) =
  (Canvas o (Box (c, y_lim))
  ,Canvas (Point (x_off + c, y_off)) (Box (x_lim - c, y_lim))
  )

splitCol' :: Int -> Canvas -> (Canvas, Canvas)
splitCol' c cv@(Canvas _ (Box (x, _))) = splitCol (x - c) cv
   
printCanvas :: (Enum a, Eq a) => Canvas -> (Point -> (a,Bool)) -> IO ()
printCanvas (Canvas off b) pfn = void $
  mapM 
  (\ pt -> 
    let (ch,bright) = pfn pt
        ch' = if ch == (cast '"') && (not bright) then (cast '\'') else ch in
      do when bright attrBoldOn
         unless bright attrDimOn
         printCh (pt + off) (cast $ ch')
         unless bright attrDimOff
         when bright attrBoldOff)
  (indices b)
  where printCh (Point (x,y)) a = mvAddCh y x a

stdCanvas :: IO Canvas
stdCanvas = do
  return $ Canvas zeroPoint (Box (30,20))

cast :: (Enum a, Enum b) => a -> b
cast = toEnum . fromEnum

print_string :: Int -> Int -> String -> IO ()
print_string x y s = do
  (ymax, xmax) <- scrSize
  if y < 0 || y >= ymax || x >= xmax then return () else
    let x' = max 0 x
        s' = if x < 0 then drop (-x) s else s
        s''= take (xmax - x) s' in
      if y + 1 == ymax && x + length s'' == xmax
      then mvWAddStr stdScr y x' (init s'') >>
           mvAddCh y (xmax - 1) (cast $ last s'')
      else mvWAddStr stdScr y x' s''
