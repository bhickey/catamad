module Canvas where

import Box
import Point

import Control.Monad

import UI.HSCurses.Curses (mvAddCh, scrSize)

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
   
printCanvas :: (Enum a) => Canvas -> (Point -> a) -> IO ()
printCanvas (Canvas off b) pfn = void $
  mapM 
  (\ pt -> do printCh (pt + off) (cast $ pfn pt))
  (indices b)
  where printCh (Point (x,y)) a = mvAddCh y x a

stdCanvas :: IO Canvas
stdCanvas = do
  (y, x) <- scrSize
  return $ Canvas zeroPoint (Box (x,y))

cast :: (Enum a, Enum b) => a -> b
cast = toEnum . fromEnum
