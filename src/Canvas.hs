module Canvas where

import Box
import qualified Box as B
import Point

import Control.Monad

import UI.HSCurses.Curses (Color, mvAddCh, scrSize)
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

data Canvas = Canvas
  { canvasOffset :: Point
  , canvasSize :: Box
  , canvasData :: Vector Char
  }

contains :: Canvas -> Point -> Bool
contains (Canvas _ (Box (xm, ym)) _) (Point (x,y)) =
  (x >= 0 && x < xm && y >= 0 && y < ym)

makeCanvas :: Point -> Box -> Char -> Canvas
makeCanvas p b c = Canvas
  p b (V.replicate (B.size b) c)

splitRow :: Int -> Canvas -> (Canvas, Canvas)
splitRow r (Canvas o@(Point (x_off, y_off)) (Box (x_lim, y_lim)) _) =
  (makeCanvas o (Box (x_lim, r)) 'a'
  ,makeCanvas (Point (x_off, y_off + r)) (Box (x_lim, y_lim - r))'b'
  )

splitRow' :: Int -> Canvas -> (Canvas, Canvas)
splitRow' r cv@(Canvas _ (Box (_, y)) _) = splitRow (y - r) cv

splitCol :: Int -> Canvas -> (Canvas, Canvas)
splitCol c (Canvas o@(Point (x_off, y_off)) (Box (x_lim, y_lim)) _) =
  (makeCanvas o (Box (c, y_lim)) 'a'
  ,makeCanvas (Point (x_off + c, y_off)) (Box (x_lim - c, y_lim)) 'b'
  )

splitCol' :: Int -> Canvas -> (Canvas, Canvas)
splitCol' c cv@(Canvas _ (Box (x, _)) _) = splitCol (x - c) cv
   
updateCanvas :: Canvas -> Point -> Char -> Canvas
updateCanvas (Canvas pt@(Point (x_off, y_off)) b@(Box (cols, _)) d) (Point (x,y)) c =
  Canvas pt b $
  d // [(toIndex b (Point (x - x_off, y - y_off)), c)]

bulkUpdateCanvas :: Canvas -> [(Point, Char)] -> Canvas
bulkUpdateCanvas (Canvas pt@(Point (x_off, y_off)) b@(Box (cols, _)) d) ptc =
  Canvas pt b $
  d // (map (\ (Point (x,y),c) -> (toIndex b (Point (x - x_off, y - y_off)), c)) ptc)

overwrite :: Canvas -> Char -> Canvas
overwrite (Canvas p b d) c = makeCanvas p b c

paint :: Canvas -> (Point -> Char) -> Canvas
paint (Canvas pt b d) fn =
  Canvas pt b $
  V.imap (\ x _ -> fn $ toPoint b x) d

printCanvas :: Canvas -> IO ()
printCanvas (Canvas (Point (x_off,y_off)) (Box (cols,_)) dat) = void $
  foldM 
  (\ index chr -> do
      mvAddCh (index2y index + y_off) (index2x index + x_off) (cast $ dat ! index)
      return (index + 1))
  0 (V.toList dat)
  where index2y ind = ind `div` cols
        index2x ind = ind `mod` cols

stdCanvas :: IO Canvas
stdCanvas = do
  (y, x) <- scrSize
  return $ makeCanvas zeroPoint (Box (x,y)) ' ' 

cast :: (Enum a, Enum b) => a -> b
cast = toEnum . fromEnum
