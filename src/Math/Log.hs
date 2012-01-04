module Math.Log where

discreteLg :: Int -> Int -> [Int]
discreteLg b p =
  snd $ foldl (\ (a,l) _ -> let r = (a * b) `mod` p in (r,r:l))
              (1,[]) [0..p-1]
