module Utils where

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

translate : (Int, Int) -> (Int, Int) -> (Int, Int)
translate (ox, oy) (x, y) =
  if |x == 0 && y == 0 -> (0, 0)
     | otherwise -> (x - ox, oy - y)
