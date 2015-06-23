module Utils where

import Random

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

translate : (Int, Int) -> (Int, Int) -> (Int, Int)
translate (ox, oy) (x, y) =
  if |x == 0 && y == 0 -> (0, 0)
     | otherwise -> (x - ox, oy - y)

randomPoint : Int -> (Float, Float)
randomPoint a =
  let seed = Random.initialSeed a
      randX = Random.float -500 500
      randY = Random.float -400 400
      (pair, _) = Random.generate (Random.pair randX randY) seed
  in  pair

