module QuadTree.Box
  ( Box
  , new
  , inside
  , subdivide ) where

type alias Box =
  { height : Float
  , center : (Float, Float)
  , width : Float }


new : (Float, Float) -> Float -> Float -> Box
new point height width =
  { center = point, height = height, width = width }


inside : Box -> (Float, Float) -> Bool
inside { height, width, center } (x, y) =
  let (x', y') = center
      width' = width / 2
      height' = height / 2
  in  insideInterval x (x' - width', x' + width') &&
      insideInterval y (y' - width', y' + height')


subdivide : Box -> (Box, Box, Box, Box)
subdivide { height, width, center } =
  let width' = width / 2
      height' = height / 2
      (x,y) = center
      newBox point =
        { height = height', width = width', center = point }
      ne = newBox (x + width', y + height')
      nw = newBox (x - width', y + height')
      sw = newBox (x - width', y - height')
      se = newBox (x + width', y - height')
  in (ne, nw, sw, se)

--| Helpers |-------------------------------------------------------------------

insideInterval : Float -> (Float, Float) -> Bool
insideInterval i (a,b) = a <= i && i < b
