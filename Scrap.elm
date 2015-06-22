module Scrap where

import Color exposing (Color, rgb)
import Drawing exposing (..)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (vec2)

--| Model |---------------------------------------------------------------------

type alias Scrap = Circle {}

default : Scrap
default =
  { position = vec2 -100 -100
  , color = rgb 255 0 190
  , radius = 15
  , stroke = 1 }

new : (Float, Float) -> Scrap
new (x, y) = { default | position <- vec2 x y }

--| View |----------------------------------------------------------------------
draw : Scrap -> Form
draw = drawCircle
