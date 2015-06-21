module Scrap where

import Color exposing (Color, rgb)
import Drawable exposing (StrokeCircle)
import Graphics.Collage exposing (..)

--| Model |---------------------------------------------------------------------

type alias Scrap = StrokeCircle {}

default : Scrap
default =
  { x=-100
  , y=-100
  , color=rgb 255 0 190
  , radius=20
  , strike=2 }

new : (Float, Float) -> Scrap
new (x', y') = { default | x <- x', y <- y' }
