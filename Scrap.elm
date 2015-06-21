module Scrap where

import Color exposing (Color, rgb)
import Graphics.Collage exposing (..)

--| Model |---------------------------------------------------------------------

type alias Scrap =
  { x: Float
  , y: Float
  , color: Color
  , radius: Float
  , strike: Float }

default : Scrap
default =
  { x=-100
  , y=-100
  , color=rgb 255 0 190
  , radius=20
  , strike=2 }

new : (Float, Float) -> Scrap
new (x', y') = { default | x <- x', y <- y' }

--| Update |--------------------------------------------------------------------

--| View |----------------------------------------------------------------------

draw : Scrap -> Form
draw scrap =
  let size = round <| (scrap.radius + 2) * 2 + scrap.strike
      shape = circle scrap.radius
      color =  filled scrap.color shape
      stroke = outlined { defaultLine | width <- scrap.strike } shape
      form =
        collage size size [ color, stroke ]
        |> toForm
  in  form
      |> alpha 0.8
      |> move (scrap.x, scrap.y)
