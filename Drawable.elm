module Drawable where

import Color exposing (Color, rgb)
import Graphics.Collage exposing (..)

type alias StrokeCircle a =
  { a
  | x: Float
  , y: Float
  , color: Color
  , radius: Float
  , strike: Float }

draw : StrokeCircle a -> Form
draw drawable =
  let size = round <| (drawable.radius + 2) * 2 + drawable.strike
      shape = circle drawable.radius
      color =  filled drawable.color shape
      stroke = outlined { defaultLine | width <- drawable.strike } shape
      form =
        collage size size [ color, stroke ]
        |> toForm
  in  form
      |> alpha 0.8
      |> move (drawable.x, drawable.y)
