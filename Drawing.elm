module Drawing where

import Color exposing (Color, rgb)
import Graphics.Collage exposing (..)
import Math.Vector2 exposing (Vec2, toTuple)

type alias Circle a =
  { a
  | position : Vec2
  , color: Color
  , radius: Float
  , stroke: Float }

drawCircle : Circle a -> Form
drawCircle { position, color, radius, stroke } =
  let size = round <| (radius + 2) * 2 + stroke
      shape = circle radius
      color' =  filled color shape
      stroke' = outlined { defaultLine | width <- stroke } shape
      form =
        collage size size [ color', stroke' ]
        |> toForm
  in  form
      |> alpha 0.8
      |> move (toTuple position)