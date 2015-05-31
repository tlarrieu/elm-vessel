module Vessel where

import Color exposing (Color, rgb)
import Graphics.Collage exposing (..)
import Time exposing (Time)

type alias Vessel =
  { x: Int
  , y: Int
  , vx: Float
  , vy: Float
  , speed: Float
  , color: Color
  , radius: Float
  , strike: Float }

--| Model |---------------------------------------------------------------------

default : Vessel
default =
  { x=0
  , y=0
  , vx=0
  , vy=0
  , speed=5
  , radius=30
  , strike=4
  , color=rgb 255 167 0 }

--| Update |--------------------------------------------------------------------

update : (Time, (Int, Int)) -> Vessel -> Vessel
update (dt, position) vessel =
  vessel
  |> updateVelocity position
  |> updatePosition dt

speed : (Int, Int) -> Vessel -> Float
speed (tx, ty) {x, y, speed} =
  let distinct a b = (abs <| a - b) >= 5
      isMoving = distinct tx x || distinct ty y
  in  if isMoving then speed else 0

updateVelocity : (Int, Int) -> Vessel -> Vessel
updateVelocity (tx, ty) ({x,y} as vessel) =
  let v = speed (tx, ty) vessel
      diff a b = toFloat (a - b)
      α = atan2 (diff ty y) (diff tx x)
  in  { vessel
      | vx <- v * cos α
      , vy <- v * sin α }

updatePosition : Time -> Vessel -> Vessel
updatePosition dt ({x,y,vx,vy} as vessel) =
  let shift a b = round <| (toFloat a) + dt * b
  in  { vessel
      | x <- shift x vx
      , y <- shift y vy }

--| View |----------------------------------------------------------------------

draw : Vessel -> Form
draw vessel =
  let size = round <| (vessel.radius + 2) * 2 + vessel.strike
      shape = circle vessel.radius
      color =  filled vessel.color shape
      stroke = outlined { defaultLine | width <- vessel.strike } shape
      form =
        collage size size [ color, stroke ]
        |> toForm
  in
      form
      |> alpha 0.8
      |> move (toFloat vessel.x, toFloat vessel.y)
