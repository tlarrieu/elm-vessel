module Vessel where

import Color exposing (Color, rgb)
import Drawable exposing (StrokeCircle)
import Graphics.Collage exposing (..)
import Time exposing (Time)

type alias Moving =
  { vx: Float
  , vy: Float
  , speed: Float }
type alias Vessel = StrokeCircle Moving

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

update : (Time, (Float, Float)) -> Vessel -> Vessel
update (dt, position) vessel =
  vessel
  |> updateVelocity position
  |> updatePosition dt

speed : (Float, Float) -> Vessel -> Float
speed (tx, ty) {x, y, speed} =
  let distinct a b = (abs <| a - b) >= 5
      isMoving = distinct tx x || distinct ty y
  in  if isMoving then speed else 0

updateVelocity : (Float, Float) -> Vessel -> Vessel
updateVelocity (tx, ty) ({x,y} as vessel) =
  let v = speed (tx, ty) vessel
      diff a b = (a - b)
      α = atan2 (diff ty y) (diff tx x)
  in  { vessel
      | vx <- v * cos α
      , vy <- v * sin α }

updatePosition : Time -> Vessel -> Vessel
updatePosition dt ({x,y,vx,vy} as vessel) =
  let shift a b = a + dt * b
  in  { vessel
      | x <- shift x vx
      , y <- shift y vy }
