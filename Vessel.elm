module Vessel where

import Color exposing (Color, rgb)
import Drawing exposing (..)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (..)
import Time exposing (Time)

type alias Moving = { velocity : Vec2 , speed: Float }
type alias Vessel = Circle Moving

--| Model |---------------------------------------------------------------------

default : Vessel
default =
  { position = vec2 0 0
  , velocity = vec2 0 0
  , speed = 5
  , radius = 20
  , stroke = 2
  , color = rgb 255 167 0 }

--| Update |--------------------------------------------------------------------

update : (Time, (Float, Float)) -> Vessel -> Vessel
update (dt, position) vessel =
  vessel
  |> updateVelocity position
  |> updatePosition dt

updateVelocity : (Float, Float) -> Vessel -> Vessel
updateVelocity (x, y) ({position, speed} as vessel) =
  let destination = vec2 x y
      isMoving = distance position destination >= 5
      actualSpeed = if isMoving then speed else 0
      (b, a) =  destination `sub` position |> toTuple
      α = atan2 a b
  in  { vessel | velocity <- scale actualSpeed (fromTuple (cos α, sin α)) }

updatePosition : Time -> Vessel -> Vessel
updatePosition dt ({position, velocity} as vessel) =
  let velocity' = scale dt velocity
  in  { vessel | position <- position `add` velocity' }

--| View |----------------------------------------------------------------------
draw : Vessel -> Form
draw = drawCircle
