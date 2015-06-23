module Vessel where

import Color exposing (rgb)
import Drawing exposing (Circle, drawCircle)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (..)
import Time exposing (Time)

--| Model |---------------------------------------------------------------------

type alias Moving = { velocity : Vec2 , speed: Float }
type alias Vessel = Circle Moving

default : Vessel
default =
  { position = vec2 0 0
  , velocity = vec2 0 0
  , speed = 5
  , radius = 20
  , stroke = 2
  , color = rgb 255 167 0 }

--| Update |--------------------------------------------------------------------

update : (Time, Vec2) -> Vessel -> Vessel
update (dt, destination) vessel =
  vessel
  |> updateVelocity destination
  |> updatePosition dt destination

updateVelocity : Vec2 -> Vessel -> Vessel
updateVelocity destination ({position, speed} as vessel) =
  let (b, a) =  destination `sub` position |> toTuple
      α = atan2 a b
  in  { vessel | velocity <- scale speed (fromTuple (cos α, sin α)) }

updatePosition : Time -> Vec2 -> Vessel -> Vessel
updatePosition dt destination ({position, velocity} as vessel) =
  let velocity' = scale dt velocity
      position' = position `add` velocity'
      deltaPos = position' `sub` position |> length
      deltaDest = destination `sub` position |> length
      position'' = if deltaPos > deltaDest then destination else position'
  in  { vessel | position <- position'' }

--| View |----------------------------------------------------------------------
draw : Vessel -> Form
draw = drawCircle
