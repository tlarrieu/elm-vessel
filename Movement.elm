module Movement (Moving, Positionned, moveTo) where

import Math.Vector2 exposing (..)
import Time exposing (Time)

type alias Moving a = { a | velocity : Vec2 , speed : Float }
type alias Positionned a = { a | position : Vec2 }

moveTo : (Time, Vec2) -> Moving (Positionned a) -> Moving (Positionned a)
moveTo (dt, destination) unit =
  unit
  |> updateVelocity destination
  |> updatePosition dt destination

updateVelocity : Vec2 -> Moving (Positionned a) -> Moving (Positionned a)
updateVelocity destination ({position, speed} as unit) =
  let (b, a) =  destination `sub` position |> toTuple
      α = atan2 a b
  in  { unit | velocity <- scale speed (fromTuple (cos α, sin α)) }

updatePosition : Time -> Vec2 -> Moving (Positionned a) -> Moving (Positionned a)
updatePosition dt destination ({position, velocity} as unit) =
  let velocity' = scale dt velocity
      position' = position `add` velocity'
      deltaPos = position' `sub` position |> length
      deltaDest = destination `sub` position |> length
      position'' = if deltaPos > deltaDest then destination else position'
  in  { unit | position <- position'' }
