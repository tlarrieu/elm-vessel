module Movement (Moving, Positionned, move) where

import Math.Vector2 exposing (..)
import Maybe exposing (..)
import Time exposing (Time)

type alias Moving a =
  { a
  | velocity : Vec2
  , speed : Float
  , destination : Maybe Vec2 }
type alias Positionned a = { a | position : Vec2 }

move : Time -> Moving (Positionned a) -> Moving (Positionned a)
move dt ({destination} as unit) =
  case destination of
    Just dest ->
      unit
      |> updateVelocity dest
      |> updatePosition dt dest
    Nothing -> unit

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
      position'' =
        if | deltaPos > deltaDest -> destination
           | otherwise -> position'
      destination' =
        if | position == position'' -> Nothing
           | otherwise -> Just destination
  in  { unit
      | position <- position''
      , destination <- destination' }
