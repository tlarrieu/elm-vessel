module Movement
  ( Moving
  , Positionned
  , anyCollision
  , collision
  , move
  , setDestination ) where

import Math.Vector2 exposing (..)
import Maybe exposing (..)
import Time exposing (Time)

type alias Moving a =
  { a
  | velocity : Vec2
  , speed : Float
  , destination : Maybe Vec2 }
type alias Positionned a = { a | position : Vec2 }
type alias Collidable a = { a | position : Vec2, radius : Float }

collision : Collidable a -> Collidable b -> Bool
collision c1 c2 = distance c1.position c2.position < c1.radius + c2.radius

anyCollision : List (Collidable t) -> Collidable u -> Bool
anyCollision list c = List.any (collision c) list

setDestination : Vec2 -> Moving (Positionned a) -> Moving (Positionned a)
setDestination destination ({position} as unit) =
  let destination' =
    if | position == destination -> Nothing
       | otherwise -> Just destination
  in { unit | destination <- destination' }

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
