module Weapon where

import Math.Vector2 exposing (distance)

import Drawing exposing (Circle)

type alias Weapon = { range : Float, quantity : Int }
type alias Weaponed a = { a | weapon : Weapon }

default = { range = 400, quantity = 3 }

inRange : Weaponed (Circle a) -> Circle b -> Bool
inRange source target =
  let dist = distance source.position target.position
      totalRadius = source.radius + target.radius
  in  dist - totalRadius < source.weapon.range

targets : Weaponed (Circle a) -> List (Circle b) -> List (Circle b)
targets source list =
  List.take source.weapon.quantity
  <| List.filter (inRange source) list
