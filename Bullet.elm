module Bullet where

import Color exposing (rgb)
import Circle exposing (Circle)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (..)
import Maybe exposing (..)
import Time exposing (Time)

import Movement exposing (Moving, Positionned, move)

--| Model |---------------------------------------------------------------------

type alias Bullet = Moving (Circle {})

default : Bullet
default =
  { position = vec2 0 0
  , velocity = vec2 0 0
  , speed = 8
  , destination = Nothing
  , radius = 5
  , stroke = 1
  , color = rgb 205 2 0 }

new : Vec2 -> Vec2 -> Bullet
new pos dest =
  { default | position <- pos }
  |> setDestination dest

reachedTarget : Bullet -> Bool
reachedTarget bullet =
  case bullet.destination of
    Just dest -> False
    Nothing -> True

--| Update |--------------------------------------------------------------------

update : Time -> Bullet -> Bullet
update dt bullet = Movement.move dt bullet

setDestination : Vec2 -> Bullet -> Bullet
setDestination = Movement.setDestination

--| View |----------------------------------------------------------------------

draw : Bullet -> Form
draw = Circle.draw
