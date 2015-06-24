module Vessel where

import Color exposing (rgb)
import Drawing exposing (Circle, drawCircle)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (..)
import Maybe exposing (..)
import Time exposing (Time)

import Movement exposing (Moving, Positionned, move)
import Weapon exposing (Weaponed)

--| Model |---------------------------------------------------------------------

type alias Vessel = Weaponed (Moving (Circle {}))

default : Vessel
default =
  { position = vec2 0 0
  , velocity = vec2 0 0
  , speed = 5
  , destination = Nothing
  , radius = 20
  , stroke = 2
  , color = rgb 255 167 0
  , weapon = Weapon.default }

--| Update |--------------------------------------------------------------------

update : Time -> Vessel -> Vessel
update dt vessel = Movement.move dt vessel

setDestination : Vec2 -> Vessel -> Vessel
setDestination = Movement.setDestination

--| View |----------------------------------------------------------------------

draw : Vessel -> Form
draw = drawCircle
