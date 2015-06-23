module Vessel where

import Color exposing (rgb)
import Drawing exposing (Circle, drawCircle)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (..)
import Time exposing (Time)

import Movement exposing (Moving, Positionned, moveTo)

--| Model |---------------------------------------------------------------------

type alias Vessel = Moving Circle

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
update = Movement.moveTo

--| View |----------------------------------------------------------------------
draw : Circle -> Form
draw = drawCircle
