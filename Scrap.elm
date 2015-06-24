module Scrap where

import Color exposing (rgb)
import Drawing exposing (Circle, drawCircle)
import Graphics.Collage exposing (Form)
import Math.Vector2 exposing (vec2, distance)

import Bullet exposing (Bullet)

--| Model |---------------------------------------------------------------------

type alias Scrap = Circle { life : Int }

default : Scrap
default =
  { position = vec2 -100 -100
  , color = rgb 255 0 190
  , radius = 15
  , stroke = 1
  , life = 4 }

new : (Float, Float) -> Scrap
new (x, y) = { default | position <- vec2 x y }

--| Update |--------------------------------------------------------------------

damage : Scrap -> Scrap
damage ({life} as scrap) = { scrap | life <- life - 1 }

dead : Scrap -> Bool
dead scrap = scrap.life == 0

--| View |----------------------------------------------------------------------

draw : Scrap -> Form
draw = drawCircle
