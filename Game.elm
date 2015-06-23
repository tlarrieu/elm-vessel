module Game where

import Color exposing (rgb)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Math.Vector2 exposing (distance, fromTuple)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Scrap exposing (Scrap)
import Utils exposing (center, translate, randomPoint)
import Vessel exposing (Vessel)

--| Model |---------------------------------------------------------------------

type Event = Move (Time, (Int, Int)) | Spawn Float
type alias Dimension = (Int, Int)
type alias Game = { vessel: Vessel , scraps: List Scrap }

background = (rgb 174 238 238)

default =
  { vessel = Vessel.default
  , scraps = [] }

--| Update |--------------------------------------------------------------------

update : Event -> Game -> Game
update event game  =
  case event of
    Move input -> move input game
    Spawn i -> spawn i game

move : (Time, (Int, Int)) -> Game -> Game
move (dt, (x, y)) game =
  let destination = fromTuple (toFloat x, toFloat y)
      vessel = Vessel.update (dt, destination) game.vessel
      dist = distance vessel.position
      rad = (+) vessel.radius
      hit scrap = dist scrap.position < rad scrap.radius
      scraps = List.filter (not << hit) game.scraps
  in  { game
      | vessel <- vessel
      , scraps <- scraps }

spawn : Float -> Game -> Game
spawn i game =
  let overcrowded = (List.length game.scraps) >= 100
      scrap = Scrap.new <| randomPoint <| round i
  in  if | overcrowded -> game
         | otherwise -> { game | scraps <- scrap  :: game.scraps }

--| View |----------------------------------------------------------------------

scene : Dimension -> Game -> Element
scene (w,h) game =
  let forms =
        List.concat
          [ [drawBackground (w, h)]
          , List.map Scrap.draw game.scraps
          , [Vessel.draw game.vessel] ]
      pos = middle
  in  container w h pos <| collage w h forms

drawBackground : Dimension -> Form
drawBackground (w, h) = rect (toFloat w) (toFloat h) |> filled background
