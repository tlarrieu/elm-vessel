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
import Bullet exposing (Bullet)

--| Model |---------------------------------------------------------------------

type Event =
  Fire Time
  | Move (Int, Int)
  | Refresh Time
  | Spawn Float
type alias Dimension = (Int, Int)
type alias Game =
  { vessel: Vessel
  , scraps: List Scrap
  , bullets: List Bullet }

background = (rgb 174 238 238)

default =
  { vessel = Vessel.default
  , scraps = []
  , bullets = []}

--| Update |--------------------------------------------------------------------

update : Event -> Game -> Game
update event game  =
  case event of
    Fire t -> fire game
    Move input -> move input game
    Refresh t -> refresh t game
    Spawn i -> spawn i game

fire : Game -> Game
fire ({vessel, scraps, bullets} as game) =
  let bullets' =
        List.map (\ scrap -> Bullet.new vessel.position scrap.position) scraps
  in  { game | bullets <- List.append bullets bullets' }

move : (Int, Int) -> Game -> Game
move (x, y) game =
  let destination = fromTuple (toFloat x, toFloat y)
  in  { game
      | vessel <- Vessel.setDestination destination game.vessel }

collision : Bullet -> Scrap -> Bool
collision b s = distance b.position s.position < b.radius + s.radius

refresh : Time -> Game -> Game
refresh dt ({vessel, scraps, bullets } as game) =
  let vessel' = Vessel.update dt vessel
      hit bullet = List.any (collision bullet) scraps
      bullets' = List.map (Bullet.update dt) <| List.filter (not << hit) bullets
  in  { game
      | vessel <- vessel'
      , bullets <- bullets' }

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
          , List.map Bullet.draw game.bullets
          , [Vessel.draw game.vessel] ]
      pos = middle
  in  container w h pos <| collage w h forms

drawBackground : Dimension -> Form
drawBackground (w, h) = rect (toFloat w) (toFloat h) |> filled background
