module Game where

import Color exposing (rgb)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Math.Vector2 as Vec exposing (add, fromTuple, toTuple)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Bullet exposing (Bullet)
import Movement exposing (Collidable)
import Scrap exposing (Scrap)
import Utils exposing (center, translate, randomPoint, pairMap)
import Vessel exposing (Vessel)
import Weapon

import QuadTree.QuadTree as QuadTree exposing (QuadTree)
import QuadTree.Box as Box

--| Model |---------------------------------------------------------------------

type Event =
  Click (Int, Int)
  | Fire Bool
  | Keys (Int, Int)
  | Refresh Time
  | Spawn Float
type alias Dimension = (Int, Int)
type alias Game =
  { vessel: Vessel
  , scraps: List Scrap
  , bullets: List Bullet
  , quadtree : QuadTree Scrap }

background = (rgb 174 238 238)

default =
  { vessel = Vessel.default
  , scraps = []
  , bullets = []
  , quadtree = QuadTree.empty (Box.new (0, 0) 1000 600)  5 }

--| Update |--------------------------------------------------------------------

update : Event -> Game -> Game
update event ({vessel} as game)  =
  case event of
    Fire t -> fire game
    Click dest -> move dest game
    Keys direction ->
      let movement =
            Vec.scale vessel.speed
            <| fromTuple
            <| pairMap toFloat direction
          dest = pairMap round <| toTuple <| vessel.position `add` movement
      in  move dest game
    Refresh t -> refresh t game
    Spawn i -> spawn i game

fire : Game -> Game
fire ({vessel, scraps, bullets} as game) =
  let scraps' = Weapon.targets vessel scraps
      bullets' =
        List.map (\scrap -> Bullet.new vessel.position scrap.position) scraps'
      bullets'' =  List.append bullets bullets'
  in  { game | bullets <- bullets'' }

move : (Int, Int) -> Game -> Game
move (x, y) game =
  let destination = fromTuple (toFloat x, toFloat y)
  in  { game
      | vessel <- Vessel.setDestination destination game.vessel }

refresh : Time -> Game -> Game
refresh dt ({vessel, scraps, bullets } as game) =
  { game
  | vessel <- refreshVessel dt vessel
  , bullets <- refreshBullets dt bullets scraps
  , scraps <- refreshScraps dt scraps bullets }

refreshVessel : Time -> Vessel -> Vessel
refreshVessel dt vessel = Vessel.update dt vessel

refreshBullets : Time -> List Bullet -> List (Collidable a) -> List Bullet
refreshBullets dt bullets collidables =
  List.filter (not << Bullet.reachedTarget)
  <| List.map (Bullet.update dt)
  <| List.filter (not << Movement.anyCollision collidables) bullets

refreshScraps : Time -> List Scrap -> List (Collidable a) -> List Scrap
refreshScraps dt scraps collidables =
  let (hScraps, nhScraps) =
        List.partition (Movement.anyCollision collidables) scraps
      hScraps' =
        List.filter (not << Scrap.dead)
        <| List.map (Scrap.damage) hScraps
  in  List.append nhScraps hScraps'

spawn : Float -> Game -> Game
spawn i game =
  let overcrowded = (List.length game.scraps) >= 5
      scrap = Scrap.new <| randomPoint <| round i
  in  if | overcrowded -> game
         | otherwise ->
           { game
           | scraps <- scrap  :: game.scraps
           , quadtree <- QuadTree.insert (toTuple scrap.position) scrap game.quadtree }

--| View |----------------------------------------------------------------------

scene : Dimension -> Game -> Element
scene (w,h) game =
  let background = [drawBackground (w, h)]
      units =
        List.concat
          [ List.map Scrap.draw game.scraps
          , List.map Bullet.draw game.bullets
          , [Vessel.draw game.vessel] ]
      test = QuadTree.draw game.quadtree
      forms = List.concat
        [ background
        , units
        , test ]
      pos = middle
  in  container w h pos <| collage w h forms

drawBackground : Dimension -> Form
drawBackground (w, h) = rect (toFloat w) (toFloat h) |> filled background
