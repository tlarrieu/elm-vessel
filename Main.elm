import Debug exposing (watch, watchSummary)
-- Libraries
import Color exposing (rgb)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Time exposing (Time)
import Window

-- Local imports
import Scrap exposing (Scrap)
import Vessel exposing (Vessel)
import Events exposing (Event(..), event)
import Utils exposing (center, translate)

--| Model |---------------------------------------------------------------------

type alias Dimension = (Int, Int)

background = (rgb 174 238 238)

type alias Game =
  { vessel: Vessel
  , scraps: List Scrap }

defaultGame =
  { vessel = Vessel.default
  , scraps = [] }

--| Update |--------------------------------------------------------------------

update : Event -> Game -> Game
update event game  =
  case event of
    Move (dt, (x,y)) ->
      let vessel = Vessel.update (dt, (toFloat x, toFloat y)) game.vessel
          distance a b = sqrt ((a.x - b.x)^2 + (a.y - b.y)^2)
          hit scrap = (distance vessel scrap) < vessel.radius + scrap.radius
          scraps = List.filter (not << hit) game.scraps
      in  { game
          | vessel <- vessel
          , scraps <- scraps }
    Spawn scrap ->
      let overcrowded = (List.length game.scraps) >= 10
      in  if | overcrowded -> game
             | otherwise -> { game | scraps <- scrap :: game.scraps }

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

--| Signals |-------------------------------------------------------------------

main : Signal Element
main =
  scene
    <~ Window.dimensions
    ~ (foldp update defaultGame event)
