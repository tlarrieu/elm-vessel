import Graphics.Element exposing (Element)
import Keyboard
import Mouse
import Random exposing (Seed)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Utils exposing (translate, center)
import Game as G

defaultFPS = 60

event : Signal G.Event
event =
  mergeMany
    [ G.Click <~ mouse
    , G.Keys <~ keyboard
    , G.Fire <~ fire
    , G.Refresh <~ refresh
    , G.Spawn <~ Time.every (Time.second / 2) ]

mouse : Signal (Int, Int)
mouse =
  let windowCenter = center <~ Window.dimensions
      mousePosition = translate <~ windowCenter ~ Mouse.position
  in  sampleOn Mouse.clicks mousePosition

keyboard : Signal (Int, Int)
keyboard =
  (\a -> (a.x, a.y))
  <~ Signal.filter
      (\sig -> sig /= { x = 0, y = 0 })
      { x = 0, y = 0 }
      (sampleOn (Time.fps defaultFPS) Keyboard.arrows)

fire : Signal Bool
fire =
  Signal.filter
    (identity)
    False
    (sampleOn (Time.every (Time.second * 0.2)) Keyboard.space)

refresh : Signal Time
refresh = ((\t -> t / 10) <~ (Time.fps defaultFPS))

main : Signal Element
main = G.scene <~ Window.dimensions ~ (foldp G.update G.default event)
