import Graphics.Element exposing (Element)
import Mouse
import Random exposing (Seed)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Utils exposing (translate, center)
import Game as G

event : Signal G.Event
event =
  Signal.mergeMany
    [ G.Move <~ input
    , G.Spawn <~ Time.every (Time.second) ]

input : Signal (Time, (Int, Int))
input =
  let delta = (\ t -> t / 10) <~ (Time.fps 60)
      windowCenter = center <~ Window.dimensions
      mousePosition = translate <~ windowCenter ~ Mouse.position
  in  (,) <~ delta ~ (Signal.sampleOn Mouse.clicks mousePosition)

main : Signal Element
main = G.scene <~ Window.dimensions ~ (foldp G.update G.default event)
