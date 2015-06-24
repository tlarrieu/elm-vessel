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
    , G.Fire <~ Time.every (Time.millisecond * 500)
    , G.Refresh <~ ((\t -> t / 10) <~ Time.fps 60)
    , G.Spawn <~ Time.every (Time.second) ]

input : Signal (Int, Int)
input =
  let windowCenter = center <~ Window.dimensions
      mousePosition = translate <~ windowCenter ~ Mouse.position
  in  Signal.sampleOn Mouse.clicks mousePosition

main : Signal Element
main = G.scene <~ Window.dimensions ~ (foldp G.update G.default event)
