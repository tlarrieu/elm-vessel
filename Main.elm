import Graphics.Element exposing (Element)
import Keyboard
import Mouse
import Random exposing (Seed)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Utils exposing (translate, center)
import Game as G

event : Signal G.Event
event =
  mergeMany
    [ G.Move <~ input
    , G.Fire <~ sampleOn (Keyboard.space) (Time.every <| Time.millisecond * 300)
    , G.Refresh <~ ((\t -> t / 10) <~ Time.fps 60)
    , G.Spawn <~ Time.every (Time.second / 2) ]

input : Signal (Int, Int)
input =
  let windowCenter = center <~ Window.dimensions
      mousePosition = translate <~ windowCenter ~ Mouse.position
  in  sampleOn Mouse.clicks mousePosition

main : Signal Element
main = G.scene <~ Window.dimensions ~ (foldp G.update G.default event)
