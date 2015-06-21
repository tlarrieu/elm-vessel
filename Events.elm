module Events where

import Mouse
import Random exposing (Seed)
import Signal exposing (..)
import Time exposing (Time)
import Window

import Scrap exposing (Scrap)
import Utils exposing (translate, center)

--| Model |---------------------------------------------------------------------

type Event = Move (Time, (Int, Int)) | Spawn Scrap

--| Signals |-------------------------------------------------------------------

event : Signal Event
event =
  Signal.mergeMany
    [ Move <~ input
    , Spawn <~ spawn ]

input : Signal (Time, (Int, Int))
input =
  let delta = (\ t -> t / 10) <~ (Time.fps 120)
  in  (,) <~ delta ~ (Signal.sampleOn Mouse.isDown relativeMousePosition)

relativeMousePosition : Signal (Int, Int)
relativeMousePosition =
  let windowCenter = center <~ Window.dimensions
  in  translate <~ windowCenter ~ Mouse.position

spawn : Signal Scrap
spawn =
  (\i -> Scrap.new <| randomPoint <| round i) <~  Time.every (Time.second * 2)

randomPoint : Int -> (Float, Float)
randomPoint a =
  let seed = Random.initialSeed a
      randX = Random.float 0 1000
      randY = Random.float 0 800
      (pair, _) = Random.generate (Random.pair randX randY) seed
  in  pair
