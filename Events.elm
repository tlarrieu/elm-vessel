module Events where

import Signal exposing (..)
import Time exposing (Time)
import Random exposing (Seed)
import Touch
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
  in Signal.sampleOn delta <| (,) <~ delta ~ relativeTap

relativeTap : Signal (Int, Int)
relativeTap =
  let windowCenter = center <~ Window.dimensions
      taps = (\ {x,y} -> (x,y)) <~ Touch.taps
  in  translate <~ windowCenter ~ taps

spawn : Signal Scrap
spawn =
  (\i -> Scrap.new <| randomPoint <| round i) <~  Time.every (Time.second * 2)

randomPoint : Int -> (Int, Int)
randomPoint a =
  let seed i = Random.initialSeed i
      randX = Random.int 0 1000
      randY = Random.int 0 800
      (pair, _) = Random.generate (Random.pair randX randY) (seed a)
  in  pair

