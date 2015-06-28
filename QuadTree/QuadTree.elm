module QuadTree.QuadTree
  ( QuadTree(..)
  , draw
  , empty
  , insert ) where

import Debug exposing (log,watch,watchSummary,trace)
import Array exposing (Array)
import Color exposing (rgb)
import Graphics.Collage exposing (..)

import QuadTree.Box as Box exposing (Box)

type QuadTree a =
  Leaf Box Int (Array.Array a)
  | Node Box Int (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)

empty : Box -> Int -> QuadTree a
empty box size = Leaf box size Array.empty


maxSize : QuadTree a -> Int
maxSize tree =
  case tree of
    Leaf _ size _ -> size
    Node _ size _ _ _ _ -> size


insert : (Float, Float) -> a -> QuadTree a -> QuadTree a
insert point item tree =
  case tree of
    Leaf box maxSize items ->
      if Box.inside box point then
        let items' = Array.push item items
        in if Array.length items' <= maxSize
          then watch "leaf" <| Leaf box maxSize items'
          else
            let insertNew quadrant =
                  Array.foldr (\item tree -> insert point item tree)
                        (empty quadrant maxSize)
                        items'
                (boxNE, boxNW, boxSW, boxSE) =
                  watch "subdivide" <| Box.subdivide box
                (ne, nw, sw, se) =
                  ( insertNew boxNE
                  , insertNew boxNW
                  , insertNew boxSW
                  , insertNew boxSE )
            in  Node box maxSize ne nw sw se
      else tree
    Node box maxSize ne nw sw se ->
      if Box.inside box point then
        let insertNew quadrant = insert point item quadrant
            (ne', nw', sw', se') =
              ( insertNew ne
              , insertNew nw
              , insertNew sw
              , insertNew se )
        in Node box maxSize ne' nw' sw' se'
      else tree

draw : QuadTree a -> List Form
draw qt =
  case qt of
    Leaf box _ _ -> [drawBox box]
    Node box _ ne nw sw se ->
      List.concat
        [ [drawBox box]
        , draw ne
        , draw nw
        , draw sw
        , draw se ]

drawBox : Box -> Form
drawBox ({center, height, width} as box) =
  let shape = rect height width
      stroke =
        outlined
          { defaultLine | width <- 1, color <- rgb 255 0 0 }
          shape
      form =
        collage (round height) (round width) [stroke]
        |> toForm
  in  form
      |> alpha 0.8
      |> move box.center
