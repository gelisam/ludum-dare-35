module Shape where

import Graphics.Element as Element exposing (Element)
import Set exposing (Set)

import Block exposing (Block(..))
import Grid exposing (Grid)


type Shape = O | L | J | Z | S | T | I
type alias ShapeId = Int


id : Shape -> ShapeId
id shape = case shape of
  O -> 0
  L -> 1
  J -> 2
  Z -> 3
  S -> 4
  T -> 5
  I -> 6

next : Shape -> Shape
next shape = case shape of
  O -> L
  L -> J
  J -> Z
  Z -> S
  S -> T
  T -> I
  I -> O

nextFrom : Set ShapeId -> Shape -> Shape
nextFrom shapeIds shape =
  let
    shape' = next shape
  in
    if id shape' `Set.member` shapeIds
    then shape'
    else nextFrom shapeIds shape'


char_grid : Shape -> Grid Char
char_grid shape = case shape of
  O -> Grid.init [ "YY"
                 , "YY"
                 ]
  L -> Grid.init [ "  O"
                 , "OOO"
                 ]
  J -> Grid.init [ "B  "
                 , "BBB"
                 ]
  Z -> Grid.init [ "RR "
                 , " RR"
                 ]
  S -> Grid.init [ " GG"
                 , "GG "
                 ]
  T -> Grid.init [ " V "
                 , "VVV"
                 ]
  I -> Grid.init [ "CCCC"
                 ]

block_grid : Shape -> Grid (Maybe Block)
block_grid = Grid.map Block.parse << char_grid

view : Shape -> Element
view = Grid.view << Grid.map Block.viewTransparent << block_grid
