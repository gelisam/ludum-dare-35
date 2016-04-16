module Level where

import Array exposing (Array)
import Graphics.Element exposing (..)
import String

import BlockColor exposing (Color(..))
import Vec
import View exposing (PositionedElement)


-- MODEL

type alias Model = ()

init : Model
init = ()


-- ' ' for floor
-- '#' for wall
-- 'b' for upgrade, 'B' for block
-- 'c' for upgrade, 'C' for block
-- 'g' for upgrade, 'G' for block
-- 'o' for upgrade, 'O' for block
-- 'r' for upgrade, 'R' for block
-- 'v' for upgrade, 'V' for block
-- 'y' for upgrade, 'Y' for block
-- '.' for player position, '!' for goal
int_level : List String
int_level =
  [ "########"
  , "#      #"
  , "#  .   #"
  , "#      #"
  , "#      #"
  , "#  BB  #"
  , "#  BOO #"
  , "#G B O #"
  , "#GGYYO #"
  , "#OGYYV #"
  , "#ORRVV #"
  , "#OORRV!#"
  , "########"
  ]

color_level : Array (Array Color)
color_level =
  let
    color : Char -> Color
    color char = case char of
      ' ' -> White
      'B' -> Blue
      'C' -> Cyan
      'G' -> Green
      'O' -> Orange
      'R' -> Red
      'V' -> Violet
      'Y' -> Yellow
      _   -> Grey
    
    row : String -> Array Color
    row = Array.fromList << List.map color << String.toList
  in
    Array.fromList (List.map row int_level)


-- UPDATE

type Action =
  NoOp

update : Action -> Model -> Model
update NoOp model = model


-- VIEW

element_level : Array (Array Element)
element_level =
  let
    block : Color -> Element
    block color =
      image 28 28 ("/imgs/" ++ String.toLower (toString color) ++ ".png")
  in
    Array.map (Array.map block) color_level

level_element : Element
level_element =
  let
    grid = flow down << Array.toList << Array.map (flow right << Array.toList)
  in
    grid element_level

view : Model -> PositionedElement
view () = (Vec.init, level_element)
