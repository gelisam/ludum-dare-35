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
    row : String -> Array Color
    row = Array.fromList << List.map (Maybe.withDefault White << BlockColor.parse) << String.toList
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
    (Array.map << Array.map) BlockColor.view color_level

level_element : Element
level_element =
  let
    grid = flow down << Array.toList << Array.map (flow right << Array.toList)
  in
    grid element_level

view : Model -> PositionedElement
view () = (Vec.init, level_element)
