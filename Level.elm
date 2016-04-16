module Level where

import Graphics.Element exposing (..)
import String

import BlockColor exposing (Color(..))
import Grid exposing (Grid)
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
int_level : Grid Char
int_level = Grid.init
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

color_level : Grid Color
color_level =
  Grid.map (Maybe.withDefault White << BlockColor.parse) int_level


-- UPDATE

type Action =
  NoOp

update : Action -> Model -> Model
update NoOp model = model


-- VIEW

element_level : Grid Element
element_level =
    Grid.map BlockColor.viewOpaque color_level

level_element : Element
level_element =
  Grid.view element_level

view : Model -> PositionedElement
view () = (Vec.init, level_element)
