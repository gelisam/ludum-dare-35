module Level where

import Graphics.Element exposing (..)
import String

import Block exposing (Block(..))
import Grid exposing (Grid)
import Vec exposing (Coord)
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

block_level : Grid Block
block_level =
  Grid.map (Maybe.withDefault White << Block.parse) int_level


obstacle : Block -> Bool
obstacle block = block /= White

collides : Coord -> Grid (Maybe Block) -> Bool
collides level_coord player_grid =
  let
    collidesWith : Maybe Block -> Maybe Block -> Bool
    collidesWith player_block level_block = case (player_block, level_block) of
      (Nothing, _) ->
        -- outside the player, no collision
        False
      (_, Nothing) ->
        -- outside the level, grey everywhere
        obstacle Grey
      (_, Just block) ->
        obstacle block
    
    collidesAt : Coord -> Bool
    collidesAt player_coord =
      let
        player_block = Maybe.withDefault Nothing (Grid.get player_coord player_grid)
        level_block = Grid.get (player_coord `Vec.plus` level_coord) block_level
      in
        collidesWith player_block level_block

    player_coords : List Coord
    player_coords = Grid.keys player_grid
  in
    List.any collidesAt player_coords


-- UPDATE

type Action =
  NoOp

update : Action -> Model -> Model
update NoOp model = model


-- VIEW

element_level : Grid Element
element_level =
    Grid.map Block.viewOpaque block_level

level_element : Element
level_element =
  Grid.view element_level

view : Model -> PositionedElement
view () = (Vec.init, level_element)
