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
char_grid : Grid Char
char_grid = Grid.init
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

player_start : Coord
player_start =
  let
    is_start_coord : Coord -> Bool
    is_start_coord coord = Grid.get coord char_grid == Just '.'
    
    coords = Grid.keys char_grid
    start_coords = List.filter is_start_coord coords
  in
    case start_coords of
      [coord] -> coord
      [] -> Debug.crash "level has no start position"
      _ -> Debug.crash "level has more than one start position"

block_grid : Grid Block
block_grid =
  Grid.map (Maybe.withDefault White << Block.parse) char_grid


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
        level_block = Grid.get (player_coord `Vec.plus` level_coord) block_grid
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

element_grid : Grid Element
element_grid =
    Grid.map Block.viewOpaque block_grid

element : Element
element =
  Grid.view element_grid

view : Model -> PositionedElement
view () = (Vec.init, element)
