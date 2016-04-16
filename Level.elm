module Level where

import Graphics.Element exposing (..)
import String

import Block exposing (Block(..))
import Grid exposing (Grid)
import Powerup exposing (Powerup(..))
import Shape exposing (Shape(..), Orientation(..))
import Vec exposing (Coord)
import View exposing (PositionedElement)


-- MODEL

type alias Model = ()

init : Model
init = ()


-- ' ' for floor
-- '#' for wall
-- 'B', 'C', 'G', 'O', 'R', 'V', and 'Y' for colored blocks
-- '*' for powerup
-- '.' for player position
-- '!' for goal
char_grid : Grid Char
char_grid = Grid.init
  [ "########"
  , "#.    *#"
  , "#     *#"
  , "#     *#"
  , "#******#"
  , "#**BB**#"
  , "#**BOO*#"
  , "#G*B O*#"
  , "#GGYYO*#"
  , "#OGYYV*#"
  , "#ORRVV*#"
  , "#OORRV!#"
  , "########"
  ]

powerups : List Powerup
powerups =
  [ Jump
  , Rotate
  , ShapeShift
  , FixedShape I R0
  , FixedShape O R0
  , FixedShape I R0
  , FixedShape I R1
  , FixedShape S R0
  , FixedShape S R1
  , FixedShape Z R0
  , FixedShape Z R1
  , FixedShape L R0
  , FixedShape L R1
  , FixedShape L R2
  , FixedShape L R3
  , FixedShape J R0
  , FixedShape J R1
  , FixedShape J R2
  , FixedShape J R3
  , FixedShape T R0
  , FixedShape T R1
  , FixedShape T R2
  , FixedShape T R3
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

powerups_start : List (Coord, Powerup)
powerups_start =
  let
    is_powerup_coord : Coord -> Bool
    is_powerup_coord coord = Grid.get coord char_grid == Just '*'
    
    coords = Grid.keys char_grid
    powerup_coords = List.filter is_powerup_coord coords
  in
    List.map2 (,) powerup_coords powerups

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
