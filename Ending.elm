module Ending where

import Graphics.Element exposing (..)
import Time exposing (Time)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Vec exposing (Coord, Vec)
import View exposing (PositionedElement)


-- MODEL

type alias Model =
  { elapsed : Time
  , coord : Coord
  , has_ended : Bool
  }

init : Coord -> Model
init goal_coord =
  { elapsed = 0
  , coord = goal_coord
  , has_ended = False
  }


-- UPDATE

type Action
  = NoOp
  | TheEnd

update : (Time, Action) -> Model -> Model
update (dt, action) model = case (action, model.has_ended) of
  (TheEnd, False) ->
    { model
    | elapsed = 0
    , has_ended = True
    }
  _ ->
    { model
    | elapsed = model.elapsed + dt
    }

pickup : Coord -> Grid (Maybe Block) -> Model -> Bool
pickup level_coord player_grid model =
  let
    player_coords : List Coord
    player_coords = Grid.keys player_grid
    
    pickup1 : Coord -> Bool
    pickup1 player_coord =
      let
        coord = level_coord `Vec.plus` player_coord
      in
        coord == model.coord
  in
    List.any pickup1 player_coords


-- VIEW

char_grid0 : Grid Char
char_grid0 =
  Grid.init
    [ "  BB  "
    , "  BOO "
    , "G B O "
    , "GGYYOC"
    , "OGYYVC"
    , "ORRVVC"
    , "OORRVC"
    ]

char_grid1 : Grid Char
char_grid1 =
  Grid.init
    [ "  BB  "
    , "  BOO "
    , "G B O "
    , "      "
    , "      "
    , "      "
    , "      "
    ]

char_grid2 : Grid Char
char_grid2 =
  Grid.init
    [ "      "
    , "      "
    , "      "
    , "      "
    , "  BB  "
    , "  BOO "
    , "G B O "
    ]

block_grid0 : Grid Block
block_grid0 =
  Grid.map (Maybe.withDefault White << Block.parse) char_grid0

block_grid1 : Grid Block
block_grid1 =
  Grid.map (Maybe.withDefault White << Block.parse) char_grid1

block_grid2 : Grid Block
block_grid2 =
  Grid.map (Maybe.withDefault White << Block.parse) char_grid2

element_grid0 : Grid Element
element_grid0 =
    Grid.map Block.viewOpaque block_grid0

element_grid1 : Grid Element
element_grid1 =
    Grid.map Block.viewOpaque block_grid1

element_grid2 : Grid Element
element_grid2 =
    Grid.map Block.viewOpaque block_grid2


view : Model -> PositionedElement
view model =
  { coord =
      model.coord  `Vec.minus` { x = Grid.width char_grid0 - 1
                               , y = Grid.height char_grid0 - 1
                               }
  , element =
      if model.elapsed < 1800 * Time.millisecond
      then
        if round (model.elapsed / 100 * Time.millisecond) % 2 == 0
        then
          Grid.view element_grid1
        else
          Grid.view element_grid0
      else
        Grid.view element_grid2
  , visible = model.has_ended
  }
