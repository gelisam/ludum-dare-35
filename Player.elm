module Player where

import Graphics.Element as Element exposing (Element)
import Set exposing (Set)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Keys
import Shape exposing (Shape(..), ShapeId)
import Vec exposing (Coord, Vec)
import View exposing (PositionedElement)


-- MODEL

type alias Model =
  { last_keys : Keys.Action
  , p : Vec Int
  , shapeIds : Set ShapeId
  , shape : Shape
  }


init : Coord -> Model
init start_coord =
  { last_keys = Keys.NoOp
  , p = start_coord
  , shapeIds = Set.fromList [0..6]
  , shape = O
  }


-- UPDATE

type alias Action = Keys.Action

update : Action -> Model -> Model
update action model =
  if model.last_keys == action
  then
    model
  else
    let
      model' = instant_update action model
    in
      { model' | last_keys = action }


instant_update : Keys.Action -> Model -> Model
instant_update action model = case action of
  Keys.NoOp ->
    model
  Keys.ArrowKey keys ->
    { model
    | p = model.p `Vec.plus` keys
    }
  Keys.ShapeShiftKey ->
    { model
    | shape = Shape.nextFrom model.shapeIds model.shape
    }


-- VIEW

block_grid : Model -> Grid (Maybe Block)
block_grid = Shape.block_grid << .shape

view : Model -> PositionedElement
view model =
  (model.p, Shape.view model.shape)
