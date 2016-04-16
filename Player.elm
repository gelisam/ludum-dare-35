module Player where

import Graphics.Element as Element exposing (Element)
import Set exposing (Set)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Keys
import Powerup exposing (Powerup(..), PowerupId)
import Shape exposing (Shape(..), Orientation(..))
import Vec exposing (Coord, Vec)
import View exposing (PositionedElement)


-- MODEL

type alias Model =
  { last_keys : Keys.Action
  , coord : Coord
  , shape : Shape
  , orientation : Orientation
  , powerupIds : Set PowerupId
  }


init : Coord -> Model
init start_coord =
  { last_keys = Keys.NoOp
  , coord = start_coord
  , shape = O
  , orientation = R0
  , powerupIds = Set.empty
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
  Keys.LeftKey ->
    { model | coord = model.coord `Vec.plus` { x = -1, y = 0 } }
  Keys.RightKey ->
    { model | coord = model.coord `Vec.plus` { x = 1, y = 0 } }
  Keys.UpKey ->
    if Powerup.id Jump `Set.member` model.powerupIds
    then
      { model | coord = model.coord `Vec.plus` { x = 0, y = -1 } }
    else
      model
  Keys.DownKey ->
    { model | coord = model.coord `Vec.plus` { x = 0, y = 1 } }
  Keys.RotationKey ->
    if Powerup.id Rotate `Set.member` model.powerupIds
    then
      { model | orientation = Shape.nextOrientation model.orientation }
    else
      model
  Keys.ShapeShiftKey ->
    if Powerup.id ShapeShift `Set.member` model.powerupIds
    then
      { model | shape = Shape.nextShape model.shape }
    else
      model


-- VIEW

block_grid : Model -> Grid (Maybe Block)
block_grid model = Shape.block_grid model.shape model.orientation

view : Model -> PositionedElement
view model =
  (model.coord, Shape.view model.shape model.orientation)
