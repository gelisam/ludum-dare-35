module Player where

import Set exposing (Set)
import Time exposing (Time)

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
  , inactive_dt : Time
  , down_delay : Int
  , coord : Coord
  , shape : Shape
  , orientation : Orientation
  , powerupIds : Set PowerupId
  , gracePeriod : Time
  }


init : Coord -> Model
init start_coord =
  { last_keys = Keys.NoOp
  , inactive_dt = 0
  , down_delay = 1000
  , coord = start_coord
  , shape = I
  , orientation = R0
  , powerupIds = Set.empty
  , gracePeriod = 0
  -- , powerupIds = Set.fromList <| List.map Powerup.id [Jump, Rotate, ShapeShift]
  }

hasPowerup : Powerup -> Model -> Bool
hasPowerup powerup model = 
  Powerup.id powerup `Set.member` model.powerupIds


-- UPDATE

type alias Action =
  { dt : Time
  , keys : Keys.Action
  }

update : Action -> Model -> Model
update action =
  time_passes action.dt >>
  keys_are_pressed action.keys

time_passes : Time -> Model -> Model
time_passes dt model =
  { model
  | inactive_dt = model.inactive_dt + dt
  , gracePeriod = model.gracePeriod - dt
  }

normal_auto_repeat_delay : Time
normal_auto_repeat_delay = 80 * Time.millisecond

up_auto_repeat_delay : Time
up_auto_repeat_delay = 120 * Time.millisecond

down_auto_repeat_delay : Time
down_auto_repeat_delay = 50 * Time.millisecond

keys_are_pressed : Keys.Action -> Model -> Model
keys_are_pressed keys model = case keys of
  Keys.NoOp ->
    if model.inactive_dt > toFloat model.down_delay * Time.millisecond
        && model.gracePeriod <= 0
    then
      let
        model' = keys_are_pressed Keys.DownKey model
      in
        { model'
        | down_delay = max 200 (model'.down_delay - 10)
        }
    else
      { model
      | last_keys = Keys.NoOp
      }
  Keys.LeftKey ->
    if model.last_keys /= Keys.LeftKey || model.inactive_dt > normal_auto_repeat_delay
    then
      { model
      | coord = model.coord `Vec.plus` { x = -1, y = 0 }
      , last_keys = Keys.LeftKey
      , inactive_dt = 0
      }
    else
      model
  Keys.RightKey ->
    if model.last_keys /= Keys.RightKey || model.inactive_dt > normal_auto_repeat_delay
    then
      { model
      | coord = model.coord `Vec.plus` { x = 1, y = 0 }
      , last_keys = Keys.RightKey
      , inactive_dt = 0
      }
    else
      model
  Keys.UpKey ->
    if
      hasPowerup Jump model &&
      (model.last_keys /= Keys.UpKey || model.inactive_dt > up_auto_repeat_delay)
    then
      { model
      | coord = model.coord `Vec.plus` { x = 0, y = -1 }
      , last_keys = Keys.UpKey
      , inactive_dt = 0
      , gracePeriod = 500 * Time.millisecond
      }
    else
      model
  Keys.DownKey ->
    if model.last_keys /= Keys.DownKey || model.inactive_dt > down_auto_repeat_delay
    then
      { model
      | coord = model.coord `Vec.plus` { x = 0, y = 1 }
      , last_keys = Keys.DownKey
      , inactive_dt = 0
      }
    else
      model
  Keys.RotationKey ->
    if
      hasPowerup Rotate model &&
      (model.last_keys /= Keys.RotationKey)
    then
      { model
      | orientation = Shape.nextOrientation model.orientation
      , last_keys = Keys.RotationKey
      }
    else
      model
  Keys.ShapeShiftKey ->
    if
      hasPowerup ShapeShift model &&
      (model.last_keys /= Keys.ShapeShiftKey)
    then
      { model
      | shape = Shape.nextShape model.shape
      , last_keys = Keys.ShapeShiftKey
      }
    else
      model

pickup : Powerup -> Model -> Model
pickup powerup model = case powerup of
  FixedShape shape orientation coord ->
    if shape == model.shape && orientation == model.orientation
    then
      model
    else
      { model
      | coord = coord `Vec.minus` { x = 2, y = 1 }
      , shape = shape
      , orientation = orientation
      }
  _ ->
    { model
    | powerupIds = Set.insert (Powerup.id powerup) model.powerupIds
    }


-- VIEW

block_grid : Model -> Grid (Maybe Block)
block_grid model = Shape.block_grid model.shape model.orientation

view : Model -> PositionedElement
view model =
  { coord = model.coord
  , element = Shape.view model.shape model.orientation
  , visible = True
  }
