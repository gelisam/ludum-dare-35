module Player exposing (Milliseconds, Model, Msg(..), init, update, pickup, block_grid, view, sub)

import Browser.Events
import Set exposing (Set)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Keys
import Powerup exposing (Powerup(..), PowerupId)
import Shape exposing (Shape(..), Orientation(..))
import Vec exposing (Coord, Vec)
import View exposing (PositionedImage)


-- MODEL

type alias Milliseconds = Float

type alias Model =
  { last_keys : Keys.Msg
  , inactive_dt : Milliseconds
  , down_delay : Int
  , coord : Coord
  , shape : Shape
  , orientation : Orientation
  , powerupIds : Set PowerupId
  , gracePeriod : Milliseconds
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
  --, powerupIds = Set.fromList <| List.map Powerup.id [Jump, Rotate, ShapeShift]
  , gracePeriod = 0
  }

hasPowerup : Powerup -> Model -> Bool
hasPowerup powerup model =
  Set.member (Powerup.id powerup) model.powerupIds


-- UPDATE

type Msg
  = TimePasses Milliseconds
  | KeyPressed Keys.Msg

update : Msg -> Model -> Model
update msg =
  case msg of
    TimePasses dt ->
      time_passes dt >> apply_gravity
    KeyPressed key ->
      keys_are_pressed key

time_passes : Milliseconds -> Model -> Model
time_passes dt model =
  { model
  | inactive_dt = model.inactive_dt + dt
  , gracePeriod = model.gracePeriod - dt
  }

apply_gravity : Model -> Model
apply_gravity model =
  if model.inactive_dt > toFloat model.down_delay
      && model.gracePeriod <= 0
  then
    let
        model_ = keys_are_pressed Keys.DownKey model
    in
    { model_
    | down_delay = max 200 (model_.down_delay - 10)
    }
  else
    model

normal_auto_repeat_delay : Milliseconds
normal_auto_repeat_delay = 80

up_auto_repeat_delay : Milliseconds
up_auto_repeat_delay = 120

down_auto_repeat_delay : Milliseconds
down_auto_repeat_delay = 50

keys_are_pressed : Keys.Msg -> Model -> Model
keys_are_pressed keys model = case keys of
  Keys.NoOp ->
      { model
      | last_keys = Keys.NoOp
      }
  Keys.LeftKey ->
    if model.last_keys /= Keys.LeftKey || model.inactive_dt > normal_auto_repeat_delay
    then
      { model
      | coord = model.coord |> Vec.plus { x = -1, y = 0 }
      , last_keys = Keys.LeftKey
      , inactive_dt = 0
      }
    else
      model
  Keys.RightKey ->
    if model.last_keys /= Keys.RightKey || model.inactive_dt > normal_auto_repeat_delay
    then
      { model
      | coord = model.coord |> Vec.plus { x = 1, y = 0 }
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
      | coord = model.coord |> Vec.plus { x = 0, y = -1 }
      , last_keys = Keys.UpKey
      , inactive_dt = 0
      , gracePeriod = 500
      }
    else
      model
  Keys.DownKey ->
    if model.last_keys /= Keys.DownKey || model.inactive_dt > down_auto_repeat_delay
    then
      { model
      | coord = model.coord |> Vec.plus { x = 0, y = 1 }
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
      | coord = coord |> Vec.minus { x = 2, y = 1 }
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

view : Model -> List PositionedImage
view model = Shape.view model.coord model.shape model.orientation

sub : Sub Milliseconds
sub = Browser.Events.onAnimationFrameDelta identity
