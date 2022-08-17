module Camera where

import Time exposing (Time)

import FocusPoint exposing (FocusPoint)
import Vec exposing (..)


-- MODEL

type alias Model =
  { target : PixelsF
  , position : PixelsF
  , velocity : PixelsF
  }

init : Coord -> Model
init coord =
  let
    position = pixelsF coord |> minus { x = 320-14, y = 240-14 }
  in
    { target = position
    , position = position
    , velocity = Vec.init
    }


-- UPDATE

type alias Action =
  { coord : Coord
  , dt : Time
  , focus_points : List FocusPoint
  }

update : Action -> Model -> Model
update action =
  update_target action >>
  update_velocity action >>
  update_position action

update_target : Action -> Model -> Model
update_target action model =
  let
    valid_targets = List.filter (FocusPoint.isClose action.coord) action.focus_points
  in
    case List.head valid_targets of
      Just focus_point ->
        { model | target = pixelsF focus_point |> minus { x = 320-14, y = 240-14 } }
      Nothing ->
        { model | target = pixelsF action.coord |> minus { x = 320-2*28, y = 240-2*28 } }

update_velocity : Action -> Model -> Model
update_velocity action model =
  { model
  | velocity = scale 0.01 (model.target |> minus model.position)
  }

update_position : Action -> Model -> Model
update_position action model =
  { model
  | position = model.position |> plus scale action.dt model.velocity
  }


view : Model -> Pixels
view model = map round model.position
