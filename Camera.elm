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

type alias Msg =
  { coord : Coord
  , dt : Time
  , focus_points : List FocusPoint
  }

update : Msg -> Model -> Model
update msg =
  update_target msg >>
  update_velocity msg >>
  update_position msg

update_target : Msg -> Model -> Model
update_target msg model =
  let
      valid_targets = List.filter (FocusPoint.isClose msg.coord) msg.focus_points
  in
  case List.head valid_targets of
    Just focus_point ->
      { model | target = pixelsF focus_point |> minus { x = 320-14, y = 240-14 } }
    Nothing ->
      { model | target = pixelsF msg.coord |> minus { x = 320-2*28, y = 240-2*28 } }

update_velocity : Msg -> Model -> Model
update_velocity msg model =
  { model
  | velocity = scale 0.01 (model.target |> minus model.position)
  }

update_position : Msg -> Model -> Model
update_position msg model =
  { model
  | position = model.position |> plus scale msg.dt model.velocity
  }


view : Model -> Pixels
view model = map round model.position
