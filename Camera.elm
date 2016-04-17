module Camera where

import Time exposing (Time)

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
    position = pixelsF coord `minus` { x = 320-2*28, y = 240-2*28 }
  in
    { target = position
    , position = position
    , velocity = Vec.init
    }


-- UPDATE

type alias Action =
  { coord : Coord
  , dt : Time
  }

update : Action -> Model -> Model
update action =
  update_target action >>
  update_velocity action >>
  update_position action

update_target : Action -> Model -> Model
update_target action model =
  { model
  | target = pixelsF action.coord `minus` { x = 320-2*28, y = 240-2*28 }
  }

update_velocity : Action -> Model -> Model
update_velocity action model =
  { model
  | velocity = scale 0.01 (model.target `minus` model.position)
  }

update_position : Action -> Model -> Model
update_position action model =
  { model
  | position = model.position `plus` scale action.dt model.velocity
  }


view : Model -> Pixels
view model = map round model.position
