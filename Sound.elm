module Sound where

import Powerup exposing (Powerup(..))


-- MODEL

type alias Model =
  { music_is_playing : Bool
  }

init : Model
init =
  { music_is_playing = False
  }


-- UPDATE

pickup : Powerup -> Model -> Model
pickup powerup model =
  { model
  | music_is_playing = True
  }


-- VIEW

type Event
  = NoOp
  | PlayMusic
  | StopMusic

event : Model -> Event
event model =
  if model.music_is_playing then PlayMusic else StopMusic

signal : Signal Model -> Signal Event
signal = Signal.map event
