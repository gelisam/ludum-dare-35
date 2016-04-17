module Sound where

import Powerup exposing (Powerup(..))


-- MODEL

type alias Model =
  { music_is_playing : Bool
  , last_powerup : Maybe Powerup
  }

init : Model
init =
  { music_is_playing = False
  , last_powerup = Nothing
  }


-- UPDATE

update : Model -> Model
update model =
  { model
  | last_powerup = Nothing
  }

pickup : Powerup -> Model -> Model
pickup powerup model =
  { model
  | last_powerup = Just powerup
  }


-- VIEW

type Event
  = NoOp
  | PlayMusic
  | StopMusic
  | PlayFixedShapeSoundEffect
  | PlayUpgradeSoundEffect

event : Model -> Event
event model = case (model.last_powerup, model.music_is_playing) of
  (Just (FixedShape _ _ _), _) ->
    PlayFixedShapeSoundEffect
  (Just _, _) ->
    PlayUpgradeSoundEffect
  (Nothing, True) ->
    PlayMusic
  (Nothing, False) ->
    StopMusic

signal : Signal Model -> Signal Event
signal = Signal.map event
