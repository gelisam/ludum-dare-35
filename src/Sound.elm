port module Sound exposing (..)

import Powerup exposing (Powerup(..))


-- MODEL

type alias SoundCommand = String

type Event
  = PlayMusic
  | StopMusic
  | PlayFixedShapeSoundEffect
  | PlayUpgradeSoundEffect

type alias Model =
  { music_is_playing : Bool
  , event : Maybe Event
  }

init : Model
init =
  { music_is_playing = False
  , event = Nothing
  }


-- UPDATE

update : Model -> Model
update model =
  { model | event = Nothing }

playMusic : Model -> Model
playMusic model = case model.music_is_playing of
  False ->
    { model
    | music_is_playing = True
    , event = Just PlayMusic
    }
  True ->
    model

pickup : Powerup -> Model -> Model
pickup powerup model = case powerup of
  FixedShape _ _ _ ->
    { model | event = Just PlayFixedShapeSoundEffect }
  _ ->
    { model | event = Just PlayUpgradeSoundEffect }


-- VIEW

port soundEvent : SoundCommand -> Cmd msg

eventString : Event -> String
eventString event =
  case event of
    PlayMusic ->
      "PlayMusic"
    StopMusic ->
      "StopMusic"
    PlayFixedShapeSoundEffect ->
      "PlayFixedShapeSoundEffect"
    PlayUpgradeSoundEffect ->
      "PlayUpgradeSoundEffect"

updateCmd : Model -> Cmd msg
updateCmd model =
  case model.event of
    Just event ->
      soundEvent (eventString event)
    Nothing ->
      Cmd.none
