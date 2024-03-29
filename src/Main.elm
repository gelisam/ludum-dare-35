module Main exposing (main)

import Array
import Browser
import Dict
import Either exposing (Either(..))
import Html exposing (Html)

import Camera
import Ending
import FocusPoint exposing (FocusPoint)
import Instructions
import Keys
import Level
import Player exposing (Milliseconds, Msg(..))
import Powerup exposing (Powerup(..))
import Powerups exposing (Powerups)
import Sound
import Vec exposing (Coord, Vec)
import View exposing (PositionedImage)

type alias Flags = {}

-- MODEL

type alias Model =
  { started : Bool
  , camera : Camera.Model
  , sound : Sound.Model
  , player : Player.Model
  , blinking_duration : Milliseconds
  , blinking_player : Maybe Player.Model
  , powerups : Powerups
  , instructions : Instructions.Model
  , ending : Ending.Model
  }


-- It's going to be a Right, but Elm won't let me assert that.
goal_focus_point : Either String FocusPoint
goal_focus_point =
  Either.map (Vec.minus { x = 2, y = 6 }) Level.goal_coord

failedInit : String -> Model
failedInit errorMessage =
  -- every value except debug is irrelevant
  { started = False
  , camera = Camera.init Vec.init
  , sound = Sound.init
  , player = Player.init Vec.init
  , blinking_duration = 0
  , blinking_player = Nothing
  , powerups = Dict.empty
  , instructions = Instructions.init
  , ending = Ending.init Vec.init
  }

initWithEither : Either String a -> (a -> Model) -> Model
initWithEither either f =
  case either of
    Left errorMessage ->
      failedInit errorMessage
    Right a ->
      f a

init : Model
init =
  initWithEither Level.player_start <| \player_start ->
  initWithEither Level.goal_coord <| \goal_coord ->
  initWithEither goal_focus_point <| \camera_start ->
  { started = False
  , camera = Camera.init camera_start
  , sound = Sound.init
  , player = Player.init player_start
  , blinking_duration = 0
  , blinking_player = Nothing
  , powerups = Level.powerups_start
  , instructions = Instructions.init
  , ending = Ending.init goal_coord
  }

initCmd : Cmd msg
initCmd =
  Cmd.none

fancyInit : flags -> ( Model, Cmd msg )
fancyInit _ =
  (init, initCmd)


-- UPDATE

type Msg
  = Start
  | PlayerAction Player.Msg

update : Player.Msg -> Model -> Model
update msg model =
  model
    |> prepare_sound
    |> game_update msg
    |> camera_update msg
    |> music_update

updateCmd : Player.Msg -> Model -> Cmd msg
updateCmd msg model =
  Sound.updateCmd model.sound

fancyUpdate : Msg -> Model -> ( Model, Cmd msg )
fancyUpdate msg model =
  case (msg, model.started) of
    (Start, _) ->
      ( { model
        | started = True
        }
      , Cmd.none
      )
    (_, False) ->
      (model, Cmd.none)
    (PlayerAction playerMsg, True) ->
      let
          model_ = update playerMsg model
      in
      (model_, updateCmd playerMsg model_)

prepare_sound : Model -> Model
prepare_sound model =
  { model | sound = Sound.update model.sound }

game_update : Player.Msg -> Model -> Model
game_update msg model = case model.blinking_player of
  Just _ ->
    case msg of
      TimePasses dt ->
        if model.blinking_duration > 100
        then
          { model | blinking_player = Nothing }
        else
          { model | blinking_duration = model.blinking_duration + dt }
      _ ->
        model
  Nothing ->
    if model.ending.has_ended
    then
      case msg of
        TimePasses dt ->
          { model
          | ending = Ending.update (dt, Ending.NoOp) model.ending
          }
        _ ->
          model
    else
      let
          player_ = Player.update msg model.player
          hasCollided = Level.collides player_.coord (Player.block_grid player_)
          wasCausedByUnintuitiveMovement =
            case msg of
              KeyPressed Keys.RotationKey ->
                True
              KeyPressed Keys.ShapeShiftKey ->
                True
              _ ->
                False
          preCollisionPlayer =
            { player_
            | coord = model.player.coord
            , shape = model.player.shape
            , orientation = model.player.orientation
            , powerupIds = model.player.powerupIds
            }
          preCollisionModel =
            { model
            | player = preCollisionPlayer
            }
      in
      case (hasCollided, wasCausedByUnintuitiveMovement) of
        (True, True) ->
          { model
          | blinking_duration = 0
          , blinking_player = Just player_
          }
        (True, False) ->
          preCollisionModel
        (False, _) ->
          { model | player = player_ }
            |> check_instructions msg
            |> check_powerups
            |> check_ending

check_instructions : Player.Msg -> Model -> Model
check_instructions msg model = case (msg, model.instructions) of
  (KeyPressed Keys.UpKey, Instructions.Intro) ->
    { model
    | instructions = Instructions.CannotRotate
    }
  (KeyPressed Keys.UpKey, Instructions.HowToUsePowerup Jump) ->
    { model
    | instructions = Instructions.UnlikelyJump
    }
  _ ->
    model

check_powerups : Model -> Model
check_powerups model =
  let
      (pickedPowerups, remainingPowerups) =
        Powerups.pickup model.player.coord (Player.block_grid model.player) model.powerups
      model_ = List.foldr maybe_pickup_powerup model pickedPowerups
  in
  { model_
  | powerups = remainingPowerups
  }

maybe_pickup_powerup : Powerup -> Model -> Model
maybe_pickup_powerup powerup model = case powerup of
  FixedShape shape orientation _ ->
    if shape == model.player.shape && orientation == model.player.orientation
    then
      model
    else
      pickup_powerup powerup model
  _ ->
    pickup_powerup powerup model

pickup_powerup : Powerup -> Model -> Model
pickup_powerup powerup model =
  { model
  | player = Player.pickup powerup model.player
  , instructions = Instructions.HowToUsePowerup powerup
  , sound = Sound.pickup powerup model.sound
  }

check_ending : Model -> Model
check_ending model =
  if Ending.pickup model.player.coord (Player.block_grid model.player) model.ending
  then
    { model
    | ending = Ending.update (0, Ending.TheEnd) model.ending
    , instructions = Instructions.YouWin
    }
  else
    model

camera_update : Player.Msg -> Model -> Model
camera_update msg model =
  case msg of
    TimePasses dt ->
      let
          powerups =
            Powerups.coords model.powerups
          is_important_powerup coord = case Powerups.get coord model.powerups of
            Nothing -> False
            Just (FixedShape _ _ _) -> False
            Just _ -> True
          camera_msg =
            { coord = model.player.coord
            , dt = dt
            , focus_points = List.concat
                [ Either.toList goal_focus_point
                , List.filter is_important_powerup powerups
                ]
            }
      in
      { model | camera = Camera.update camera_msg model.camera }
    _ ->
      model

music_update : Model -> Model
music_update model =
  case goal_focus_point of
    Right start_coord ->
      if FocusPoint.isClose model.player.coord start_coord
      then
        model
      else
        -- the player ventures into the unknown, cue the music!
        { model
        | sound = Sound.playMusic model.sound
        }
    Left _ ->
      model


-- VIEW

view : Model -> Html Msg
view model =
  View.view Start
    { started =
        model.started
    , camera =
        Camera.view model.camera
    , images = List.concat
        [ Level.view
        , Powerups.view model.powerups
        , viewPlayer model
        , Ending.view model.ending
        ]
    , instructions = Instructions.view model.instructions
    , debug = ""
    }

viewPlayer : Model -> List PositionedImage
viewPlayer model = case model.blinking_player of
  Just player ->
    Player.view player
  Nothing ->
    Player.view model.player



subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Keys.sub
        (PlayerAction << Player.KeyPressed)
        (PlayerAction << Player.KeyReleased)
    , Player.sub
        (PlayerAction << Player.TimePasses)
    ]

main : Program Flags Model Msg
main =
  Browser.element
    { init = fancyInit
    , view = view
    , update = fancyUpdate
    , subscriptions = subscriptions
    }
