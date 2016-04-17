module Main where

import AnimationFrame
import Html exposing (Html)
import Time exposing (Time)

import Camera
import Ending
import FocusPoint exposing (FocusPoint)
import Instructions
import Keys
import Level
import Player
import Powerup exposing (Powerup(..))
import Powerups exposing (Powerups)
import Sound
import Vec exposing (Coord, Vec)
import View exposing (PositionedElement)


-- MODEL

type alias Model =
  { camera : Camera.Model
  , sound : Sound.Model
  , player : Player.Model
  , blinking_duration : Time
  , blinking_player : Maybe Player.Model
  , powerups : Powerups
  , instructions : Instructions.Model
  , ending : Ending.Model
  }


goal_focus_point : FocusPoint
goal_focus_point =
  Level.goal_coord `Vec.minus` { x = 2, y = 6}

init : Model
init =
  { camera = Camera.init goal_focus_point
  , sound = Sound.init
  , player = Player.init Level.player_start
  , blinking_duration = 0
  , blinking_player = Nothing
  , powerups = Level.powerups_start
  , instructions = Instructions.init
  , ending = Ending.init Level.goal_coord
  }


-- UPDATE

update : Player.Action -> Model -> Model
update action =
  prepare_sound >>
  game_update action >>
  camera_update action >>
  music_update

prepare_sound : Model -> Model
prepare_sound model =
  { model | sound = Sound.update model.sound }

game_update : Player.Action -> Model -> Model
game_update action model = case model.blinking_player of
  Just _ ->
    if model.blinking_duration > 100 * Time.millisecond
    then
      { model | blinking_player = Nothing }
    else
      { model | blinking_duration = model.blinking_duration + action.dt }
  Nothing ->
    if model.ending.has_ended
    then
      { model
      | ending = Ending.update (action.dt, Ending.NoOp) model.ending
      }
    else
      let
        player' = Player.update action model.player
        collision = Level.collides player'.coord (Player.block_grid player')
      in
        if collision
        then
          if action.keys == Keys.RotationKey || action.keys == Keys.ShapeShiftKey
          then
            { model
            | blinking_duration = 0
            , blinking_player = Just player'
            }
          else
            model
        else
          { model | player = player' }
            |> check_instructions action
            |> check_powerups
            |> check_ending

check_instructions : Player.Action -> Model -> Model
check_instructions action model = case (action.keys, model.instructions) of
  (Keys.UpKey, Instructions.Intro) ->
    { model
    | instructions = Instructions.CannotRotate
    }
  (Keys.UpKey, Instructions.HowToUsePowerup Jump) ->
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
    model' = List.foldr maybe_pickup_powerup model pickedPowerups
  in
    { model'
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

camera_update : Player.Action -> Model -> Model
camera_update action model =
  let powerups =
        Powerups.coords model.powerups
      is_important_powerup coord = case Powerups.get coord model.powerups of
        Nothing -> False
        Just (FixedShape _ _ _) -> False
        Just _ -> True
      camera_action =
        { coord = model.player.coord
        , dt = action.dt
        , focus_points = List.concat
            [ [goal_focus_point]
            , List.filter is_important_powerup powerups
            ]
        }
  in
    { model | camera = Camera.update camera_action model.camera }

music_update : Model -> Model
music_update model =
  if FocusPoint.isClose model.player.coord goal_focus_point
  then
    model
  else
    -- the player ventures into the unknown, cue the music!
    { model
    | sound = Sound.playMusic model.sound
    }


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      Camera.view model.camera
  , elements = List.concat
      [ [Level.view]
      , Powerups.view model.powerups
      , [viewPlayer model]
      , [Ending.view model.ending]
      ]
  , instructions = Instructions.view model.instructions
  , debug = toString
      (round (model.ending.elapsed / 100 * Time.millisecond))
  }

viewPlayer : Model -> PositionedElement
viewPlayer model = case model.blinking_player of
  Just player ->
    Player.view player
  Nothing ->
    Player.view model.player


-- SIGNALS

state : Signal Model
state =
  Signal.foldp update init input

main : Signal Html
main =
  Signal.map view state


input : Signal Player.Action
input =
  let
    delta = AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (\dt keys -> { dt = min dt 30, keys = keys }) delta Keys.signal)

port soundEvent : Signal String
port soundEvent =
  state
    |> Signal.map .sound
    |> Sound.signal
    |> Signal.map toString
