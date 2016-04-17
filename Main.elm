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
import Vec exposing (Coord, Vec)
import View


-- MODEL

type alias Model =
  { camera : Camera.Model
  , player : Player.Model
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
  , player = Player.init Level.player_start
  , powerups = Level.powerups_start
  , instructions = Instructions.init
  , ending = Ending.init Level.goal_coord
  }


-- UPDATE

update : Player.Action -> Model -> Model
update action =
  game_update action >>
  camera_update action

game_update : Player.Action -> Model -> Model
game_update action model =
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
      then model
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
    model' = List.foldr pickup_powerup model pickedPowerups
  in
    { model'
    | powerups = remainingPowerups
    }

pickup_powerup : Powerup -> Model -> Model
pickup_powerup powerup model =
  { model
  | player = Player.pickup powerup model.player
  , instructions = Instructions.HowToUsePowerup powerup
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


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      Camera.view model.camera
  , elements = List.concat
      [ [Level.view]
      , Powerups.view model.powerups
      , [Player.view model.player]
      , [Ending.view model.ending]
      ]
  , instructions = Instructions.view model.instructions
  , debug = toString
      (round (model.ending.elapsed / 100 * Time.millisecond))
  }


-- SIGNALS

main : Signal Html
main =
  Signal.map view (Signal.foldp update init input)


input : Signal Player.Action
input =
  let
    delta = AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (\dt keys -> { dt = min dt 30, keys = keys }) delta Keys.signal)
