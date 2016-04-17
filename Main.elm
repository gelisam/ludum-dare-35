module Main where

import AnimationFrame
import Html exposing (Html)
import Time exposing (Time)

import Ending
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
  { player : Player.Model
  , powerups : Powerups
  , instructions : Instructions.Model
  , ending : Ending.Model
  }


init : Model
init =
  { player = Player.init Level.player_start
  , powerups = Level.powerups_start
  , instructions = Instructions.init
  , ending = Ending.init Level.goal_coord
  }


-- UPDATE

unlessCollision : (Player.Model -> Player.Model) -> Player.Model -> Player.Model
unlessCollision f player =
  let
    player' = f player
    collision = Level.collides player'.coord (Player.block_grid player')
  in
    if collision then player else player'

update : Player.Action -> Model -> Model
update action model =
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
  | player = unlessCollision (Player.pickup powerup) model.player
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


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      model.player.coord
  , elements =
      [Level.view] ++
      Powerups.view model.powerups ++
      [Player.view model.player] ++
      Ending.view model.ending
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
    Signal.sampleOn delta (Signal.map2 (\dt keys -> { dt = dt, keys = keys }) delta Keys.signal)
