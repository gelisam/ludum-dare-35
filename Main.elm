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
        let
          (pickedPowerups, remainingPowerups) =
            Powerups.pickup player'.coord (Player.block_grid player') model.powerups
          ending_picked =
            Ending.pickup player'.coord (Player.block_grid player') model.ending
          ending_action =
            if ending_picked then Ending.TheEnd else Ending.NoOp
        in
          { model
          | player = List.foldr (unlessCollision << Player.pickup) player' pickedPowerups
          , powerups = remainingPowerups
          , ending = Ending.update (action.dt, ending_action) model.ending
          }


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
