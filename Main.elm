module Main where

import AnimationFrame
import Html exposing (Html)
import Time exposing (Time)

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
  }


init : Model
init =
  { player = Player.init Level.player_start
  , powerups = Level.powerups_start
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
      in
        { model
        | player = List.foldr (unlessCollision << Player.pickup) player' pickedPowerups
        , powerups = remainingPowerups
        }


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      model.player.coord
  , elements =
      [Level.view] ++
      Powerups.view model.powerups ++
      [Player.view model.player]
  , debug = toString
      (model.player.inactive_dt, Time.second)
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
