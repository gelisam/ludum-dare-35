module Main where

import AnimationFrame
import Html exposing (Html)

import Keys
import Level
import Player
import Powerup exposing (Powerup)
import Vec exposing (Coord, Vec)
import View


-- MODEL

type alias Model =
  { player : Player.Model
  , powerups : List (Coord, Powerup)
  }


init : Model
init =
  { player = Player.init Level.player_start
  , powerups = Level.powerups_start
  }


-- UPDATE

update : (Float, Keys.Action) -> Model -> Model
update (dt, keys) model =
  let
    player' = Player.update keys model.player
    collision = Level.collides player'.coord (Player.block_grid player')
  in
    if collision
    then model
    else
      { model
      | player = player'
      }


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      model.player.coord
  , elements =
      Level.view
        :: Player.view model.player
        :: List.map (\(coord, powerup) -> (coord, Powerup.view powerup)) model.powerups
  , debug = toString
      model.player.powerupIds
  }


-- SIGNALS

main : Signal Html
main =
  Signal.map view (Signal.foldp update init input)


input : Signal (Float, Keys.Action)
input =
  let
    delta = Signal.map (\t -> t/20) AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keys.signal)
