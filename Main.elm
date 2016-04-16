module Main where

import AnimationFrame
import Html exposing (Html)

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

update : (Float, Keys.Action) -> Model -> Model
update (dt, keys) model =
  let
    player' = Player.update keys model.player
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
        | player = List.foldr Player.pickup player' pickedPowerups
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
      model.player.powerupIds
  }


-- SIGNALS

main : Signal Html
main =
  Signal.map view (Signal.foldp update init input)


-- disable animations
input : Signal (Float, Keys.Action)
input = Signal.map (\action -> (0, action)) Keys.signal

-- input : Signal (Float, Keys.Action)
-- input =
--   let
--     delta = Signal.map (\t -> t/20) AnimationFrame.frame
--   in
--     Signal.sampleOn delta (Signal.map2 (,) delta Keys.signal)
