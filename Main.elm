module Main where

import AnimationFrame
import Html exposing (Html)

import Keys
import Level
import Player
import Vec exposing (Vec)
import View


-- MODEL

type alias Model =
  { player : Player.Model
  , level : Level.Model
  }


init : Model
init =
  { player = Player.init
  , level = Level.init
  }


-- UPDATE

update : (Float, Keys.Action) -> Model -> Model
update (dt, keys) model =
  { model
  | player = Player.update keys model.player
  , level = Level.update Level.NoOp model.level
  }


-- VIEW

view : Model -> Html
view model = View.view
  { camera =
      model.player.p
  , elements =
      [ Level.view model.level
      , Player.view model.player
      ]
  , debug = toString
      model.player.last_keys
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
