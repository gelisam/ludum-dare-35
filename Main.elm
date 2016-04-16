module Main where

import AnimationFrame
import Html exposing (Html)

import Keys exposing (Keys)
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

update : (Float, Keys) -> Model -> Model
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
      [ (Vec.init, Level.view model.level)
      , (model.player.p, Player.view model.player)
      ]
  , debug = toString
      (model.player.p.x, model.player.p.y)
  }


-- SIGNALS

main : Signal Html
main =
  Signal.map view (Signal.foldp update init input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keys.signal)
