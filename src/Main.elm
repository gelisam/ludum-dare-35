module Main exposing (main)

import Array
import Browser
import Either
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Block
import Grid
import Instructions
import Keys
import Level
import Player
import Powerup
import Powerups
import Shape
import Vec
import View

type alias Flags = {}

type alias Model =
  { player : Player.Model
  , instructions : Instructions.Model
  , debug : String
  }

type alias Msg = Player.Msg

main : Program Flags Model Msg
main =
  Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

failedInit : String -> ( Model, Cmd msg )
failedInit errorMessage =
  ( { player =
        Player.init Vec.init  -- any random position will do
    , instructions =
        Instructions.init
    , debug =
        errorMessage
    }
  , Cmd.none
  )

init : flags -> ( Model, Cmd msg )
init _ =
  Either.unpack failedInit (\player_start ->
    ( { player =
          Player.init player_start
      , instructions =
          Instructions.init
      , debug =
          ""
      }
    , Cmd.none
    ))
    Level.player_start

view : Model -> Html Msg
view model =
  View.view
    { camera =
        { x = 1000
        , y = 0
        }
    , images =
        ( Level.view
       ++ Shape.view {x = 10, y = 3} Shape.L Shape.R0
       ++ Player.view model.player
        )
    , instructions =
        Instructions.view model.instructions
    , debug =
        model.debug
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( { player =
        Player.update msg model.player
    , instructions =
        model.instructions
    , debug =
        model.debug
    }
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Sub.map Player.KeyPressed Keys.sub
    , Sub.map Player.TimePasses Player.sub
    ]
