module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Block
import Debug
import Grid
import Instructions
import Keys
import Level
import Powerup
import Powerups
import Shape
import Vec
import View

type alias Flags = {}

type alias Model =
  { instructions : Instructions.Model
  , debug : String
  }

type alias Msg = Keys.Action

main : Program Flags Model Msg
main =
  Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : flags -> ( Model, Cmd msg )
init _ =
  ( { instructions =
        Instructions.init
    , debug =
        ""
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  View.view
    { camera =
        { x = 0
        , y = 0
        }
    , images =
        ( Level.view
       ++ Shape.view {x = 10, y = 3} Shape.L Shape.R0
        )
    , instructions =
        Instructions.view model.instructions
    , debug =
        model.debug
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( { instructions =
        model.instructions
    , debug =
        model.debug ++ Debug.toString msg
    }
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Keys.sub
