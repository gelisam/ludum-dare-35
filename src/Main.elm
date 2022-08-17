module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Block
import Counter
import Grid
import Instructions
import Vec
import View

type alias Flags = {}

type alias Model =
  { counter : Int
  , instructions : Instructions.Model
  }

type alias Msg = Counter.Msg

main : Program Flags Model Msg
main =
  Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : flags -> ( Model, Cmd msg )
init _ =
  ( { counter =
        0
    , instructions =
        Instructions.init
    }
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  View.view
    { camera =
        { x = model.counter
        , y = 0
        }
    , counter =
        Counter.view model.counter
    , images =
        Array.fromList
          [ Array.fromList
              [ Just Block.Orange
              , Just Block.Yellow
              ]
          , Array.fromList
              [ Nothing
              , Just Block.Yellow
              ]
          ]
          |> Grid.map Block.viewTransparent
          |> Grid.view
    , instructions =
        Instructions.view model.instructions
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
      (counter, cmd) = Counter.update msg model.counter
  in
  ( { counter =
        counter
    , instructions =
        model.instructions
    }
  , cmd
  )

subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none
