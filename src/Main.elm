module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Counter
import Instructions
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
    { counter =
        Counter.view model.counter
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