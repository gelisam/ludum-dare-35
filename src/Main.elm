port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Flags = {}

type alias Model = Int

type Msg = Increment | Decrement

port soundEvent : String -> Cmd msg

main : Program Flags Model Msg
main =
  Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }

init : flags -> ( Model, Cmd msg )
init _ =
  ( 0, Cmd.none )

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment ->
      ( model + 1, soundEvent "PlayFixedShapeSoundEffect" )

    Decrement ->
      ( model - 1, Cmd.none )

subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none
