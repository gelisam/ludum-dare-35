port module Counter exposing (Model, Msg, init, view, update)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Instructions
import View

type alias Model = Int

type Msg = Increment | Decrement

port soundEvent : String -> Cmd msg

init : flags -> ( Model, Cmd msg )
init _ =
  ( 0, Cmd.none )

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Increment ->
      ( model + 1, soundEvent "PlayFixedShapeSoundEffect" )

    Decrement ->
      ( model - 1, Cmd.none )
