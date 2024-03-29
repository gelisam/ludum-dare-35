module Keys exposing (Msg(..), sub)

import Browser.Events
import Char
import Json.Decode exposing (Decoder)
import Platform.Sub

import Vec exposing (Vec)


type Msg
  = NoOp
  | LeftKey
  | RightKey
  | UpKey
  | DownKey
  | RotationKey
  | ShapeShiftKey


keyDecoder : Decoder String
keyDecoder =
  Json.Decode.field "key" Json.Decode.string

keyMsg : String -> Msg
keyMsg key =
  case key of
    "ArrowLeft" ->
      LeftKey
    "ArrowRight" ->
      RightKey
    "ArrowUp" ->
      UpKey
    "ArrowDown" ->
      DownKey
    "z" ->
      RotationKey
    "x" ->
      ShapeShiftKey
    _ ->
      NoOp

sub : (Msg -> msg) -> (Msg -> msg) -> Sub msg
sub pressed released =
  Sub.batch
    [ Browser.Events.onKeyDown keyDecoder
        |> Sub.map keyMsg
        |> Sub.map pressed
    , Browser.Events.onKeyUp keyDecoder
        |> Sub.map keyMsg
        |> Sub.map released
    ]
