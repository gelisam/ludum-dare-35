module Keys exposing (Action(..), sub)

import Browser.Events
import Char
import Json.Decode exposing (Decoder)
import Platform.Sub

import Vec exposing (Vec)


type Action
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

keyAction : String -> Action
keyAction key =
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

sub : Sub Action
sub =
  Browser.Events.onKeyDown keyDecoder
    |> Sub.map keyAction
