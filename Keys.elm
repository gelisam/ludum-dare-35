module Keys where

import Char
import Keyboard
import Signal

import Vec exposing (Vec)


type Action
  = NoOp
  | LeftKey
  | RightKey
  | UpKey
  | DownKey
  | RotationKey
  | ShapeShiftKey


arrowAction : Vec Int -> Action
arrowAction vec =
  if (vec.x == -1 && vec.y == 0) then LeftKey
  else if (vec.x == 1 && vec.y == 0) then RightKey
  else if (vec.x == 0 && vec.y == 1) then UpKey
  else if (vec.x == 0 && vec.y == -1) then DownKey
  else NoOp

rotationAction : Bool -> Action
rotationAction isDown = if isDown then RotationKey else NoOp

shapeShiftAction : Bool -> Action
shapeShiftAction isDown = if isDown then ShapeShiftKey else NoOp


arrowSignal : Signal Action
arrowSignal = Keyboard.arrows
  |> Signal.map arrowAction

rotationSignal : Signal Action
rotationSignal = Keyboard.isDown (Char.toCode 'Z')
  |> Signal.map rotationAction

shapeShiftSignal : Signal Action
shapeShiftSignal = Keyboard.isDown (Char.toCode 'X')
  |> Signal.map shapeShiftAction

signal : Signal Action
signal =
  arrowSignal
    |> Signal.merge rotationSignal
    |> Signal.merge shapeShiftSignal
