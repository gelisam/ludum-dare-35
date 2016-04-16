module Keys where

import Char
import Keyboard
import Signal

import Vec exposing (Vec)


type Action
  = NoOp
  | ArrowKey (Vec Int)
  | RotationKey
  | ShapeShiftKey


arrowAction : Vec Int -> Action
arrowAction vec =
  if (vec.x == 0 && vec.y == 0) || (vec.x /= 0 && vec.y /= 0)
  then
    NoOp
  else
    -- Keyboard's positive Y point up, Html's Y points down. We use Html's convention.
    ArrowKey { vec | y = -vec.y }

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
signal = arrowSignal `Signal.merge` rotationSignal `Signal.merge` shapeShiftSignal
