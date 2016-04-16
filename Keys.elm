module Keys where

import Keyboard
import Signal

import Vec exposing (Vec)


type alias Keys = Vec Int


-- Elm's positive Y point up, HTML's Y points down. We use HTML's convention.
flipY : Keys -> Keys
flipY keys = { keys | y = -keys.y }

signal : Signal Keys
signal = Signal.map flipY Keyboard.arrows
