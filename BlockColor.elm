module BlockColor where

import Graphics.Element as Element exposing (Element)
import String


type Color
  = White
  | Grey
  | Blue
  | Cyan
  | Green
  | Orange
  | Red
  | Violet
  | Yellow


opaqueColor : Char -> Color
opaqueColor char = case char of
  ' ' -> White
  'B' -> Blue
  'C' -> Cyan
  'G' -> Green
  'O' -> Orange
  'R' -> Red
  'V' -> Violet
  'Y' -> Yellow
  _   -> Grey

-- ' ' for transparent
transparentColor : Char -> Maybe Color
transparentColor char = case opaqueColor char of
  White -> Nothing
  color -> Just color


view : Color -> Element
view color =
  Element.image 28 28 ("/imgs/" ++ String.toLower (toString color) ++ ".png")
