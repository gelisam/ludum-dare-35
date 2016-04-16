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


parse : Char -> Maybe Color
parse char = case char of
  '#' -> Just Grey
  'B' -> Just Blue
  'C' -> Just Cyan
  'G' -> Just Green
  'O' -> Just Orange
  'R' -> Just Red
  'V' -> Just Violet
  'Y' -> Just Yellow
  _   -> Nothing


view : Color -> Element
view color =
  Element.image 28 28 ("/imgs/" ++ String.toLower (toString color) ++ ".png")
