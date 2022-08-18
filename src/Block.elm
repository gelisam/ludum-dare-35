module Block exposing (..)

import View exposing (ImagePath)


type Block
  = White
  | Grey
  | Blue
  | Cyan
  | Green
  | Orange
  | Red
  | Violet
  | Yellow


parse : Char -> Maybe Block
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


viewOpaque : Block -> ImagePath
viewOpaque color =
  case color of
    White ->
      "imgs/white.png"
    Grey ->
      "imgs/grey.png"
    Blue ->
      "imgs/blue.png"
    Cyan ->
      "imgs/cyan.png"
    Green ->
      "imgs/green.png"
    Orange ->
      "imgs/orange.png"
    Red ->
      "imgs/red.png"
    Violet ->
      "imgs/violet.png"
    Yellow ->
      "imgs/yellow.png"

viewTransparent : Maybe Block -> Maybe ImagePath
viewTransparent = 
  Maybe.map viewOpaque
