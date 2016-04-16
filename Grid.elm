module Grid where

import Array exposing (Array)
import Graphics.Element as Element exposing (..)
import String

import Color exposing (Color(..))


type alias Grid a = Array (Array a)


init : List String -> Grid Char
init strings =
  let
    row : String -> Array Char
    row = Array.fromList << String.toList
  in
    Array.fromList (List.map row strings)

map : (a -> b) -> Grid a -> Grid b
map = Array.map << Array.map


viewOpaque : Grid Element -> Element
viewOpaque =
  flow down << Array.toList << Array.map (flow right << Array.toList)

viewTransparent : Grid (Maybe Element) -> Element
viewTransparent =
  viewOpaque << map (Maybe.withDefault (spacer 28 28))
