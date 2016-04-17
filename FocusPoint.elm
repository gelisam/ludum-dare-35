module FocusPoint where

import Vec exposing (..)


type alias FocusPoint = Coord


isClose : Coord -> FocusPoint -> Bool
isClose coord1 coord2 =
  let
    vector = coord1 `minus` coord2
    manhatan_distance = abs vector.x + abs vector.y
  in
    manhatan_distance < 12
