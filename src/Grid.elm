module Grid exposing (..)

import Array exposing (Array)
import String

import Block exposing (Block(..))
import Vec exposing (Coord, Pixels, plus)
import View exposing (ImagePath, PositionedImage)


type alias Grid a = Array (Array a)


init : List String -> Grid Char
init strings =
  let
      row : String -> Array Char
      row = Array.fromList << String.toList
  in
  Array.fromList (List.map row strings)


width : Grid a -> Int
width grid = case Array.get 0 grid of
  Just row -> Array.length row
  Nothing -> 0

height : Grid a -> Int
height = Array.length

get : Coord -> Grid a -> Maybe a
get coord grid =
  Array.get coord.y grid |> Maybe.andThen (Array.get coord.x)

map : (a -> b) -> Grid a -> Grid b
map = Array.map << Array.map

indexedMap : (Coord -> a -> b) -> Grid a -> Grid b
indexedMap f =
  Array.indexedMap (\y ->
    Array.indexedMap (\x ->
      f { x = x, y = y}))

keys : Grid a -> List Coord
keys grid =
  let
      xs = List.range 0 (width grid-1)
      ys = List.range 0 (height grid-1)
      key y x = { y = y, x = x }
      row_keys y = List.map (key y) xs
  in
  List.concatMap row_keys ys


viewOpaque : Coord -> Grid ImagePath -> List PositionedImage
viewOpaque pos grid =
  grid
    |> indexedMap (\coord imagePath ->
         { coord =
             coord |> plus pos
         , src =
             imagePath
         , visible =
             True
         })
    |> Array.toList
    |> List.concatMap Array.toList

viewTransparent : Coord -> Grid (Maybe ImagePath) -> List PositionedImage
viewTransparent pos grid =
  grid
    |> indexedMap (\coord ->
         Maybe.map (\imagePath ->
           { coord =
               coord |> plus pos
           , src =
               imagePath
           , visible =
               True
           }))
    |> Array.toList
    |> List.concatMap (Array.toList >> List.filterMap identity)
