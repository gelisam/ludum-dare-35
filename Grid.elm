module Grid where

import Array exposing (Array)
import Graphics.Element as Element exposing (..)
import String

import Block exposing (Block(..))
import Vec exposing (Coord)


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

keys : Grid a -> List Coord
keys grid =
  let
      xs = [0 .. width grid-1]
      ys = [0 .. height grid-1]
      key y x = { y = y, x = x }
      row_keys y = List.map (key y) xs
  in
  List.concatMap row_keys ys


view : Grid Element -> Element
view =
  flow down << Array.toList << Array.map (flow right << Array.toList)
