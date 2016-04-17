module Powerup where

import Graphics.Element as Element exposing (Element)

import Shape exposing (Shape(..), Orientation(..))
import Vec exposing (Coord, Vec)


type Powerup
  = FixedShape Shape Orientation Coord
  | Jump
  | Rotate
  | ShapeShift


type alias PowerupId = Int

id : Powerup -> PowerupId
id powerup = case powerup of
  FixedShape _ _ _ -> 0
  Jump -> 1
  Rotate -> 2
  ShapeShift -> 3


view : Powerup -> Element
view powerup = case powerup of
  FixedShape shape orientation _ ->
    Element.image 28 28 ("/imgs/" ++ toString shape ++ "_" ++ toString orientation ++ ".png")
  Jump ->
    Element.image 28 28 "/imgs/jump.png"
  Rotate ->
    Element.image 28 28 "/imgs/rotate.png"
  ShapeShift ->
    Element.image 28 28 "/imgs/shapeshift.png"
