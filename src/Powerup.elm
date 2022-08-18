module Powerup exposing (Powerup(..), view)

import Shape exposing (Shape(..), Orientation(..), shapeString, orientationString)
import Vec exposing (Coord, Vec)
import View exposing (PositionedImage)


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

view : Coord -> Powerup -> Bool -> PositionedImage
view coord powerup visible =
  { coord = coord
  , src = case powerup of
      FixedShape shape orientation _ ->
        "imgs/" ++ shapeString shape ++ "_" ++ orientationString orientation ++ ".png"
      Jump ->
        "imgs/jump.png"
      Rotate ->
        "imgs/rotate.png"
      ShapeShift ->
        "imgs/shapeshift.png"
  , visible = visible
  }
