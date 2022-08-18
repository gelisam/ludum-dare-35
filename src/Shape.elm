module Shape exposing
  ( Shape(..)
  , Orientation(..)
  , nextShape
  , nextOrientation
  , shapeString
  , orientationString
  , block_grid
  , view
  )

import Set exposing (Set)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Vec exposing (Coord)
import View exposing (PositionedImage)


type Shape = O | L | J | Z | S | T | I
type Orientation = R0 | R1 | R2 | R3


nextShape : Shape -> Shape
nextShape shape = case shape of
  O -> L
  L -> J
  J -> Z
  Z -> S
  S -> T
  T -> I
  I -> O

nextOrientation : Orientation -> Orientation
nextOrientation orientation = case orientation of
  R0 -> R1
  R1 -> R2
  R2 -> R3
  R3 -> R0


shapeString : Shape -> String
shapeString shape =
  case shape of
    O ->
      "O"
    L ->
      "L"
    J ->
      "J"
    Z ->
      "Z"
    S ->
      "S"
    T ->
      "T"
    I ->
      "I"

orientationString : Orientation -> String
orientationString orientation =
  case orientation of
    R0 ->
      "R0"
    R1 ->
      "R1"
    R2 ->
      "R2"
    R3 ->
      "R3"


char_grid : Shape -> Orientation -> Grid Char
char_grid shape orientation = case (shape, orientation) of
  (O, _) -> Grid.init [ "    "
                      , " YY "
                      , " YY "
                      , "    "
                      ]
  (I, R0) -> Grid.init [ "    "
                       , "CCCC"
                       , "    "
                       , "    "
                       ]
  (I, R1) -> Grid.init [ "  C "
                       , "  C "
                       , "  C "
                       , "  C "
                       ]
  (S, R0) -> Grid.init [ "    "
                       , "  GG"
                       , " GG "
                       , "    "
                       ]
  (S, R1) -> Grid.init [ "  G "
                       , "  GG"
                       , "   G"
                       , "    "
                       ]
  (Z, R0) -> Grid.init [ "    "
                       , " RR "
                       , "  RR"
                       , "    "
                       ]
  (Z, R1) -> Grid.init [ "   R"
                       , "  RR"
                       , "  R "
                       , "    "
                       ]
  (L, R0) -> Grid.init [ "    "
                       , " OOO"
                       , " O  "
                       , "    "
                       ]
  (L, R1) -> Grid.init [ "  O "
                       , "  O "
                       , "  OO"
                       , "    "
                       ]
  (L, R2) -> Grid.init [ "   O"
                       , " OOO"
                       , "    "
                       , "    "
                       ]
  (L, R3) -> Grid.init [ " OO "
                       , "  O "
                       , "  O "
                       , "    "
                       ]
  (J, R0) -> Grid.init [ "    "
                       , " BBB"
                       , "   B"
                       , "    "
                       ]
  (J, R1) -> Grid.init [ "  BB"
                       , "  B "
                       , "  B "
                       , "    "
                       ]
  (J, R2) -> Grid.init [ " B  "
                       , " BBB"
                       , "    "
                       , "    "
                       ]
  (J, R3) -> Grid.init [ "  B "
                       , "  B "
                       , " BB "
                       , "    "
                       ]
  (T, R0) -> Grid.init [ "    "
                       , " VVV"
                       , "  V "
                       , "    "
                       ]
  (T, R1) -> Grid.init [ "  V "
                       , "  VV"
                       , "  V "
                       , "    "
                       ]
  (T, R2) -> Grid.init [ "  V "
                       , " VVV"
                       , "    "
                       , "    "
                       ]
  (T, R3) -> Grid.init [ "  V "
                       , " VV "
                       , "  V "
                       , "    "
                       ]
  (_, R2) -> char_grid shape R0
  (_, R3) -> char_grid shape R1

block_grid : Shape -> Orientation -> Grid (Maybe Block)
block_grid shape orientation = Grid.map Block.parse <| char_grid shape orientation

view : Coord -> Shape -> Orientation -> List PositionedImage
view coord shape orientation =
  block_grid shape orientation
    |> Grid.map Block.viewTransparent
    |> Grid.viewTransparent coord
