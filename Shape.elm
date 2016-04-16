module Shape where

import Graphics.Element as Element exposing (Element)

import Block exposing (Block(..))
import Grid exposing (Grid)


type Shape = O | L | Z | S | J | T | I

char_grid : Shape -> Grid Char
char_grid shape = case shape of
  O -> Grid.init [ "YY"
                 , "YY"
                 ]
  L -> Grid.init [ "  O"
                 , "OOO"
                 ]
  Z -> Grid.init [ "RR "
                 , " RR"
                 ]
  S -> Grid.init [ " GG"
                 , "GG "
                 ]
  J -> Grid.init [ "B  "
                 , "BBB"
                 ]
  T -> Grid.init [ " V "
                 , "VVV"
                 ]
  I -> Grid.init [ "CCCC"
                 ]

block_grid : Shape -> Grid (Maybe Block)
block_grid = Grid.map Block.parse << char_grid

view : Shape -> Element
view = Grid.view << Grid.map Block.viewTransparent << block_grid
