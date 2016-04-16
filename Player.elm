module Player where

import Graphics.Element as Element exposing (Element)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Keys exposing (Keys)
import Vec exposing (Vec)
import View exposing (PositionedElement)


char_shapes : List (Grid Char)
char_shapes = List.map Grid.init
  [ [ "YY"
    , "YY"
    ]
  , [ "  O"
    , "OOO"
    ]
  , [ "RR "
    , " RR"
    ]
  , [ " GG"
    , "GG "
    ]
  , [ "B  "
    , "BBB"
    ]
  , [ " V "
    , "VVV"
    ]
  , [ "CCCC"
    ]
  ]

element_shape : Grid Char -> Element
element_shape = Grid.view << Grid.map (Block.viewTransparent << Block.parse)

element_shapes : List Element
element_shapes = List.map element_shape char_shapes


-- MODEL

type alias Model =
  { last_keys : Keys
  , p : Keys
  }


init : Model
init =
  { last_keys = Vec.init
  , p = Vec.init
  }


-- UPDATE

type alias Action = Keys

update : Action -> Model -> Model
update action model =
  if model.last_keys == action
  then
    model
  else
    let
      model' = instant_update action model
    in
      { model' | last_keys = action }


instant_update : Keys -> Model -> Model
instant_update keys model =
  { model
  | p = model.p `Vec.plus` keys
  }


-- VIEW

view : Model -> PositionedElement
view model =
  (model.p, Element.image 28 28 "/imgs/red.png")
