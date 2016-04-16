module Player where

import Graphics.Element as Element exposing (Element)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Keys
import Shape exposing (Shape(..))
import Vec exposing (Vec)
import View exposing (PositionedElement)


-- MODEL

type alias Model =
  { last_keys : Keys.Action
  , p : Vec Int
  , shape : Shape
  }


init : Model
init =
  { last_keys = Keys.NoOp
  , p = Vec.init
  , shape = O
  }


-- UPDATE

type alias Action = Keys.Action

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


instant_update : Keys.Action -> Model -> Model
instant_update action model = case action of
  Keys.NoOp ->
    model
  Keys.ArrowKey keys ->
    { model
    | p = model.p `Vec.plus` keys
    }
  Keys.ShapeShiftKey ->
    { model
    | shape = I
    }


-- VIEW

view : Model -> PositionedElement
view model =
  (model.p, Shape.view model.shape)
