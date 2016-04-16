module Player where

import Graphics.Element as Element exposing (Element)

import Keys exposing (Keys)
import Vec exposing (Vec)
import View exposing (PositionedElement)


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
