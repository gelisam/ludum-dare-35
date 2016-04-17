module Instructions where

import Html exposing (Html)


-- MODEL

type Model
  = Intro
  | CannotRotate
  | HowToFixedShift
  | HowToUp
  | HowToJump
  | HowToRotate
  | HowToShapeShift
  | HowToWin

init : Model
init = Intro


-- UPDATE

type alias Action = Model

update : Action -> Model -> Model
update action _ = action


-- VIEW

view : Model -> Html
view model = case model of
  Intro -> Html.text "Seems easy enough. Clear the lines!"
  CannotRotate -> Html.text "Hmm, it seems the rotation button is broken?"
  HowToFixedShift -> Html.text ""
  HowToUp -> Html.text "An up arrow! Can I rotate now?"
  HowToJump -> Html.text "Moving upwards? I that even a legal move?"
  HowToRotate -> Html.text "Press Z!"
  HowToShapeShift -> Html.text "Press X!"
  HowToWin -> Html.text "Finally! The lines were cleared and thanks to your sacrifice, the other blocks lived happily ever after. THE END"
