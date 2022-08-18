module Instructions exposing (..)

import Html exposing (Html)

import Powerup exposing (Powerup(..))


-- MODEL

type Model
  = Intro
  | CannotRotate
  | HowToUsePowerup Powerup
  | UnlikelyJump
  | YouWin

init : Model
init = Intro


-- UPDATE

type alias Msg = Model

update : Msg -> Model -> Model
update action _ = action


-- VIEW

view : Model -> Html msg
view model = case model of
  Intro -> Html.text "Seems easy enough. Clear the lines!"
  CannotRotate -> Html.text "Hmm, it seems the rotation button is broken?"
  HowToUsePowerup (FixedShape _ _ _) -> Html.text ""
  HowToUsePowerup Jump -> Html.text "An up arrow! Can I rotate now?"
  UnlikelyJump -> Html.text "Moving upwards? I that even a legal move?"
  HowToUsePowerup Rotate -> Html.text "Press Z!"
  HowToUsePowerup ShapeShift -> Html.text "Press X!"
  YouWin -> Html.text "Finally, the lines were cleared! Thanks to your sacrifice, the other blocks lived happily ever after. THE END"
