module View exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, src, style)
import Collage exposing (Collage, image)
import Collage.Layout exposing (at, spacer, bottomRight)
import Collage.Render

import Vec exposing (..)


type alias PositionedElement msg =
  { coord : Coord
  , element : Collage msg
  , visible : Bool -- due to react-style rendering optimizations,
                   -- it's faster to display invisible elements
                   -- than to remove them from the display list
  }

type alias Model msg =
  { camera : Pixels
  , counter : Html msg
  --, elements : List PositionedElement
  , instructions : Html msg
  --, debug : String
  }

view : Model msg -> Html msg
view model =
  let
      camera_pixels = model.camera

      positionElement : PositionedElement msg -> Collage msg
      positionElement positionedElement =
        let
            relative_pixels = pixels positionedElement.coord |> minus camera_pixels
        in
        spacer (toFloat relative_pixels.x) (toFloat relative_pixels.y)
          |> at bottomRight positionedElement.element

      viewLayer : Bool -> Html msg -> Html msg
      viewLayer visible html =
        Html.div [if visible then class "visible_layer" else class "invisible_layer"] [html]

      viewPositionedElement : PositionedElement msg -> Html msg
      viewPositionedElement positionedElement =
        positionedElement
          |> positionElement
          |> Collage.Render.svg
          |> viewLayer positionedElement.visible

      examplePositionedElement : PositionedElement msg
      examplePositionedElement =
        { coord =
            { x = 0
            , y = 0
            }
        , element =
            image (28, 28) "imgs/white.png"
        , visible =
            True
        }

      everything : List (Html msg)
      everything = [model.counter, viewPositionedElement examplePositionedElement]
      --everything = List.map viewPositionedElement model.elements

      background : Html msg
      background =
        Html.div [class "visible_layer"]
          [ Html.div
              [ class "background"
              , style
                  "background-position"
                  ( String.fromInt (-camera_pixels.x) ++ "px "
                 ++ String.fromInt (-camera_pixels.y) ++ "px"
                  )
              ]
              []
          ]
  in
  Html.div [class "top"]
    [ Html.img [class "title", src "imgs/title.png"] []
    , Html.div [class "container"]
        (background :: everything)
    , Html.div [class "instructions"] [model.instructions]
    --, Html.div [instructions_style] [Html.text model.debug]
    ]
