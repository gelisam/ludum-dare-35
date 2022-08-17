module View exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, src, style)

import Vec exposing (..)


type alias PositionedImage =
  { coord : Coord
  , src : String
  , visible : Bool -- due to react-style rendering optimizations,
                   -- it's faster to display invisible images
                   -- than to remove them from the display list
  }

type alias Model msg =
  { camera : Pixels
  , counter : Html msg
  --, images : List PositionedImage
  , instructions : Html msg
  --, debug : String
  }

view : Model msg -> Html msg
view model =
  let
      camera_pixels = model.camera

      relativePosition : Pixels -> List (Attribute msg)
      relativePosition pos =
        [ style "position" "relative"
        , style "left" (String.fromInt pos.x ++ "px")
        , style "top" (String.fromInt pos.y ++ "px")
        ]

      backgroundPosition : Pixels -> Attribute msg
      backgroundPosition pos =
        style
          "background-position"
          ( String.fromInt pos.x ++ "px "
         ++ String.fromInt pos.y ++ "px"
          )

      viewPositionedImage : PositionedImage -> Html msg
      viewPositionedImage positionedImage =
        let
            relative_pixels = pixels positionedImage.coord |> minus camera_pixels
            visibility_class = if positionedImage.visible then "visible" else "invisible"
        in
        Html.div
          [class visibility_class]
          [ Html.img
              ( class "image"
             :: src positionedImage.src
             :: relativePosition relative_pixels
              )
              []
          ]

      examplePositionedImage : PositionedImage
      examplePositionedImage =
        { coord =
            { x = 0
            , y = 0
            }
        , src =
            "imgs/white.png"
        , visible =
            True
        }

      everything : List (Html msg)
      everything = [viewPositionedImage examplePositionedImage]
      --everything = List.map viewPositionedImage model.elements

      background : Html msg
      background =
        Html.div [class "visible"]
          [ Html.div
              [ class "background"
              , backgroundPosition (Vec.init |> minus camera_pixels)
              ]
              []
          ]
  in
  Html.div [class "top"]
    [ Html.img [class "title", src "imgs/title.png"] []
    , Html.div [class "container"]
        (background :: everything)
    , Html.div [class "instructions"] [model.instructions]
    , model.counter
    --, Html.div [instructions_style] [Html.text model.debug]
    ]
