module View exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)

import Vec exposing (..)


type alias ImagePath = String

type alias PositionedImage =
  { coord : Coord
  , src : ImagePath
  , visible : Bool -- due to react-style rendering optimizations,
                   -- it's faster to display invisible images
                   -- than to remove them from the display list
  }

type alias Model msg =
  { started : Bool
  , camera : Pixels
  , images : List PositionedImage
  , instructions : Html msg
  , debug : String
  }

view : msg -> Model msg -> Html msg
view start model =
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

      everything : List (Html msg)
      everything = List.map viewPositionedImage model.images

      background : Html msg
      background =
        Html.div [class "visible"]
          [ Html.div
              [ class "background"
              , backgroundPosition (Vec.init |> minus camera_pixels)
              ]
              []
          ]

      playButton : Html msg
      playButton =
        Html.img
          [ src "imgs/play.png"
          , style "position" "relative"
          , style "width" "64px"
          , style "height" "64px"
          , style "left" (String.fromInt ((640 - 64) // 2) ++ "px")
          , style "top" (String.fromInt ((480 - 64) // 2) ++ "px")
          , onClick start
          ]
          []
  in
  Html.div [class "top"]
    [ Html.img [class "title", src "imgs/title.png"] []
    , Html.div [class "container"]
        ( Html.div
            (if model.started then [] else [class "disabled"])
            ( [background]
           ++ everything
            )
       :: if model.started then [] else [playButton]
        )
    , Html.div [class "instructions"] [model.instructions]
    , Html.div [class "instructions"] [Html.text model.debug]
    ]
