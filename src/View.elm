module View exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, src, style)
--import Graphics.Element exposing (..)

import Vec exposing (..)


--type alias PositionedElement =
--  { coord : Coord
--  , element : Element
--  , visible : Bool -- due to react-style rendering optimizations,
--                   -- it's faster to display invisible elements
--                   -- than to remove them from the display list
--  }

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

  --  positionElement : PositionedElement -> Element
  --  positionElement positionedElement =
  --    let
  --      relative_pixels = pixels positionedElement.coord `minus` camera_pixels
  --      position = topLeftAt
  --        (absolute relative_pixels.x)
  --        (absolute relative_pixels.y)
  --    in
  --      container 640 480 position positionedElement.element
  --
  --  viewLayer : Bool -> Html -> Html
  --  viewLayer visible html =
  --    Html.div [if visible then visible_layer_style else invisible_layer_style] [html]
  --
  --  viewPositionedElement : PositionedElement -> Html
  --  viewPositionedElement positionedElement =
  --    positionedElement
  --      |> positionElement
  --      |> Html.fromElement
  --      |> viewLayer positionedElement.visible
  --
      everything : List (Html msg)
      everything = [model.counter]
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
