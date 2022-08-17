module View exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)
--import Graphics.Element exposing (..)

--import Vec exposing (..)


--type alias PositionedElement =
--  { coord : Coord
--  , element : Element
--  , visible : Bool -- due to react-style rendering optimizations,
--                   -- it's faster to display invisible elements
--                   -- than to remove them from the display list
--  }

type alias Model msg =
  { counter : Html msg
  --{ camera : Pixels
  --, elements : List PositionedElement
  , instructions : Html msg
  --, debug : String
  }

view : Model msg -> Html msg
view model =
  --let
  --  camera_pixels = model.camera
  --
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
  --  everything : List Html
  --  everything = List.map viewPositionedElement model.elements
  --
  --  bg_style = Attributes.style
  --    [ ("width", "640px")
  --    , ("height", "480px")
  --    , ("background-image", "url('imgs/grey.png')")
  --    , ("background-size", "28px 28px")
  --    , ("background-position", toString (-camera_pixels.x) ++ "px " ++ toString (-camera_pixels.y) ++ "px")
  --    ]
  --in
  Html.div [class "top"]
    [ Html.div [class "container"] [model.counter]
    , Html.div [class "instructions"] [model.instructions]
    ]
  --  [ Html.img [title_style, Attributes.src "imgs/title.png"] []
  --  , Html.div [container_style]
  --      (Html.div [visible_layer_style] [Html.div [bg_style] []] :: everything)
  --  , Html.div [instructions_style] [model.instructions]
  --  , Html.div [instructions_style] [Html.text model.debug]
  --  ]
