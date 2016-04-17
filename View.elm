module View where

import Html exposing (Html)
import Html.Attributes as Attributes
import Graphics.Element exposing (..)

import Vec exposing (..)


type alias PositionedElement =
  { coord : Coord
  , element : Element
  , visible : Bool -- due to react-style rendering optimizations,
                   -- it's faster to display invisible elements
                   -- than to remove them from the display list
  }

type alias Model =
  { camera : Pixels
  , elements : List PositionedElement
  , instructions : Html
  , debug : String
  }

view : Model -> Html
view model =
  let
    camera_pixels = model.camera
    
    positionElement : PositionedElement -> Element
    positionElement positionedElement =
      let
        relative_pixels = pixels positionedElement.coord `minus` camera_pixels
        position = topLeftAt
          (absolute relative_pixels.x)
          (absolute relative_pixels.y)
      in
        container 640 480 position positionedElement.element
    
    viewLayer : Bool -> Html -> Html
    viewLayer visible html =
      Html.div [if visible then visible_layer_style else invisible_layer_style] [html]
    
    viewPositionedElement : PositionedElement -> Html
    viewPositionedElement positionedElement =
      positionedElement
        |> positionElement
        |> Html.fromElement
        |> viewLayer positionedElement.visible
    
    everything : List Html
    everything = List.map viewPositionedElement model.elements
    
    top_style = Attributes.style
      [ ("image-rendering", "pixelated")
      ]
    
    title_style = Attributes.style
      [ ("margin-top", "2em")
      , ("margin-left", "auto")
      , ("margin-right", "auto")
      , ("display", "block")
      , ("width", "363px")
      , ("height", "45px")
      ]
    
    container_style = Attributes.style
      [ ("margin-top", "1em")
      , ("margin-left", "auto")
      , ("margin-right", "auto")
      , ("width", "640px")
      , ("height", "480px")
      , ("border", "4px solid black")
      ]
    
    visible_layer_style = Attributes.style
      [ ("width", "0px")
      , ("height", "0px")
      , ("overflow", "visible")
      ]
    
    invisible_layer_style = Attributes.style
      [ ("width", "0px")
      , ("height", "0px")
      , ("overflow", "hidden")
      ]
    
    bg_style = Attributes.style
      [ ("width", "640px")
      , ("height", "480px")
      , ("background-image", "url('imgs/grey.png')")
      , ("background-size", "28px 28px")
      , ("background-position", toString (-camera_pixels.x) ++ "px " ++ toString (-camera_pixels.y) ++ "px")
      ]
    
    instructions_style = Attributes.style
      [ ("margin-top", "1em")
      , ("margin-left", "auto")
      , ("margin-right", "auto")
      , ("width", "640px")
      ]
  in
    Html.div [top_style]
      [ Html.img [title_style, Attributes.src "imgs/title.png"] []
      , Html.div [container_style]
          (Html.div [visible_layer_style] [Html.div [bg_style] []] :: everything)
      , Html.div [instructions_style] [model.instructions]
      , Html.div [instructions_style] [Html.text model.debug]
      ]
