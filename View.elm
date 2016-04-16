module View where

import Html exposing (Html)
import Html.Attributes as Attributes
import Graphics.Element exposing (..)

import Vec exposing (..)


type alias PositionedElement = (Coord, Element)

type alias Model =
  { camera : Coord
  , elements : List PositionedElement
  , debug : String
  }

view : Model -> Html
view model =
  let
    camera_pixels = pixels model.camera `minus` { x = 320-2*28, y = 240-2*28 }
    
    deltaPosition : Coord -> Element -> Element
    deltaPosition coord =
      let
        relative_pixels = pixels coord `minus` camera_pixels
        position = topLeftAt
          (absolute relative_pixels.x)
          (absolute relative_pixels.y)
      in
        container 640 480 position
    
    everything =
      layers (List.map (uncurry deltaPosition) model.elements)
    
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
    
    layer_style = Attributes.style
      [ ("width", "0px")
      , ("height", "0px")
      , ("overflow", "visible")
      ]
    
    bg_style = Attributes.style
      [ ("width", "640px")
      , ("height", "480px")
      , ("background-image", "url('/imgs/grey.png')")
      , ("background-size", "28px 28px")
      , ("background-position", toString (-camera_pixels.x) ++ "px " ++ toString (-camera_pixels.y) ++ "px")
      ]
  in
    Html.div [top_style]
      [ Html.img [title_style, Attributes.src "/imgs/title.png"] []
      , Html.div [container_style]
          [ Html.div [layer_style] [Html.div [bg_style] []]
          , Html.div [layer_style] [Html.fromElement everything]
          ]
      , Html.div [layer_style] [Html.text model.debug]
      ]
