module Main where

import AnimationFrame
import Array exposing (Array)
import Color exposing (..)
import Debug
import Graphics.Element exposing (..)
import Html exposing (Html)
import Html.Attributes as Attributes
import Keyboard
import Maybe
import Signal
import String
import Time exposing (..)

import Keys exposing (Keys)
import Player
import Vec exposing (Vec)


-- MODEL

type Color
  = White  -- ' '
  | Grey   -- '#'
  | Blue   -- 'b' for upgrade, 'B' for block
  | Cyan   -- 'c' for upgrade, 'C' for block
  | Green  -- 'g' for upgrade, 'G' for block
  | Orange -- 'o' for upgrade, 'O' for block
  | Red    -- 'r' for upgrade, 'R' for block
  | Violet -- 'v' for upgrade, 'V' for block
  | Yellow -- 'y' for upgrade, 'Y' for block

-- '.' for player position, '!' for goal
int_level : List String
int_level =
  [ "########"
  , "#      #"
  , "#  .   #"
  , "#      #"
  , "#      #"
  , "#  BB  #"
  , "#  BOO #"
  , "#G B O #"
  , "#GGYYO #"
  , "#OGYYV #"
  , "#ORRVV #"
  , "#OORRV!#"
  , "########"
  ]

color_level : Array (Array Color)
color_level =
  let
    color : Char -> Color
    color char = case char of
      ' ' -> White
      'B' -> Blue
      'C' -> Cyan
      'G' -> Green
      'O' -> Orange
      'R' -> Red
      'V' -> Violet
      'Y' -> Yellow
      _   -> Grey
    
    row : String -> Array Color
    row = Array.fromList << List.map color << String.toList
  in
    Array.fromList (List.map row int_level)

element_level : Array (Array Element)
element_level =
  let
    block : Color -> Element
    block color =
      image 28 28 ("/imgs/" ++ String.toLower (toString color) ++ ".png")
  in
    Array.map (Array.map block) color_level

level_element : Element
level_element =
  let
    grid = flow down << Array.toList << Array.map (flow right << Array.toList)
  in
    grid element_level


type alias Model =
  { player : Player.Model
  }


init : Model
init =
  { player = Player.init
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
  { model
  | player = Player.update keys model.player
  }


-- VIEW

view : Model -> Html
view model =
  let
    level_dp = Vec.init
    player_dp = Vec.scale 28 model.player.p
    camera_dp = { x = player_dp.x - (320-14)
                , y = player_dp.y - (240-14)
                }
    bg_dp = camera_dp
    
    debug = (model.player.p.x, model.player.p.y)
    
    deltaPosition : Vec Int -> Element -> Element
    deltaPosition dp =
      let
        position = topLeftAt
          (absolute (dp.x - camera_dp.x))
          (absolute (dp.y - camera_dp.y))
      in
        container 640 480 position
    
    everything =
      layers
        [ level_element
            |> deltaPosition level_dp
        , Player.view model.player
            |> deltaPosition player_dp
        ]
    
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
      , ("background-position", toString (-bg_dp.x) ++ "px " ++ toString (-bg_dp.y) ++ "px")
      ]
  in
    Html.div [top_style]
      [ Html.img [title_style, Attributes.src "/imgs/title.png"] []
      , Html.div [container_style]
          [ Html.div [layer_style] [Html.div [bg_style] []]
          , Html.div [layer_style] [Html.fromElement everything]
          ]
      , Html.div [layer_style] [Html.text (toString debug)]
      ]


-- SIGNALS

main : Signal Html
main =
  Signal.map view (Signal.foldp update init input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keys.signal)
