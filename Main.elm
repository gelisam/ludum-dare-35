module Main where

import AnimationFrame
import Array exposing (Array)
import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (Html)
import Html.Attributes as Attributes
import Keyboard
import Maybe
import Signal
import String
import Time exposing (..)
import Window


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
    w = Array.length element_level
    h = Array.length (Maybe.withDefault Array.empty (Array.get 0 element_level))
    grid = flow down << Array.toList << Array.map (flow right << Array.toList)
  in
    grid element_level


type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }


type alias Keys = { x:Int, y:Int }


init : Model
init =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) player =
  player
    |> walk keys
    |> physics dt


physics : Float -> Model -> Model
physics dt player =
  { player
  | x = player.x + dt * player.vx
  , y = player.y + dt * player.vy
  }


walk : Keys -> Model -> Model
walk keys player =
  { player
  | vx = toFloat keys.x / 20
  , vy = toFloat keys.y / 20
  }


-- VIEW

view : (Int, Int) -> Model -> Html
view _ player =
  let
    (w',h') = (640, 480)
    (w,h) = (toFloat w', toFloat h')

    src =
      "/imgs/red.png"

    playerImage =
      image 28 28 src

    camera_dx = 320-14
    camera_dy = 240-14
    bg_dx = -(round (player.x * 28)) + camera_dx
    bg_dy = round (player.y * 28) + camera_dy
    
    everything =
      layers
        [ level_element
            |> container 640 480 (topLeftAt (absolute bg_dx) (absolute bg_dy))
        , playerImage
            |> container 640 480 (topLeftAt (absolute camera_dx) (absolute camera_dy))
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
      , ("background-position", toString bg_dx ++ "px " ++ toString bg_dy ++ "px")
      ]
  in
    Html.div [top_style]
      [ Html.img [title_style, Attributes.src "/imgs/title.png"] []
      , Html.div [container_style]
          [ Html.div [layer_style] [Html.div [bg_style] []]
          , Html.div [layer_style] [Html.fromElement everything]
          ]
      ]


-- SIGNALS

main : Signal Html
main =
  Signal.map2 view Window.dimensions (Signal.foldp update init input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) AnimationFrame.frame
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
