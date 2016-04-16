module Main where

import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (Html)
import Html.Attributes as Attributes
import Keyboard
import Time exposing (..)
import Window


-- MODEL

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
  | vx = toFloat keys.x
  , vy = toFloat keys.y
  }


-- VIEW

view : (Int, Int) -> Model -> Html
view _ player =
  let
    (w',h') = (640, 480)
    (w,h) = (toFloat w', toFloat h')

    verb =
      if player.y > 0 then
          "jump"

      else if player.vx /= 0 then
          "walk"

      else
          "stand"

    src =
      "/imgs/red.png"

    playerImage =
      image 28 28 src

    everything =
      collage w' h'
        [ playerImage
            |> toForm
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
      , ("background-position", toString (-player.x) ++ "px " ++ toString player.y ++ "px")
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
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
