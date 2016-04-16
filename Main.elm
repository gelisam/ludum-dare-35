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
  , dir : Direction
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


mario : Model
mario =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  }


-- UPDATE

update : (Float, Keys) -> Model -> Model
update (dt, keys) mario =
  mario
    |> gravity dt
    |> jump keys
    |> walk keys
    |> physics dt


jump : Keys -> Model -> Model
jump keys mario =
  if keys.y > 0 && mario.vy == 0 then
      { mario | vy = 6.0 }

  else
      mario


gravity : Float -> Model -> Model
gravity dt mario =
  { mario |
      vy = if mario.y > 0 then mario.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y = max 0 (mario.y + dt * mario.vy)
  }


walk : Keys -> Model -> Model
walk keys mario =
  { mario |
      vx = toFloat keys.x,
      dir =
        if keys.x < 0 then
            Left

        else if keys.x > 0 then
            Right

        else
            mario.dir
  }


-- VIEW

view : (Int, Int) -> Model -> Html
view _ mario =
  let
    (w',h') = (640, 480)
    (w,h) = (toFloat w', toFloat h')

    verb =
      if mario.y > 0 then
          "jump"

      else if mario.vx /= 0 then
          "walk"

      else
          "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "/imgs/red.png"

    marioImage =
      image 28 28 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY)
    
    everything =
      collage w' h'
        [ rect w 50
            |> filled (rgb 74 167 43)
            |> move (0, 24 - h/2)
        , marioImage
            |> toForm
            |> move position
        ]
    
    container_style = Attributes.style
      [ ("image-rendering", "pixelated")
      , ("margin-top", "1em")
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
      , ("background-position", "0px 0px")
      ]
  in
    Html.div [container_style]
      [ Html.div [layer_style] [Html.div [bg_style] []]
      , Html.div [layer_style] [Html.fromElement everything]
      ]


-- SIGNALS

main : Signal Html
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
