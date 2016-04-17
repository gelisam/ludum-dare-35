module Powerups where

import Dict exposing (Dict)
import Graphics.Element as Element exposing (Element)

import Block exposing (Block)
import Grid exposing (Grid)
import Powerup exposing (Powerup(..))
import Vec exposing (Coord, Vec)
import View exposing (PositionedElement)


type alias Powerups = Dict (Int,Int) Powerup

toKey : Coord -> (Int,Int)
toKey coord = (coord.x, coord.y)

fromKey : (Int,Int) -> Coord
fromKey (x,y) = { x = x, y = y }

fromList : List (Coord, Powerup) -> Powerups
fromList = Dict.fromList << List.map (\(coord, powerup) -> (toKey coord, powerup))

get : Coord -> Powerups -> Maybe Powerup
get coord = Dict.get (toKey coord)

remove : Coord -> Powerups -> Powerups
remove coord = Dict.remove (toKey coord)


pickup : Coord -> Grid (Maybe Block) -> Powerups -> (List Powerup, Powerups)
pickup level_coord player_grid powerups =
  let
    player_coords : List Coord
    player_coords = Grid.keys player_grid
    
    pickup1 : Coord -> (List Powerup, Powerups) -> (List Powerup, Powerups)
    pickup1 player_coord (picked, remaining) =
      let
        coord = level_coord `Vec.plus` player_coord
      in
        case (Grid.get player_coord player_grid, get coord remaining) of
          (Just (Just _), Just (FixedShape shape orientation dp)) ->
            (FixedShape shape orientation (coord `Vec.plus` dp) :: picked, remaining)
          (Just (Just _), Just powerup) ->
            (powerup :: picked, remove coord remaining)
          _ ->
            (picked, remaining)
  in
    List.foldr pickup1 ([], powerups) player_coords

view : Powerups -> List PositionedElement
view = Dict.toList >> List.map (\(key, powerup) -> (fromKey key, Powerup.view powerup))
