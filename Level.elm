module Level where

import Graphics.Element exposing (..)

import Block exposing (Block(..))
import Grid exposing (Grid)
import Powerup exposing (Powerup(..))
import Powerups exposing (Powerups)
import Shape exposing (Shape(..), Orientation(..))
import Vec exposing (Coord)
import View exposing (PositionedElement)


-- ' ' for floor
-- '#' for wall
-- 'B', 'C', 'G', 'O', 'R', 'V', and 'Y' for colored blocks
-- '*' for powerup
-- '.' for player position
-- '!' for goal
char_grid : Grid Char
char_grid = Grid.init
  [ "##########################################################################"
  , "###########################################.     #########################"
  , "###########################################      #########################"
  , "###########################################      #########################"
  , "#########    ##############################      #########################"
  , "#####*    *    ############################      #########################"
  , "##### #         #########                        #########################"
  , "##### ###        #######     ##############  BB  #########################"
  , "##### ########    #####  *  ###############  BOO #########################"
  , "##### ########     ###     ################G B O #########################"
  , "##### #########     #  *  #################GGYYO #########################"
  , "####  ##########         ##################OGYYV #############*###########"
  , "#### * ##########    *  ###################ORRVV ############# ###########"
  , "####   ###########     ####################OORRV!#############  ##########"
  , "#####    ########     #######################################   ##########"
  , "######   ########       #####################################   ##########"
  , "####     #######           ################################     ##########"
  , "####   ########    ##          ############################   ############"
  , "##                ####              #######################   ############"
  , "#### #########   #######                  ###############     #####*######"
  , "#### ########    ########                                     ###        #"
  , "#############   #############                                 ###      * #"
  , "############    #################  * ##########   ###############        #"
  , "############   #################### ###########   ###                    #"
  , "###########    #################### ###########   ##*     #######        #"
  , "###########     ################### ###########   ### #    ###############"
  , "###########      ###          ##### ###########   ### #     ##############"
  , "###########                           #########   ### # #    #############"
  , "############                           ########   ### # #     ############"
  , "##############                          #######   *## # #*#   ############"
  , "################     ########            ######   ### # #     ############"
  , "##################  * ########*          ######   ### # #    #############"
  , "#######################################* ######   ### #     ##############"
  , "#######################################  ######   ### #    ###############"
  , "######################################  *######   ###     ################"
  , "######################################   ######   ###    #################"
  , "######################################  #######   ##    ##################"
  , "######################################                 ###################"
  , "###################################### *              ####################"
  , "######################################               #####################"
  , "##########################################################################"
  , "##########################################################################"
  ]

powerups : List Powerup
powerups =
  [ FixedShape I R1 { x = 0, y = 1 }
  , FixedShape T R1 { x = 0, y = 0 }
  , FixedShape T R2 { x = 0, y = 0 }
  , FixedShape O R0 { x = 1, y = -1 }
  , FixedShape J R3 { x = 0, y = 2 }
  , ShapeShift
  , FixedShape I R0 { x = 1, y = 0 }
  , Rotate
  , FixedShape I R1 { x = 0, y = 0 }
  , FixedShape I R1 { x = 0, y = -1 }
  , FixedShape I R0 { x = 2, y = 0 }
  , FixedShape S R0 { x = -1, y = 0 }
  , FixedShape Z R1 { x = 0, y = -1 }
  , FixedShape T R2 { x = 0, y = 0 }
  , FixedShape T R0 { x = 0, y = -1 }
  , Jump
  , FixedShape O R0 { x = 0, y = 0 }
  , FixedShape L R0 { x = 0, y = 0 }
  ]

player_start : Coord
player_start =
  let
    is_start_coord : Coord -> Bool
    is_start_coord coord = Grid.get coord char_grid == Just '.'
    
    coords = Grid.keys char_grid
    start_coords = List.filter is_start_coord coords
  in
    case start_coords of
      [coord] -> coord
      [] -> Debug.crash "level has no start position"
      _ -> Debug.crash "level has more than one start position"

goal_coord : Coord
goal_coord =
  let
    is_goal_coord : Coord -> Bool
    is_goal_coord coord = Grid.get coord char_grid == Just '!'
    
    coords = Grid.keys char_grid
    goal_coords = List.filter is_goal_coord coords
  in
    case goal_coords of
      [coord] -> coord
      [] -> Debug.crash "level has no goal position"
      _ -> Debug.crash "level has more than one goal position"

powerups_start : Powerups
powerups_start =
  let
    is_powerup_coord : Coord -> Bool
    is_powerup_coord coord = Grid.get coord char_grid == Just '*'
    
    coords = Grid.keys char_grid
    powerup_coords = List.filter is_powerup_coord coords
  in
    Powerups.fromList <| List.map2 (,) powerup_coords powerups

block_grid : Grid Block
block_grid =
  Grid.map (Maybe.withDefault White << Block.parse) char_grid


obstacle : Block -> Bool
obstacle block = block /= White

collides : Coord -> Grid (Maybe Block) -> Bool
collides level_coord player_grid =
  let
    collidesWith : Maybe Block -> Maybe Block -> Bool
    collidesWith player_block level_block = case (player_block, level_block) of
      (Nothing, _) ->
        -- outside the player, no collision
        False
      (_, Nothing) ->
        -- outside the level, grey everywhere
        obstacle Grey
      (_, Just block) ->
        obstacle block
    
    collidesAt : Coord -> Bool
    collidesAt player_coord =
      let
        player_block = Maybe.withDefault Nothing (Grid.get player_coord player_grid)
        level_block = Grid.get (player_coord `Vec.plus` level_coord) block_grid
      in
        collidesWith player_block level_block

    player_coords : List Coord
    player_coords = Grid.keys player_grid
  in
    List.any collidesAt player_coords


-- VIEW

element_grid : Grid Element
element_grid =
    Grid.map Block.viewOpaque block_grid

element : Element
element =
  Grid.view element_grid

view : PositionedElement
view =
  { coord = Vec.init
  , element = element
  , visible = True
  }
