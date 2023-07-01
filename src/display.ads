-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                              Display (Spec)                               --
--                                                                           --
--                      Copyright (C) 2023 Dylan Eskew                       --
--                                                                           --
-- This file is a part of GBADA.                                             --
--                                                                           --
-- GBADA is free software: you can redistribute it and/or modify it under    --
-- the terms of the GNU General Public License as published by the Free      --
-- Software Foundation, either version 3 of the License, or (at your option) --
-- any later version.                                                        --
--                                                                           --
-- GBADA is distributed in the hope that it will be useful, but WITHOUT ANY  --
-- WARRANTY; wihtout even the implied warranty of MERCHANTABILITY or FITNESS --
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more     --
-- details.                                                                  --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with GBADA. If not, see <https://www.gnu.org/licenses/>.                  --
-------------------------------------------------------------------------------

--  Display module for GBADA utilizing the ncurses library for a terminal-based
--  screen

with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Types; use Types;

package Display is

   function Init_Display return Boolean;
   procedure End_Display;
   procedure Render;

private

   ------------
   -- Screen --
   ------------

   Scr_Height : constant Line_Position   := 144;
   Scr_Width  : constant Column_Position := 160;

   subtype Scr_Line_Position is Line_Position range 0 .. Scr_Height - 1;
   subtype Scr_Column_Position is Column_Position range 0 .. Scr_Width - 1;

   --  ncurses location for (0, 0) of the Gameboy screen
   Scr_Y_0     : Line_Position;
   Scr_X_0     : Column_Position;

   subtype Map_Y_Pos is Line_Position range 0 .. 255;
   subtype Map_X_Pos is Line_Position range 0 .. 255;

   --  Screen (0, 0) location on the 256x256 tile map
   Scr_Y_Pos : Map_Y_Pos;
   Scr_X_Pos : Map_X_Pos;

   Pixel : constant String := "  ";

   -- LCDC Masks --
   LCD_Enable       : constant UInt8 := 2#10000000#;
   Window_Map       : constant UInt8 := 2#01000000#;
   Window_Enable    : constant UInt8 := 2#00100000#;
   BG_Window_Data   : constant UInt8 := 2#00010000#;
   BG_Tile_Map      : constant UInt8 := 2#00001000#;
   OBJ_Size         : constant UInt8 := 2#00000100#;
   OBJ_Enable       : constant UInt8 := 2#00000010#;
   BG_Window_Enable : constant UInt8 := 2#00000001#;

   -----------
   -- Tiles --
   -----------

   Tile_Size : constant UInt16 := 16;
   Tile_Width : constant := 8;

   subtype Tile_Pixel is Integer range 0 .. Tile_Width - 1;
   type Tile is array (Tile_Pixel, Tile_Pixel) of Color_Pair;

   Tile_Map_0 : array (Map_Y_Pos, Map_X_Pos) of Color_Pair;
   Tile_Map_1 : array (Map_Y_Pos, Map_X_Pos) of Color_Pair;

end Display;
