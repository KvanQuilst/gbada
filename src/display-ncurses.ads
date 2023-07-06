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

--  Display interface for GBADA utilizing ncurses for a terminal-based display

private with Terminal_Interface.Curses;

with Types; use Types;

package Display is
   -- Generic display utilities --

   function Initialize return Boolean;
   procedure Finish;

   subtype Color is Integer range 0 .. 4;
   type Color_Value is (Red, Green, Blue);
   type Palette is array (Color range 1 .. 4, Color_Value) of UInt8;

   procedure Set_Palette (P : Palette);

   Screen_Height : constant Integer := 144;
   Screen_Width  : constant Integer := 160;
   type Scan_Line is array (Integer range 1 .. Screen_Width) of Color;

   procedure Render_Scan_Line (SL : Scan_Line; Y_Pos : Natural);

   --------------
   -- Palettes --
   --------------

   DMG : constant Palette :=
      ((16#0F#, 16#38#, 16#0F#),
       (16#30#, 16#62#, 16#30#),
       (16#8B#, 16#AC#, 16#0F#),
       (16#9B#, 16#BC#, 16#0F#));

private
   -- ncurses specific implementation for display --
   use Terminal_Interface.Curses;

   ------------
   -- Screen --
   ------------

   Scr_Height : constant Line_Position   := Line_Position (Screen_Height);
   Scr_Width  : constant Column_Position := Column_Position (Screen_Width);

   subtype Scr_Line_Position   is Line_Position   range 0 .. Scr_Height - 1;
   subtype Scr_Column_Position is Column_Position range 0 .. Scr_Width - 1;

   --  ncurses location for (0, 0) of the Gameboy screen
   Scr_Y_0 : Line_Position;
   Scr_X_0 : Column_Position;

   Pixel : constant String := "  ";

end Display;
