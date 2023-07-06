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
package Display is

   function Init_Display return Boolean;
   procedure End_Display;

   type Palette is private;
   procedure Set_Palette (P : Palette);

private
   use Terminal_Interface.Curses;

   ------------
   -- Screen --
   ------------

   Scr_Height : Line_Position   := 144;
   Scr_Width  : Column_Position := 160;

   subtype Scr_Line_Position   is Line_Position   range 0 .. Scr_Height - 1;
   subtype Scr_Column_Position is Column_Position range 0 .. Scr_Width - 1;

   --  ncurses location for (0, 0) of the Gameboy screen
   Scr_Y_0 : Line_Position;
   Scr_X_0 : Column_Position;

   Pixel : constant String := "  ";

   --------------
   -- Palettes --
   --------------

   type Color_Value is (Red, Green, Blue);
   type Palette is array (Integer range 1 .. 4, Color_Value) of RGB_Value;

   DMG : constant Palette :=
      ((16#0F#, 16#38#, 16#0F#),
       (16#30#, 16#62#, 16#30#),
       (16#8B#, 16#AC#, 16#0F#),
       (16#9B#, 16#BC#, 16#0F#));

end Display;
