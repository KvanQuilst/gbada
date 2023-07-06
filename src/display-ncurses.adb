--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                              Display (Body)                               --
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
with Ada.Text_IO;

package body Display is

   --  Draw_Pixel
   --  Draw a pixel at the given location on the Screen
   --
   --  Color: The color of the pixel
   --  Y_Pos : Relative to Y_Scr_0, the y-coord
   --  X_Pos : Relative to X_Scr_0, the x-coord
   procedure Draw_Pixel (Color : Color_Pair;
                         Y_Pos : Scr_Line_Position;
                         X_Pos : Scr_Column_Position) is
   begin
      Set_Color (Pair => Color);
      Add (Str    => Pixel,
           Line   => Scr_Y_0 + Y_Pos,
           Column => Scr_X_0 + X_Pos * Pixel'Length);
   end Draw_Pixel;

   --  Init_Display
   --  Initialize ncureses with the appropriate settings for GBAda.
   --
   --  Returns: Boolean indicating success or failure
   function Init_Display return Boolean is
      Cursor_Vis : Cursor_Visibility := Invisible;
   begin
      Init_Windows;

      -- Terminal Size Check --
      if Lines < Scr_Height or else Columns < Scr_Width then
         End_Display;
         Ada.Text_IO.Put_Line ("Terminal size is too small!");
         Ada.Text_IO.Put_Line ("Req:" & Scr_Height'Image & " lines," &
                               Scr_Width'Image & " columns");
         return False;
      end if;

      -- Set Screen Origin --
      Scr_Y_0 := (Lines - Scr_Height) / 2;
      Scr_X_0 := (Columns - Scr_Width * Pixel'Length) / 2;

      -- ncurses Settings --
      Set_Cursor_Visibility (Cursor_Vis);
      Set_KeyPad_Mode (SwitchOn => True);
      Set_Raw_Mode    (SwitchOn => True);
      Set_Echo_Mode   (SwitchOn => False);
      Set_NL_Mode     (SwitchOn => False);

      -- Colors --
      Start_Color;
      Set_Palette (DMG);

      -- "Blank" the Screen --
      for I in Scr_Line_Position'Range loop
         for J in Scr_Column_Position'Range loop
            Draw_Pixel (Color_Pair (I mod 4 + 1), I, J);
         end loop;
      end loop;
      Refresh;

      return True;
   end Init_Display;

   procedure End_Display is
   begin
      null;
   end End_Display;

   --  Set_Palette
   --  Set the rendered color palette to the indicated palette
   --  Disclaimer: May have unintended artifact issues during emulation
   --
   --  P: The color to use
   procedure Set_Palette (P : Palette) is
   begin
      for I in 1 .. 4 loop
         Init_Color (Color_Number (I), P (I, Red), P (I, Green), P (I, Blue));
         Init_Pair  (Color_Pair (I), Color_Number (I), Color_Number (I));
      end loop;
   end Set_Palette;

end Display;
