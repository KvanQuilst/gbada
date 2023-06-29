-------------------------------------------------------------------------------
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

   function Init_Display return Boolean is
      Cursor_Vis : Cursor_Visibility := Invisible;
   begin
      Init_Windows;

      if Lines < Scr_Height or else Columns < Scr_Width then
         End_Display;
         Ada.Text_IO.Put_Line ("Terminal size is too small!");
         Ada.Text_IO.Put_Line ("Req:" & Scr_Height'Image & " lines," &
                               Scr_Width'Image & " columns");
         return False;
      end if;

      Scr_Y_0 := (Lines - Scr_Height) / 2;
      Scr_X_0 := (Columns - Scr_Width) / 2;

      Set_Cursor_Visibility (Cursor_Vis);
      Set_KeyPad_Mode (SwitchOn => True);
      Set_Raw_Mode    (SwitchOn => True);
      Set_Echo_Mode   (SwitchOn => False);
      Set_NL_Mode     (SwitchOn => False);

      Refresh_Without_Update;

      Start_Color;
      Init_Color (1, 16#0F#, 16#38#, 16#0F#);
      Init_Color (2, 16#30#, 16#62#, 16#30#);
      Init_Color (3, 16#8B#, 16#AC#, 16#0F#);
      Init_Color (4, 16#9B#, 16#BC#, 16#0F#);

      Init_Pair (1, 1, 1);
      Init_Pair (2, 2, 2);
      Init_Pair (3, 3, 3);
      Init_Pair (4, 4, 4);

      -- "Blank" Screen --
      for I in 0 .. Scr_Height - 1 loop
         Set_Color (Pair => Color_Pair (I mod 4 + 1));

         for J in 0 .. Scr_Width - 1 loop
            Add (Ch => ' ',
                 Line => Scr_Y_0 + I,
                 Column => Scr_X_0 + J);
         end loop;
      end loop;

      Refresh;

      return True;
   end Init_Display;

   procedure End_Display is
   begin
      End_Windows;
   end End_Display;

end Display;
