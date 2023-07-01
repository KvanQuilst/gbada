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

with Memory;

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
      Add (Str => Pixel,
           Line => Scr_Y_0 + Y_Pos,
           Column => Scr_X_0 + X_Pos * Pixel'Length);
   end Draw_Pixel;
   pragma Inline (Draw_Pixel);

   --  Init_Display
   --  Initialize ncurses with the appropriate settings. "Blank" the screen.
   --
   --  Returns: Boolean indicating success or failure
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
      Scr_X_0 := (Columns - Scr_Width * Pixel'Length) / 2;

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
      for I in Scr_Line_Position'Range loop
         for J in Scr_Column_Position'Range loop
            Draw_Pixel (Color_Pair (I mod 4 + 1), I, J);
         end loop;
      end loop;

      Render;
      Refresh;

      return True;
   end Init_Display;

   --  End_Display
   --  Gracefully clean-up and shutdown ncurses
   procedure End_Display is
   begin
      End_Windows;
   end End_Display;

   --  Convert_Tile
   --  Convert a tile's worth of memory into usable Tile data for rendering
   --
   --  Returns: Tile data at given address
   function Convert_Tile (Addr : Address) return Tile is
      Data_1, Data_2 : UInt8;
      A : Address := Addr;
      T : Tile := (others => (others => 1));
      Int1, Int2 : UInt4;
   begin
      for I in 0 .. 7 loop
         Data_1 := Memory.Read_Byte (A + UInt16 (I * 2));
         Data_2 := Memory.Read_Byte (A + UInt16 (I * 2) + 1);

         for J in 0 .. 7 loop
            Int1 := (if (Data_1 and 2 ** (7 - J)) > 0 then 1 else 0);
            Int2 := (if (Data_2 and 2 ** (7 - J)) > 0 then 1 else 0) * 2;
            T (I, J) := Color_Pair (Int1 + Int2 + 1);
         end loop;

         A := A + 2;
      end loop;

      return T;
   end Convert_Tile;

   --  Tile_Map
   --  Retrieve the appropriate tile data given a tile index referenced
   --  against the LCDC register
   --
   --  Tile_Idx: The tile index to get the tile data for
   --  Returns: Tile data
   function Tile_Map (Tile_Idx : UInt8) return Tile is
      use Memory;

      LCDC : constant UInt8 := Read_Byte (A_LCDC);
      Data_Addr : constant Address :=
         (if (LCDC and BG_Window_Data) > 0 then A_Tiles_0 else A_Tiles_2);
      T : Tile;
   begin
      if Data_Addr = A_Tiles_2 then
         T := Convert_Tile (Data_Addr +
                            UInt16 (Tile_Idx and 16#7F#) * Tile_Size -
                           (if (Tile_Idx and 16#80#) > 0
                            then Tile_Size else 0) * 128);
      else
         T := Convert_Tile (Data_Addr + UInt16 (Tile_Idx) * Tile_Size);
      end if;

      return T;
   end Tile_Map;

   --  Render_Tile
   --  Given a tile index, renders the tile at the appropriate location
   --
   --  Tile_Idx: The index of the tile to be rendered in the tile data
   --  Y_Pos, X_Pos: The position on screen to render the tile
   procedure Render_Tile (Tile_Idx : UInt8;
                          Y_Pos    : Scr_Line_Position;
                          X_Pos    : Scr_Column_Position) is
      T : constant Tile := Tile_Map (Tile_Idx);
   begin
      for I in Tile_Pixel'Range loop
         for J in Tile_Pixel'Range loop
            Draw_Pixel (T (I, J),
                        Y_Pos + Line_Position (I),
                        X_Pos + Column_Position (J));
         end loop;
      end loop;
   end Render_Tile;

   procedure Render is
      Addr : Address := 16#0104#;
      T : Tile;
   begin
      for I in 0 .. 19 loop
         for J in 0 .. 17 loop
            T := Convert_Tile (Addr);

            for K in 0 .. 7 loop
               for L in 0 .. 7 loop
                  Draw_Pixel (T (K, L),
                              Scr_Line_Position (J * Tile_Width + K),
                              Scr_Column_Position (I * Tile_Width + L));
               end loop;
            end loop;

            Addr := Addr + 16;
         end loop;
      end loop;
   end Render;

end Display;
