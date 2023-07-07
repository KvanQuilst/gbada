-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                                 PPU (Spec)                                --
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
with Types; use Types;

package PPU is

   function Initialize return Boolean;
   procedure Finish;

   procedure Execute;

private

   ---------------
   -- PPU State --
   ---------------

   --  Dot_Count
   --  A track of how much time has passed since the start of rendering the
   --  current frame. Used to switch PPU state at the correct timings.
   Dot_Count : Integer := 0;
   Dot_Delay : Integer := 0;

   -- LCDC Masks --
   LCD_Enable       : constant UInt8 := 2#10000000#;
   Window_Map       : constant UInt8 := 2#01000000#;
   Window_Enable    : constant UInt8 := 2#00100000#;
   BG_Window_Data   : constant UInt8 := 2#00010000#;
   BG_Tile_Map      : constant UInt8 := 2#00001000#;
   OBJ_Size         : constant UInt8 := 2#00000100#;
   OBJ_Enable       : constant UInt8 := 2#00000010#;
   BG_Window_Enable : constant UInt8 := 2#00000001#;

   -- STAT Masks --
   STAT_LYC_LY_Int  : constant UInt8 := 2#01000000#;
   STAT_OAM_Int     : constant UInt8 := 2#00100000#;
   STAT_VBLANK_Int  : constant UInt8 := 2#00010000#;
   STAT_HBLANK_Int  : constant UInt8 := 2#00001000#;
   STAT_LCY_LY_Flag : constant UInt8 := 2#00000100#;
   STAT_Mode_Flag   : constant UInt8 := 2#00000011#;

end PPU;
