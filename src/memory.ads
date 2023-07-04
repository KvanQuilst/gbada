-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                               Memory (Spec)                               --
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

--  Memory module which handles the storage, reading, and writing of memory

with Ada.Sequential_IO;

with Types; use Types;

package Memory is

   procedure Read_Cart (File_Name : String);

   procedure Write_Byte (Item   : UInt8;
                         Addr   : Address;
                         Is_CPU : Boolean := True);
   function Read_Byte (Addr   : Address;
                       Is_CPU : Boolean := True) return UInt8;

   procedure Write_Double (Item   : UInt16;
                           Addr   : Address;
                           Is_CPU : Boolean := True);
   function Read_Double (Addr   : Address;
                         Is_CPU : Boolean := True) return UInt16;

   ----------------------------
   -- Memory Lock Procedures --
   ----------------------------

   procedure Lock_VRAM with Inline;
   procedure Unlock_VRAM with Inline;
   procedure Lock_OAM with Inline;
   procedure Unlock_OAM with Inline;

   -----------------------
   -- Notable Addresses --
   -----------------------

   A_Tiles_0 : constant Address := 16#8000#;
   A_Tiles_1 : constant Address := 16#8800#;
   A_Tiles_2 : constant Address := 16#9000#;
   A_Tile_Map_0 : constant Address := 16#9800#;
   A_Tile_Map_1 : constant Address := 16#9C00#;

   A_LCDC : constant Address := 16#FF40#;
   A_STAT : constant Address := 16#FF41#;
   A_SCY  : constant Address := 16#FF42#;
   A_SCX  : constant Address := 16#FF43#;
   A_LY   : constant Address := 16#FF44#;
   A_LYC  : constant Address := 16#FF45#;
   A_BGP  : constant Address := 16#FF47#;
   A_OBP0 : constant Address := 16#FF48#;
   A_OBP1 : constant Address := 16#FF49#;
   A_WY   : constant Address := 16#FF4A#;
   A_WX   : constant Address := 16#FF4B#;

private

   Mem_Map : array (Address'Range) of UInt8;

   VRAM_Start : constant Address := 16#8000#;
   VRAM_End   : constant Address := 16#9FFF#;
   OAM_Start  : constant Address := 16#FE00#;
   OAM_End    : constant Address := 16#FE9F#;

   ------------------
   -- Memory Locks --
   ------------------

   VRAM_Lock : Boolean := False;
   OAM_Lock  : Boolean := False;

   package UInt8_Seq_IO is new Ada.Sequential_IO (UInt8); use UInt8_Seq_IO;

   Invalid_Memory_Reference_Exception : exception;

end Memory;
