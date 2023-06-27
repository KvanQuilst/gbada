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
-- GBADA. If not, see <https://www.gnu.org/licenses/>.                       --
-------------------------------------------------------------------------------

--  Memory module which handles the storage, reading, and writing of memory

with Ada.Sequential_IO;

with Types; use Types;

package Memory is

   -- Notable Addresses --
   A_Tiles_0 : constant Address := 16#8000#;
   A_Tiles_1 : constant Address := 16#8800#;
   A_Tiles_2 : constant Address := 16#9000#;

   procedure Read_Cart (File_Name : String);

   procedure Write_Byte (Item : UInt8; Addr : Address);
   function  Read_Byte  (Addr : Address) return UInt8;

   procedure Write_Double (Item : UInt16; Addr : Address);
   function Read_Double (Addr : Address) return UInt16;

private

   Mem_Map : array (Address'Range) of UInt8;

   package UInt8_Seq_IO is new Ada.Sequential_IO (UInt8); use UInt8_Seq_IO;

   Invalid_Memory_Reference_Exception : exception;

end Memory;
