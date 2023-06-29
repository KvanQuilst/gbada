-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                               Memory (Body)                               --
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
package body Memory is

   procedure Read_Cart (File_Name : String) is
      F : File_Type;
      I : Address := 0;
      Val : UInt8;
   begin
      Open (F, In_File, File_Name);

      -- TODO: Write magic number and filetype check --

      while not End_Of_File (F) loop
         Read (F, Val);
         Write_Byte (Val, I);
         I := I + 1;
      end loop;

      Close (F);
   end Read_Cart;

   procedure Write_Byte (Item : UInt8; Addr : Address) is
   begin
      Mem_Map (Addr) := Item;
   end Write_Byte;

   function Read_Byte (Addr : Address) return UInt8 is
   begin
      return Mem_Map (Addr);
   end Read_Byte;

   procedure Write_Double (Item : UInt16; Addr : Address) is
      Double : constant UInt16_Split := (False, Item);
   begin
      Mem_Map (Addr + 1) := Double.Upper;
      Mem_Map (Addr) := Double.Lower;
   end Write_Double;

   function Read_Double (Addr : Address) return UInt16 is
      Double : constant UInt16_Split :=
         (True, Mem_Map (Addr + 1), Mem_Map (Addr));
   begin
      return Double.Full;
   end Read_Double;

end Memory;
