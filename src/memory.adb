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

   --  Check_Allowed
   --  For the CPU, check if the memory area is locked
   --
   --  Returns: True if allowed, False if not
   function Check_Allowed (Addr : Address) return Boolean is
   begin
      if Addr >= VRAM_Start and then Addr <= VRAM_End then
         return not VRAM_Lock;
      elsif Addr >= OAM_Start and then Addr <= OAM_End then
         return not OAM_Lock;
      end if;

      return True;
   end Check_Allowed;

   --  Read_Cart
   --  Read the cartridge file in from the provided File_Name
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

      --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, I'Image);

      Close (F);
   end Read_Cart;

   --  Write_Byte
   --  Write a byte of data to the specified memory address
   --
   --  Item: The UInt8 to write
   --  Addr: The address to write to
   --  Is_CPU: Is the CPU writing to memory?
   procedure Write_Byte (Item : UInt8;
                         Addr : Address;
                         Is_CPU : Boolean := True) is
   begin
      if Is_CPU and then not Check_Allowed (Addr) then
         return;
      end if;

      Mem_Map (Addr) := Item;
   end Write_Byte;

   --  Read_Byte
   --  Read a byte of data from the specified memory address
   --
   --  Addr: The address to read from
   --  Is_CPU: Is the CPU writing to memory?
   function Read_Byte (Addr : Address;
                       Is_CPU : Boolean := True) return UInt8 is
   begin
      if Is_CPU and then not Check_Allowed (Addr) then
         return 16#FF#;
      end if;

      return Mem_Map (Addr);
   end Read_Byte;

   --  Write_Double
   --  Write a double byte of data to the specified memory address
   --
   --  Item: The UInt16 to write
   --  Addr: The address to write to
   --  Is_CPU: Is the CPU writing to memory?
   procedure Write_Double (Item : UInt16;
                           Addr : Address;
                           Is_CPU : Boolean := True) is
      Double : constant UInt16_Split := (False, Item);
   begin
      if Is_CPU and then not Check_Allowed (Addr) then
         return;
      end if;

      Mem_Map (Addr + 1) := Double.Upper;
      Mem_Map (Addr) := Double.Lower;
   end Write_Double;

   --  Read_Double
   --  Read a double from the specified address
   --
   --  Addr: The address to read from
   --  Is_CPU: Is the CPU writing to memory?
   function Read_Double (Addr : Address;
                         Is_CPU : Boolean := True) return UInt16 is
      Double : constant UInt16_Split :=
         (True, Mem_Map (Addr + 1), Mem_Map (Addr));
   begin
      if Is_CPU and then not Check_Allowed (Addr) then
         return 16#FFFF#;
      end if;

      return Double.Full;
   end Read_Double;

   --  Lock_VRAM
   --  Prevent the CPU from reading or writing VRAM (0x8000 - 0x9FFF)
   procedure Lock_VRAM is
   begin
      VRAM_Lock := True;
   end Lock_VRAM;

   --  Unlock_VRAM
   --  Allow the CPU to read or write VRAM (0x8000 - 0x9FFF)
   procedure Unlock_VRAM is
   begin
      VRAM_Lock := False;
   end Unlock_VRAM;

   --  Lock_OAM
   --  Prevent the CPU from reading or writing OAM (0xFE00 - 0xFE9F)
   procedure Lock_OAM is
   begin
      OAM_Lock := True;
   end Lock_OAM;

   --  Unlock_OAM
   --  Allow the CPU to read or write OAM (0xFE00 - 0xFE9F)
   procedure Unlock_OAM is
   begin
      OAM_Lock := False;
   end Unlock_OAM;

end Memory;
