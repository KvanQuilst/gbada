-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                                 PPU (Body)                                --
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
with Display;
with Memory;

package body PPU is

   --  Initialize
   --  Initialize the PPU start state and display
   --
   --  Returns: Boolean whether intialization was successful
   function Initialize return Boolean is
   begin
      return Display.Initialize;
   end Initialize;

   --  Finish
   --  Clean up display and PPU resources
   procedure Finish is
   begin
      Display.Finish;
   end Finish;

   --  Execute
   --  PPU state machine logic
   procedure Execute is
      use Memory;
      STAT : constant UInt8 := Read_Byte (A_STAT, Is_CPU => False);
      Mode : constant UInt2 := UInt2 (STAT and STAT_Mode_Flag);
   begin
      case Mode is
         when 0 => -- HBlank (85 - 208 Dots) --
            if Dot_Count = 208 - Dot_Delay then
               Dot_Count := -1;

               --  Update LY and STAT
               Write_Byte (Read_Byte (A_LY) + 1, A_LY, Is_CPU => False);
               Write_Byte ((STAT and 16#FC#) +
                           (if Read_Byte (A_LY) = 144 then 2 else 1),
                           A_LY, Is_CPU => False);
            end if;

         when 1 => -- VBlank (4560 Dots) --
            if Dot_Count = 456 then
               Dot_Count := -1;

               --  Update LY and STAT
               Write_Byte (Read_Byte (A_LY) + 1, A_LY, Is_CPU => False);
               Write_Byte ((STAT and 16#FC#) +
                           (if Read_Byte (A_LY) = 0 then 2 else 1),
                           A_STAT, Is_CPU => False);
               Lock_OAM;
            end if;

         when 2 => -- OAM Overlap (80 Dots) --
            if Dot_Count = 80 then
               Dot_Count := -1;

               --  Change STAT mode to 3
               Write_Byte ((STAT and 16#FC#) + 3, A_STAT, Is_CPU => False);
               Lock_VRAM;
            end if;

         when 3 => -- OAM/VRAM Render (168 - 291 Dots) --
            if Dot_Count = 168 + Dot_Delay then
               Dot_Count := -1;

               --  Change STAT mode to 0
               Write_Byte ((STAT and 16#FC#) + 0, A_STAT, Is_CPU => False);
               Unlock_VRAM;
            end if;
      end case;

      Dot_Count := Dot_Count + 1;
   end Execute;

end PPU;
