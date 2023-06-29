-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                            Display . Read (Body)                          --
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
with Memory;

package body Display.Read is

   function Get_Tile (Addr : Address) return Tile is
      Data_1, Data_2 : UInt8;
      T : Tile := (others => (others => Colors (0)));
   begin
      for I in Tile_Width'Range loop
         Data_1 := Memory.Read_Byte (Addr + UInt16 (I));
         Data_2 := Memory.Read_Byte (Addr + UInt16 (I) + 1);

         for J in Tile_Width'Range loop

            --  Colors (0 | 1 | 2 | 3) relates to the Gameboy's 4 color options
            --  Data_1 and Data_2 are masked and added to obtain the set color
            T (I, J) := Colors (UInt2 (Data_1 and 2 ** (7 - J)) +
                                UInt2 ((Data_2 and 2 ** (7 - J)) * 2));
         end loop;
      end loop;

      return T;
   end Get_Tile;

end Display.Read;
