-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                                Types (Spec)                               --
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

--  Basic generic types for use throughout GBADA

with Ada.Text_IO;

package Types is

   type Bit    is mod 2**1;
   type UInt4  is mod 2**4;
   type UInt8  is mod 2**8;
   type UInt16 is mod 2**16;

   type UInt16_Split (Split : Boolean := True) is record
     case Split is
        when True =>
           Upper : UInt8;
           Lower : UInt8;
        when False =>
           Full  : UInt16;
     end case;
   end record
      with Unchecked_Union;

   for UInt16_Split use record
      Upper at 1 range 0 .. 7;
      Lower at 0 range 0 .. 7;
      Full  at 0 range 0 .. 15;
   end record;

   subtype Address is UInt16;

   package UInt8_IO  is new Ada.Text_IO.Modular_IO (UInt8);
   package UInt16_IO is new Ada.Text_IO.Modular_IO (UInt16);

end Types;
