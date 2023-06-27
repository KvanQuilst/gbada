-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                      Copyright (C) 2023 Dylan Eskew                       --
--                                                                           --
-- GBADA is free software: you can  redistribute it  and/or modify it  under --
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

--  Main execution file for GBADA. Central location for program logic

with CPU;
with Display;
with Memory;

procedure Gbada is
begin
   if not Display.Setup_Display then
      return;
   end if;
   Memory.Read_Cart ("games/tetris.gb");

   CPU.Execute;

   delay 10.0;

   Display.Finalize_Display;
end Gbada;
