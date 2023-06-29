-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                            Display . Read (Spec)                          --
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

--  Reads the tile and sprite data from memory and converts it to a SDL
--  rendering format

package Display.Read is

   --  Gameboy Tile: 8x8 pixels, converted to SDL RGBA pixel format
   subtype Tile_Width is Integer range 0 .. 7;
   type Tile is array (Tile_Width, Tile_Width) of SDL.Video.Palettes.Colour;

   --  Get Gameboy Tile data from memory location and convert it to SDL RGBA
   --  pixel format
   function Get_Tile (Addr : Address) return Tile;

end Display.Read;
