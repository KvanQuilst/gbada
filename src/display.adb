-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                               Display (Body)                              --
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

with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;

package body Display is

   function Setup_Display return Boolean is
      use SDL.C;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create (
         Win      => Window,
         Title    => "GBADA - Gameboy Emulator",
         Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
         Size     => SDL.Positive_Sizes'(Width * Scale, Height * Scale),
         Flags    => 0
      );

      SDL.Video.Renderers.Makers.Create (Renderer, Window.Get_Surface);

      return True;
   end Setup_Display;

   procedure Finalize_Display is
   begin
      Window.Finalize;
      SDL.Finalise;
   end Finalize_Display;

end Display;
