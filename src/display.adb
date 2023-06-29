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

with SDL.Video.Pixel_Formats;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

--  with Display.Read;

package body Display is

   function Setup_Display return Boolean is
      use SDL.C;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create (
         Win    => Window,
         Title  => "GBADA - Gameboy Emulator",
         X      => SDL.Video.Windows.Centered_Window_Position,
         Y      => SDL.Video.Windows.Centered_Window_Position,
         Width  => Width * Scale,
         Height => Height * Scale,
         Flags  => 0
      );

      SDL.Video.Renderers.Makers.Create (Renderer, Window);
      Renderer.Set_Draw_Colour (Colors (0));
      Renderer.Clear;
      Renderer.Present;

      SDL.Video.Textures.Makers.Create (
         Tex => Texture,
         Renderer => Renderer,
         Format => SDL.Video.Pixel_Formats.Pixel_Format_RGBA_8888,
         Kind => SDL.Video.Textures.Static,
         Size => (Width * Scale, Height * Scale)
      );

      return True;
   end Setup_Display;

   procedure Finalize_Display is
   begin
      Renderer.Finalize;
      Window.Finalize;
      SDL.Finalise;
   end Finalize_Display;

end Display;
