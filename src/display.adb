with SDL.Video.Windows.Makers;
with SDL.Video.Renderers.Makers;

package body Display is

   function Setup_Display return Boolean is
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
