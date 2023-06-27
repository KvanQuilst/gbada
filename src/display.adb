with SDL;
with SDL.Video.Windows.Makers;
with SDL.Video.Renderers;

package body Display is

   function Setup_Display return Boolean is
      Window   : SDL.Video.Windows.Window;
      Renderer : SDL.Video.Renderers.Renderer;
   begin
      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         return False;
      end if;

      SDL.Video.Windows.Makers.Create (
         Win      => Window,
         Title    => "GBADA - Gameboy Emulator",
         Position => SDL.Natural_Coordinates'(X => 10, Y => 10),
         Size     => SDL.Positive_Sizes'(160, 144),
         Flags    => 0
      );

      return True;
   end Setup_Display;

end Display;
