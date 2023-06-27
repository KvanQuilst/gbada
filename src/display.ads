with Interfaces.C; use Interfaces.C;
with SDL;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Video.Palettes;

package Display is

   function Setup_Display return Boolean;
   procedure Finalize_Display;

private

   Window : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;

   Colors : constant array (0 .. 3) of SDL.Video.Palettes.Colour :=
      ((16#0F#, 16#38#, 16#0F#, 16#FF#), --  Darkest Green
       (16#30#, 16#62#, 16#30#, 16#FF#), --  Dark Green
       (16#8B#, 16#AC#, 16#0F#, 16#FF#), --  Light Green
       (16#9B#, 16#BC#, 16#0F#, 16#FF#)  --  Lightest Green
      );

   Width  : constant int := 160;
   Height : constant int := 144;
   Scale  : int := 1;

end Display;
