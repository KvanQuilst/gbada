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
