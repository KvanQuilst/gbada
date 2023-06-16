with CPU;
with Memory;
procedure Gbada is
begin
   Memory.Read_Cart ("games/tetris.gb");

   CPU.Execute;
end Gbada;
