with Ada.Text_IO;   use Ada.Text_IO;
with Gbada.Types;   use Gbada.Types;
with Gbada.CPU; use Gbada.CPU;
with Gbada.CPU.Instructions; use Gbada.CPU.Instructions;
with Gbada.Memory; use Gbada.Memory;
with Gbada.CartridgeReader; use Gbada.CartridgeReader;
with Gbada.PPU;

procedure Main is
begin
  ReadRom ("games/tetris.gb");
  Put_Line ("");

  Start_Clock;

end Main;
