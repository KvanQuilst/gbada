with Ada.Text_IO;   use Ada.Text_IO;
with GBADA.Types;   use GBADA.Types;
with CPU; use CPU;
with CPU.Instructions; use CPU.Instructions;
with Memory; use Memory;

procedure Main is
  Val : UInt8;
begin

  Put_Line ("Registers");
  Put_Line ("Register A: " & Reg.A.Val'Image);
  Put_Line ("Register B: " & Reg.B.Val'Image);
  Put_Line ("Register C: " & Reg.C.Val'Image);
  Put_Line ("Register D: " & Reg.D.Val'Image);
  Put_Line ("Register E: " & Reg.E.Val'Image);
  Put_Line ("Register F: " & Reg.F.Val'Image);
  Put_Line ("Register H: " & Reg.H.Val'Image);
  Put_Line ("Register L: " & Reg.L.Val'Image);
  Put_Line ("Register SP: " & Reg_SP'Image);
  Put_Line ("Register PC: " & Reg_PC'Image);
  Put_Line ("");
  Put_Line ("Flags");
  Put_Line ("Zero: " & Reg.F.Zero_Flag'Image);
  Put_Line ("");

end Main;
