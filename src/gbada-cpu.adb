with Ada.Text_IO; use Ada.Text_IO;
with Gbada.CPU.Instructions; use Gbada.CPU.Instructions;

package body Gbada.CPU is

  procedure Start_Clock is
  begin
    Print_Registers (True);
    loop
      Instr_Start := Clock;

      Read_Instruction;
      Print_Registers (False);

      delay until Instr_Start + Instr_Delay;
    end loop;
  end Start_Clock;

  procedure Print_Register (R : Register_Name) is
  begin
    case R is
      when A => Put ("Register A: "); UInt8_IO.Put (Reg.A.Val, UInt8'Width, 16);
      when B => Put ("Register B: "); UInt8_IO.Put (Reg.B.Val, UInt8'Width, 16);
      when C => Put ("Register C: "); UInt8_IO.Put (Reg.C.Val, UInt8'Width, 16);
      when D => Put ("Register D: "); UInt8_IO.Put (Reg.D.Val, UInt8'Width, 16);
      when E => Put ("Register E: "); UInt8_IO.Put (Reg.E.Val, UInt8'Width, 16);
      when F => Put ("Register F: "); UInt8_IO.Put (Reg.F.Val, UInt8'Width, 16);
      when H => Put ("Register H: "); UInt8_IO.Put (Reg.H.Val, UInt8'Width, 16);
      when L => Put ("Register L: "); UInt8_IO.Put (Reg.L.Val, UInt8'Width, 16);
      when AF => Put ("Register AF: "); UInt16_IO.Put (Reg.AF.Val, UInt16'Width, 16);
      when BC => Put ("Register BC: "); UInt16_IO.Put (Reg.BC.Val, UInt16'Width, 16);
      when DE => Put ("Register DE: "); UInt16_IO.Put (Reg.DE.Val, UInt16'Width, 16);
      when HL => Put ("Register HL: "); UInt16_IO.Put (Reg.HL.Val, UInt16'Width, 16);
      when SP => Put ("Register SP: "); UInt16_IO.Put (Reg.SP.Val, UInt16'Width, 16);
      when PC => Put ("Register PC: "); UInt16_IO.Put (Reg.PC, UInt16'Width, 16);
    end case;
    Put_Line ("");
  end Print_Register;

  procedure Print_Registers (Header : Boolean) is
  begin
    if Header then
      Put_Line (" PC                : " &
                " Instr         |" &
                " A       " &
                " BC        " &
                " DE        " &
                " HL       | " &
                "Flags");
      Put_Line ("                                    |         " &
                "                                |            ");
    end if;
    UInt16_IO.Put (Reg.PC, 9, 16); Put (" = ");
    UInt8_IO.Put (ReadByte (Reg.PC), 6, 16); Put (" : ");
    Put (Instruction_String & " | ");
    UInt8_IO.Put (Reg.A.Val, 6, 16); Put ("  ");
    UInt16_IO.Put (Reg.BC.Val, 9, 16); Put ("  ");
    UInt16_IO.Put (Reg.DE.Val, 9, 16); Put ("  ");
    UInt16_IO.Put (Reg.HL.Val, 9, 16); Put (" |  ");
    Put_Line ((if Reg.F.Zero_Flag = 2#1# then "Z" else "-") &
              (if Reg.F.Subtraction_Flag = 2#1# then "N" else "-") &
              (if Reg.F.Carry_Flag = 2#1# then "C" else "-") &
              (if Reg.F.HalfCarry_Flag = 2#1# then "H" else "-"));
  end Print_Registers;

end Gbada.CPU;
