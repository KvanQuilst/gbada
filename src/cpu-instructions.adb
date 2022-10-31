with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

package body CPU.Instructions is
  
  procedure Read_Instruction is
  begin
    case MemMap (Reg_PC) is

        -- 8-Bit LD --
      when 16#06# => LD_Instr (Reg.B, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when 16#0E# => LD_Instr (Reg.C, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when 16#16# => LD_Instr (Reg.D, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when 16#1E# => LD_Instr (Reg.E, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when 16#26# => LD_Instr (Reg.H, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when 16#2E# => LD_Instr (Reg.L, MemMap (Reg_PC + 1)); Reg_PC := Reg_PC + 1;
      when others => raise Invalid_Instruction_Call_Exception
        with "Instruction not implemented!";
    end case;
    Reg_PC := Reg_PC + 1;
  end Read_Instruction;


  procedure LD_Instr (Dest : Address; Val : UInt8) is
  begin
    WriteByte (Dest, Val);
  end LD_Instr;

  procedure LD_Instr (Dest : in out Register; Val : UInt8) is
  begin
    Dest.Val := Val;
  end LD_Instr;

  procedure LD_Instr (Dest : in out Paired_Register; Val : UInt8) is
  begin
    WriteByte (Dest.Val, Val);
  end LD_Instr;

end CPU.Instructions;
