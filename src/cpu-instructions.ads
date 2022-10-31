with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

package CPU.Instructions is

  -- Read the next byte at PC
  procedure Read_Instruction;

private

  procedure LD_Instr (Dest : Address; Val : UInt8);
  procedure LD_Instr (Dest : in out Register; Val : UInt8);
  procedure LD_Instr (Dest : in out Paired_Register; Val : UInt8);

  Invalid_Instruction_Call_Exception : exception;

end CPU.Instructions;
