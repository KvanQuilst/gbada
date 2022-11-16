with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

package CPU.Instructions is

  -- Read the next byte at PC
  procedure Read_Instruction;

--private
  
  Invalid_Instruction_Call_Exception : exception;

end CPU.Instructions;
