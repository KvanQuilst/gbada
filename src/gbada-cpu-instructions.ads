with Gbada.Types; use Gbada.Types;
with Gbada.Memory; use Gbada.Memory;

package Gbada.CPU.Instructions is

  -- Read the next byte at PC
  procedure Read_Instruction;

--private
  
  Invalid_Instruction_Call_Exception : exception;

end Gbada.CPU.Instructions;
