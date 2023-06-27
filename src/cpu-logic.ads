-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                             CPU . Logic (Spec)                            --
--                                                                           --
--                      Copyright (C) 2023 Dylan Eskew                       --
--                                                                           --
-- This file is a part of GBADA.                                             --
--                                                                           --
-- GBADA is free software: you can redistribute it and/or modify it under    --
-- the terms of the GNU General Public License as published by the Free      --
-- Software Foundation, either version 3 of the License, or (at your option) --
-- any later version.                                                        --
--                                                                           --
-- GBADA is distributed in the hope that it will be useful, but WITHOUT ANY  --
-- WARRANTY; wihtout even the implied warranty of MERCHANTABILITY or FITNESS --
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more     --
-- details.                                                                  --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- GBADA. If not, see <https://www.gnu.org/licenses/>.                       --
-------------------------------------------------------------------------------

--  CPU package which handles all of the instruction logic and execution.

package CPU.Logic is

   procedure Execute_Instruction;

   function Instruction_Image (Addr : Address) return String;

private

   Inc_PC : Boolean;

   type Instruction is record
      Image    : String (1 .. 18);
      Bytes    : UInt4;
      M_Cycles : UInt4;
   end record;

   --
   --  Index is equivalent to opcode
   --
   --  V: 8-bit value (d8)
   --  Y: 16-bit value (d16, a16)
   --  M: 8-bit signed value (r8)
   --  K: 8-bit offset value from 0xFF00 (a8)
   --
   Instr_Info : constant array (UInt8'Range) of Instruction := (
      -- 0x00 --
      ("NOP               ", 1, 1),
      ("LD BC, Y          ", 2, 3),
      ("LD (BC), A        ", 1, 2),
      ("INC BC            ", 1, 2),
      ("INC B             ", 1, 1),
      ("DEC B             ", 1, 1),
      ("LD B, V           ", 2, 2),
      ("RLCA              ", 1, 1),
      ("LD (Y), SP        ", 3, 5),
      ("ADD HL, BC        ", 1, 2),
      ("LD A, (BC)        ", 1, 2),
      ("DEC BC            ", 1, 2),
      ("INC C             ", 1, 1),
      ("DEC C             ", 1, 1),
      ("LD C, V           ", 2, 2),
      ("RRCA              ", 1, 1),
      -- 0x10 --
      ("STOP V            ", 2, 1),
      ("LD DE, Y          ", 3, 3),
      ("LD (DE), A        ", 1, 2),
      ("INC DE            ", 1, 2),
      ("INC D             ", 1, 1),
      ("DEC D             ", 1, 1),
      ("LD D, V           ", 2, 2),
      ("RLA               ", 1, 1),
      ("JR M              ", 2, 3),
      ("ADD HL, DE        ", 1, 2),
      ("LD A, (DE)        ", 1, 2),
      ("DEC DE            ", 1, 2),
      ("INC E             ", 1, 1),
      ("DEC E             ", 1, 1),
      ("LD E, V           ", 2, 2),
      ("RRA               ", 1, 1),
      -- 0x20 --
      ("JR NZ, M          ", 2, 3),
      ("LD HL, Y          ", 3, 3),
      ("LDI (HL), A       ", 1, 2),
      ("INC HL            ", 1, 2),
      ("INC H             ", 1, 1),
      ("DEC H             ", 1, 1),
      ("LD H, V           ", 2, 2),
      ("DAA               ", 1, 1),
      ("JR Z, M           ", 2, 3),
      ("ADD HL, HL        ", 1, 2),
      ("LDI A, (HL)       ", 1, 2),
      ("DEC HL            ", 1, 2),
      ("INC L             ", 1, 1),
      ("DEC L             ", 1, 1),
      ("LD L, V           ", 2, 2),
      ("CPL               ", 1, 1),
      -- 0x30 --
      ("JR NC, M          ", 2, 3),
      ("LD SP, Y          ", 3, 3),
      ("LDD (HL), A       ", 1, 2),
      ("INC SP            ", 1, 2),
      ("INC (HL)          ", 1, 3),
      ("DEC (HL)          ", 1, 3),
      ("LD (HL), V        ", 2, 3),
      ("SCF               ", 1, 1),
      ("JR C, M           ", 2, 3),
      ("ADD HL, SP        ", 1, 2),
      ("LDD A, (HL)       ", 1, 2),
      ("DEC SP            ", 1, 2),
      ("INC A             ", 1, 1),
      ("DEC A             ", 1, 1),
      ("LD A, V           ", 2, 2),
      ("CCF               ", 1, 1),
      -- 0x40 --
      ("LD B, B           ", 1, 1),
      ("LD B, C           ", 1, 1),
      ("LD B, D           ", 1, 1),
      ("LD B, E           ", 1, 1),
      ("LD B, H           ", 1, 1),
      ("LD B, L           ", 1, 1),
      ("LD B, (HL)        ", 1, 2),
      ("LD B, A           ", 1, 1),
      ("LD C, B           ", 1, 1),
      ("LD C, C           ", 1, 1),
      ("LD C, D           ", 1, 1),
      ("LD C, E           ", 1, 1),
      ("LD C, H           ", 1, 1),
      ("LD C, L           ", 1, 1),
      ("LD C, (HL)        ", 1, 2),
      ("LD C, A           ", 1, 1),
      -- 0x50 --
      ("LD D, B           ", 1, 1),
      ("LD D, C           ", 1, 1),
      ("LD D, D           ", 1, 1),
      ("LD D, E           ", 1, 1),
      ("LD D, H           ", 1, 1),
      ("LD D, L           ", 1, 1),
      ("LD D, (HL)        ", 1, 2),
      ("LD D, A           ", 1, 1),
      ("LD E, B           ", 1, 1),
      ("LD E, C           ", 1, 1),
      ("LD E, D           ", 1, 1),
      ("LD E, E           ", 1, 1),
      ("LD E, H           ", 1, 1),
      ("LD E, L           ", 1, 1),
      ("LD E, (HL)        ", 1, 2),
      ("LD E, A           ", 1, 1),
      -- 0x60 --
      ("LD H, B           ", 1, 1),
      ("LD H, C           ", 1, 1),
      ("LD H, D           ", 1, 1),
      ("LD H, E           ", 1, 1),
      ("LD H, H           ", 1, 1),
      ("LD H, L           ", 1, 1),
      ("LD H, (HL)        ", 1, 2),
      ("LD H, A           ", 1, 1),
      ("LD L, B           ", 1, 1),
      ("LD L, C           ", 1, 1),
      ("LD L, D           ", 1, 1),
      ("LD L, E           ", 1, 1),
      ("LD L, H           ", 1, 1),
      ("LD L, L           ", 1, 1),
      ("LD L, (HL)        ", 1, 2),
      ("LD L, A           ", 1, 1),
      -- 0x70 --
      ("LD (HL), B        ", 1, 2),
      ("LD (HL), C        ", 1, 2),
      ("LD (HL), D        ", 1, 2),
      ("LD (HL), E        ", 1, 2),
      ("LD (HL), H        ", 1, 2),
      ("LD (HL), L        ", 1, 2),
      ("HALT              ", 1, 1),
      ("LD (HL), A        ", 1, 2),
      ("LD A, B           ", 1, 1),
      ("LD A, C           ", 1, 1),
      ("LD A, D           ", 1, 1),
      ("LD A, E           ", 1, 1),
      ("LD A, H           ", 1, 1),
      ("LD A, L           ", 1, 1),
      ("LD A, (HL)        ", 1, 2),
      ("LD A, A           ", 1, 1),
      -- 0x80 --
      ("ADD B             ", 1, 1),
      ("ADD C             ", 1, 1),
      ("ADD D             ", 1, 1),
      ("ADD E             ", 1, 1),
      ("ADD H             ", 1, 1),
      ("ADD L             ", 1, 1),
      ("ADD (HL)          ", 1, 2),
      ("ADD A             ", 1, 1),
      ("ADC B             ", 1, 1),
      ("ADC C             ", 1, 1),
      ("ADC D             ", 1, 1),
      ("ADC E             ", 1, 1),
      ("ADC H             ", 1, 1),
      ("ADC L             ", 1, 1),
      ("ADC (HL)          ", 1, 2),
      ("ADC A             ", 1, 1),
      -- 0x90 --
      ("SUB B             ", 1, 1),
      ("SUB C             ", 1, 1),
      ("SUB D             ", 1, 1),
      ("SUB E             ", 1, 1),
      ("SUB H             ", 1, 1),
      ("SUB L             ", 1, 1),
      ("SUB (HL)          ", 1, 2),
      ("SUB A             ", 1, 1),
      ("SBC B             ", 1, 1),
      ("SBC C             ", 1, 1),
      ("SBC D             ", 1, 1),
      ("SBC E             ", 1, 1),
      ("SBC H             ", 1, 1),
      ("SBC L             ", 1, 1),
      ("SBC (HL)          ", 1, 2),
      ("SBC A             ", 1, 1),
      -- 0xA0 --
      ("AND A, B          ", 1, 1),
      ("AND A, C          ", 1, 1),
      ("AND A, D          ", 1, 1),
      ("AND A, E          ", 1, 1),
      ("AND A, H          ", 1, 1),
      ("AND A, L          ", 1, 1),
      ("AND A, (HL)       ", 1, 2),
      ("AND A, A          ", 1, 1),
      ("XOR B             ", 1, 1),
      ("XOR C             ", 1, 1),
      ("XOR D             ", 1, 1),
      ("XOR E             ", 1, 1),
      ("XOR H             ", 1, 1),
      ("XOR L             ", 1, 1),
      ("XOR (HL)          ", 1, 2),
      ("XOR A             ", 1, 1),
      -- 0xB0 --
      ("OR B              ", 1, 1),
      ("OR C              ", 1, 1),
      ("OR D              ", 1, 1),
      ("OR E              ", 1, 1),
      ("OR H              ", 1, 1),
      ("OR L              ", 1, 1),
      ("OR (HL)           ", 1, 2),
      ("OR A              ", 1, 1),
      ("CP B              ", 1, 1),
      ("CP C              ", 1, 1),
      ("CP D              ", 1, 1),
      ("CP E              ", 1, 1),
      ("CP H              ", 1, 1),
      ("CP L              ", 1, 1),
      ("CP (HL)           ", 1, 2),
      ("CP A              ", 1, 1),
      -- 0xC0 --
      ("RET NZ            ", 1, 5),
      ("POP BC            ", 1, 3),
      ("JP NZ, Y          ", 3, 4),
      ("JP Y              ", 3, 4),
      ("CALL NZ, Y        ", 3, 6),
      ("PUSH BC           ", 1, 4),
      ("ADD A, V          ", 2, 2),
      ("RST 00H           ", 1, 4),
      ("RET Z             ", 1, 5),
      ("RET               ", 1, 4),
      ("JP Z, Y           ", 3, 4),
      ("PREFIX            ", 1, 1),
      ("CALL Z, Y         ", 3, 6),
      ("CALL Y            ", 3, 6),
      ("ADC A, M          ", 2, 2),
      ("RST 08H           ", 1, 4),
      -- 0xD0 --
      ("RET NC            ", 1, 5),
      ("POP DE            ", 1, 3),
      ("JP NC, Y          ", 3, 4),
      ("Reserved          ", 1, 1),
      ("CALL NC, Y        ", 3, 6),
      ("PUSH DE           ", 1, 4),
      ("SUB A, V          ", 2, 2),
      ("RST 10H           ", 1, 4),
      ("RET C             ", 1, 5),
      ("RETI              ", 1, 4),
      ("JP C, Y           ", 3, 4),
      ("Reserved          ", 1, 1),
      ("CALL C, Y         ", 3, 6),
      ("Reserved          ", 1, 1),
      ("SBC A, V          ", 2, 2),
      ("RST 18H           ", 1, 4),
      -- 0xE0 --
      ("LDH (K), A        ", 2, 3),
      ("POP HL            ", 1, 3),
      ("LD (C), A         ", 1, 2),
      ("Reserved          ", 1, 1),
      ("Reserved          ", 1, 1),
      ("PUSH HL           ", 1, 4),
      ("AND A, V          ", 2, 2),
      ("RST 20H           ", 1, 4),
      ("ADD SP, M         ", 2, 4),
      ("JP HL             ", 1, 1),
      ("LD (Y), A         ", 3, 4),
      ("Reserved          ", 1, 1),
      ("Reserved          ", 1, 1),
      ("Reserved          ", 1, 1),
      ("XOR A, V          ", 2, 2),
      ("RST 28H           ", 1, 4),
      -- 0xF0 --
      ("LDH A, (K)        ", 2, 3),
      ("POP AF            ", 1, 3),
      ("LD A, (C)         ", 1, 2),
      ("DI                ", 1, 1),
      ("Reserved          ", 1, 1),
      ("PUSH AF           ", 1, 4),
      ("OR A, V           ", 2, 2),
      ("RST 30H           ", 1, 4),
      ("LD HL, SP + M     ", 2, 3),
      ("LD SP, HL         ", 1, 2),
      ("LD A, (Y)         ", 3, 4),
      ("EI                ", 1, 1),
      ("Reserved          ", 1, 1),
      ("Reserved          ", 1, 1),
      ("CP A, V           ", 2, 2),
      ("RST 38H           ", 1, 4));

   Invalid_Instruction_Call_Exception : exception;

end CPU.Logic;
