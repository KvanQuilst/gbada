-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                                 CPU (Body)                                --
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
-- with GBADA. If not, see <https://www.gnu.org/licenses/>.                  --
-------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;

with CPU.Logic; use CPU.Logic;
with Hex_IO; use Hex_IO;

package body CPU is

   procedure Execute is
   begin
      CPU.Print_Registers (True);
      for I in 1 .. 100 loop
         Execute_Instruction;
         CPU.Print_Registers (False);
      end loop;
   end Execute;

   procedure Write_Register (R : Register_8; Item : UInt8) is
   begin
      case R is
         when A => Registers.A.Val := Item;
         when F => Registers.F.Val := Item;
         when B => Registers.B.Val := Item;
         when C => Registers.C.Val := Item;
         when D => Registers.D.Val := Item;
         when E => Registers.E.Val := Item;
         when H => Registers.H.Val := Item;
         when L => Registers.L.Val := Item;
      end case;
   end Write_Register;

   function Read_Register (R : Register_8) return UInt8 is
   begin
      case R is
         when A => return Registers.A.Val;
         when F => return Registers.F.Val;
         when B => return Registers.B.Val;
         when C => return Registers.C.Val;
         when D => return Registers.D.Val;
         when E => return Registers.E.Val;
         when H => return Registers.H.Val;
         when L => return Registers.L.Val;
      end case;
   end Read_Register;

   procedure Write_Register (R : Register_16; Item : UInt16) is
   begin
      case R is
         when AF => Registers.AF.Val := Item;
         when BC => Registers.BC.Val := Item;
         when DE => Registers.DE.Val := Item;
         when HL => Registers.HL.Val := Item;
         when SP => Registers.SP     := Item;
         when PC => Registers.PC     := Item;
      end case;
   end Write_Register;

   function Read_Register (R : Register_16) return UInt16 is
   begin
      case R is
         when AF => return Registers.AF.Val;
         when BC => return Registers.BC.Val;
         when DE => return Registers.DE.Val;
         when HL => return Registers.HL.Val;
         when SP => return Registers.SP;
         when PC => return Registers.PC;
      end case;
   end Read_Register;

   -------------------
   -- CPU State I/O --
   -------------------

   procedure Print_Register (R : Register) is
   begin
      case R is
         when A => Put ("Register A: "); Put (Registers.A.Val);
         when F =>
            Put ("Flags: ");
            Put ((if Registers.F.Zero = 1        then "Z" else "-") &
                 (if Registers.F.Subtraction = 1 then "N" else "-") &
                 (if Registers.F.Half_Carry = 1  then "H" else "-") &
                 (if Registers.F.Carry = 1       then "C" else "-"));
         when B => Put ("Register B: "); Put (Registers.B.Val);
         when C => Put ("Register C: "); Put (Registers.C.Val);
         when D => Put ("Register D: "); Put (Registers.D.Val);
         when E => Put ("Register E: "); Put (Registers.E.Val);
         when H => Put ("Register H: "); Put (Registers.H.Val);
         when L => Put ("Register L: "); Put (Registers.L.Val);
         when AF => Put ("Register AF: "); Put (Registers.AF.Val);
         when BC => Put ("Register BC: "); Put (Registers.BC.Val);
         when DE => Put ("Register DE: "); Put (Registers.DE.Val);
         when HL => Put ("Register HL: "); Put (Registers.HL.Val);
         when SP => Put ("Register SP: "); Put (Registers.SP);
         when PC => Put ("Register PC: "); Put (Registers.PC);
      end case;
      New_Line;
   end Print_Register;

   --
   --  Layout
   --  PC: Instr | SP | AF | BC | DE | HL | F
   --
   procedure Print_Registers (Header : Boolean := False) is
   begin
      if Header then
         Put_Line ("  PC    " &
                   "  Instr              " &
                   "| SP     " &
                   "| AF     " &
                   "| BC     " &
                   "| DE     " &
                   "| HL     " &
                   "| Flags");
      end if;

      Put (Registers.PC, 8); Put ("  ");
      Put (Instruction_Image (Registers.PC) & " | ");
      Put (Registers.SP); Put (" | ");
      Put (Registers.AF.Val); Put (" | ");
      Put (Registers.BC.Val); Put (" | ");
      Put (Registers.DE.Val); Put (" | ");
      Put (Registers.HL.Val); Put (" | ");
      Put ((if Registers.F.Zero = 1        then "Z" else "-") &
           (if Registers.F.Subtraction = 1 then "N" else "-") &
           (if Registers.F.Half_Carry = 1  then "H" else "-") &
           (if Registers.F.Carry = 1       then "C" else "-"));
      New_Line;
   end Print_Registers;

end CPU;
