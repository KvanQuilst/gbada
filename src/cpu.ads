-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                                 CPU (Spec)                                --
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

--  CPU module for GBADA. Handles the emulation of the hardware and its state.

with Ada.Real_Time; use Ada.Real_Time;
with Types; use Types;

package CPU is

   type Register    is (A, F, B, C, D, E, H, L, AF, BC, DE, HL, SP, PC);
   type Register_8  is (A, F, B, C, D, E, H, L);
   type Register_16 is (AF, BC, DE, HL, SP, PC);

   procedure Print_Register  (R : Register);
   procedure Print_Registers (Header : Boolean := False);

   procedure Execute;

private

   -----------
   -- Clock --
   -----------

   Instr_Start : Time;
   Instr_Delay : Time_Span;
   M_Cycle : constant Time_Span := Nanoseconds (4 * 238);

   Interrupt_Enable : Boolean := True;
   Halted           : Boolean := False;

   ---------------
   -- Registers --
   ---------------

   type Register_8_T (Flag : Boolean := True) is
      record
         case Flag is
            when True =>
               Val : UInt8;
            when False =>
               Zero        : Bit;
               Subtraction : Bit;
               Half_Carry  : Bit;
               Carry       : Bit;
               Reserved    : UInt4;
         end case;
      end record
         with Unchecked_Union;

   for Register_8_T use
      record
         Val         at 0 range 0 .. 7;

         Zero        at 0 range 7 .. 7;
         Subtraction at 0 range 6 .. 6;
         Half_Carry  at 0 range 5 .. 5;
         Carry       at 0 range 4 .. 4;
         Reserved    at 0 range 0 .. 3;
      end record;

   type Register_16_T (Split : Boolean := True) is
      record
         case Split is
            when True =>
               Upper : UInt8;
               Lower : UInt8;
            when False =>
               Val : UInt16;
         end case;
      end record
         with Unchecked_Union;

   for Register_16_T use
      record
         Val   at 0 range 0 .. 15;

         Upper at 1 range 0 .. 7;
         Lower at 0 range 0 .. 7;
      end record;

   type Registers_T (Paired : Boolean := True) is
      record
         SP : Address;
         PC : Address;
         case Paired is
            when True =>
               AF : Register_16_T;
               BC : Register_16_T;
               DE : Register_16_T;
               HL : Register_16_T;
            when False =>
               A : Register_8_T;
               F : Register_8_T;
               B : Register_8_T;
               C : Register_8_T;
               D : Register_8_T;
               E : Register_8_T;
               H : Register_8_T;
               L : Register_8_T;
         end case;
      end record
         with Unchecked_Union;

   for Registers_T use
      record
         AF at 16#0# range 0 .. 15;
         A  at 16#1# range 0 .. 7;
         F  at 16#0# range 0 .. 7;
         BC at 16#2# range 0 .. 15;
         B  at 16#3# range 0 .. 7;
         C  at 16#2# range 0 .. 7;
         DE at 16#4# range 0 .. 15;
         D  at 16#5# range 0 .. 7;
         E  at 16#4# range 0 .. 7;
         HL at 16#6# range 0 .. 15;
         H  at 16#7# range 0 .. 7;
         L  at 16#6# range 0 .. 7;
         SP at 16#8# range 0 .. 15;
         PC at 16#A# range 0 .. 15;
      end record;

   Registers : Registers_T := (
      Paired => True,
      AF => (False, 16#01B0#),
      BC => (False, 16#0013#),
      DE => (False, 16#00D8#),
      HL => (False, 16#014D#),
      SP => 16#FFFF#,
      PC => 16#0150#
   );

   procedure Write_Register (R : Register_8; Item : UInt8);
   function Read_Register (R : Register_8) return UInt8;

   procedure Write_Register (R : Register_16; Item : UInt16);
   function Read_Register (R : Register_16) return UInt16;

end CPU;
