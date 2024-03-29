-------------------------------------------------------------------------------
--                                                                           --
--                                   GBADA                                   --
--                                                                           --
--                             CPU . Logic (Body)                            --
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Hex_IO; use Hex_IO;
with Memory; use Memory;

package body CPU.Logic is

   -----------------------
   -- Instruction Calls --
   -----------------------

   procedure NOP is
   begin
      null;
   end NOP;

   --  For now, equivalent to HALT
   procedure STOP is begin
      Halted := True;
   end STOP;

   --  Set Carry Flag
   --  Clear subtraction and half-carry flags and set carry flag
   procedure SCF is
   begin
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry  := 0;
      Registers.F.Carry       := 1;
   end SCF;

   --  Complement Carry Flag
   --  Clear subtraction and half-carry flags and complement carry flag
   procedure CCF is
   begin
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry  := 0;
      Registers.F.Carry       := not Registers.F.Carry;
   end CCF;

   --  Stop CPU execution; exit halted state on button interrupt
   procedure HALT is
   begin
      Halted := True;
   end HALT;

   --  Disable interrupts
   procedure DI is
   begin
      Interrupt_Enable := False;
   end DI;

   --  Enable interrupts
   procedure EI is
   begin
      Interrupt_Enable := True;
   end EI;

   --
   --  LD
   --  Load data into source from destination
   --
   --  R_R: 8-bit Register, 8-bit Register
   --  R_I: 8-bit Register, 8-bit Immediate
   --  RR_II: 16-bit Register, 16-bit Immediate
   --  RRA_A, A_RRA: 16-bit Register
   --  II_A, A_II: none
   --  II_SP: none
   --  R_HLA, HLA_R: 8-bit Register
   --  HLA_I: 8-bit Immediate
   --  CA_A, A_CA: none
   --  HL_SP_SI: 8-bit Signed Immediate
   --  SP_HL: none
   --
   type LD_Op_Type is (R_R, R_I, RR_II, RRA_A, A_RRA, II_A, A_II, II_SP,
                       R_HLA, HLA_R, CA_A, A_CA, SP_HL, HLA_I, HL_SP_SI);
   type LD_Operands (O_Type : LD_Op_Type) is record
      case O_Type is
         when R_R =>
            R1 : Register_8;
            R2 : Register_8;
         when R_I | R_HLA | HLA_R =>
            R : Register_8;
         when RR_II =>
            RR : Register_16;
         when RRA_A | A_RRA =>
            RRA : Register_16;
         when II_A | A_II | II_SP | CA_A | A_CA | SP_HL | HL_SP_SI | HLA_I =>
            null;
      end case;
   end record;

   procedure LD (Ops : LD_Operands) is
   begin
      case Ops.O_Type is
         when R_R =>
            Write_Register (Ops.R1, Read_Register (Ops.R2));
         when R_I =>
            Write_Register (Ops.R, Read_Byte (Registers.PC + 1));
         when RR_II =>
            Write_Register (Ops.RR, Read_Double (Registers.PC + 1));
         when RRA_A =>
            Write_Byte (Registers.A.Val, Read_Register (Ops.RRA));
         when A_RRA =>
            Write_Register (A, Read_Byte (Read_Register (Ops.RRA)));
         when II_A =>
            Write_Byte (Registers.A.Val, Read_Double (Registers.PC + 1));
         when A_II =>
            Write_Register (A, Read_Byte (Read_Double (Registers.PC + 1)));
         when II_SP =>
            Write_Double (Registers.SP, Read_Double (Registers.PC + 1));
         when R_HLA =>
            Write_Register (Ops.R, Read_Byte (Registers.HL.Val));
         when HLA_R =>
            Write_Byte (Read_Register (Ops.R), Registers.HL.Val);
         when CA_A =>
            Write_Byte (Registers.A.Val, 16#FF00# + UInt16 (Registers.C.Val));
         when A_CA =>
            Write_Register
               (A, Read_Byte (16#FF00# + UInt16 (Registers.C.Val)));
         when HL_SP_SI =>
            declare
               Val8  : constant UInt8 := Read_Byte (Registers.PC + 1);
               Val16 : constant UInt16 := Registers.SP -
                  (if (Val8 and 16#80#) > 0 then 1 else 0) * 128 +
                  UInt16 (Val8 and 16#7F#);
               Orig  : constant UInt16 := Registers.HL.Val;
            begin
               Registers.HL.Val := Val16;

               Registers.F.Zero := 0;
               Registers.F.Subtraction := 0;
               Registers.F.Half_Carry :=
                  (if (Orig and 16#0FFF#) + (Val16 and 16#0FFF#) = 16#1000#
                   then 1 else 0);
               Registers.F.Carry := (if Registers.HL.Val < Orig then 1 else 0);
            end;
         when SP_HL =>
            Registers.SP := Registers.HL.Val;
         when HLA_I =>
            Write_Byte (Read_Byte (Registers.PC + 1), Registers.HL.Val);
      end case;
   end LD;

   type LDID_Op_Type is (HLA_A, A_HLA);

   --  Load using value at HL; increment value at HL
   procedure LDI (Op : LDID_Op_Type) is
   begin
      case Op is
         when HLA_A =>
            Write_Byte (Registers.A.Val, Registers.HL.Val);
         when A_HLA =>
            Write_Register (A, Read_Byte (Registers.HL.Val));
      end case;
      Registers.HL.Val := Registers.HL.Val + 1;
   end LDI;

   --  Load using value at HL; decrement value at HL
   procedure LDD (Op : LDID_Op_Type) is
   begin
      case Op is
         when HLA_A =>
            Write_Byte (Registers.A.Val, Registers.HL.Val);
         when A_HLA =>
            Write_Register (A, Read_Byte (Registers.HL.Val));
      end case;
      Registers.HL.Val := Registers.HL.Val - 1;
   end LDD;

   type LDH_Op_Type is (IA_A, A_IA);
   procedure LDH (Op : LDH_Op_Type) is
   begin
      case Op is
         when IA_A =>
            Write_Byte
               (Registers.A.Val, 16#FF00# +
                UInt16 (Read_Byte (Registers.PC + 1)));
         when A_IA =>
            Write_Register
               (A, Read_Byte
                  (16#FF00# + UInt16 (Read_Byte (Registers.PC + 1))));
      end case;
   end LDH;

   --  Push onto the stack at SP; decrement SP 2 bytes
   procedure PUSH (RR : Register_16) is
   begin
      Registers.SP := Registers.SP - 2;
      Write_Double (Read_Register (RR), Registers.SP);
   end PUSH;

   --  Pop off of the stack at SP into a register; increment SP 2 bytes
   procedure POP (RR : Register_16) is
   begin
      Write_Register (RR, Read_Double (Registers.SP));
      Registers.SP := Registers.SP + 2;
   end POP;

   --
   --  Arithmetic Operation Operands
   --
   --  R: 8-bit Register
   --  I: 8-bit Immediate
   --  RR: 16-bit Register
   --  HLA: none
   --  SP: 8-bit Signed Immediate
   --
   type Arith_Op_Type is (R, I, RR, HLA, SP);
   type Arith_Operands (O_Type : Arith_Op_Type) is record
      case O_Type is
         when R =>
            R : Register_8;
         when RR =>
            RR : Register_16;
         when I | HLA | SP =>
            null;
      end case;
   end record;

   --  Increment data at location by 1
   procedure INC (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (Ops.R, Read_Register (Ops.R) + 1);

            Registers.F.Zero := (if Read_Register (Ops.R) = 0 then 1 else 0);
            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Read_Register (Ops.R) and 16#0F#) = 0 then 1 else 0);

         when RR =>
            Write_Register (Ops.RR, Read_Register (Ops.RR) + 1);

         when HLA =>
            Write_Byte (Read_Byte (Registers.HL.Val) + 1, Registers.HL.Val);

            Registers.F.Zero :=
               (if Read_Byte (Registers.HL.Val) = 0 then 1 else 0);
            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Read_Byte (Registers.HL.Val) and 16#0F#) = 0
                then 1 else 0);

         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
   end INC;

   --  Decrement data at location by 1
   procedure DEC (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (Ops.R, Read_Register (Ops.R) - 1);

            Registers.F.Zero := (if Read_Register (Ops.R) = 0 then 1 else 0);
            Registers.F.Subtraction := 1;
            Registers.F.Half_Carry :=
               (if (Read_Register (Ops.R) and 16#0F#) = 16#0F# then 1 else 0);

         when RR =>
            Write_Register (Ops.RR, Read_Register (Ops.RR) - 1);

         when HLA =>
            Write_Byte (Read_Byte (Registers.HL.Val) + 1, Registers.HL.Val);

            Registers.F.Zero :=
               (if Read_Byte (Registers.HL.Val) = 0 then 1 else 0);
            Registers.F.Subtraction := 1;
            Registers.F.Half_Carry :=
               (if (Read_Byte (Registers.HL.Val) and 16#0F#) = 16#0F#
                then 1 else 0);

         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
   end DEC;

   procedure ADD (Ops : Arith_Operands) is
      Orig8  : constant UInt8 := Registers.A.Val;
      Orig16 : constant UInt16 := Registers.HL.Val;
      Val8   : UInt8;
      Val16  : UInt16;
   begin
      case Ops.O_Type is
         when R =>
            Val8 := Read_Register (Ops.R);
            Write_Register (A, Orig8 + Val8);

            Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Orig8 and 16#0F#) + (Val8 and 16#0F#) = 16#10#
                then 1 else 0);
            Registers.F.Carry := (if Registers.A.Val < Orig8 then 1 else 0);

         when I =>
            Val8 := Read_Byte (Registers.PC + 1);
            Write_Register (A, Orig8 + Val8);

            Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Orig8 and 16#0F#) + (Val8 and 16#0F#) = 16#10#
                then 1 else 0);
            Registers.F.Carry := (if Registers.A.Val < Orig8 then 1 else 0);

         when RR =>
            Val16 := Read_Register (Ops.RR);
            Write_Register (HL, Orig16 + Val16);

            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Orig16 and 16#0FFF#) + (Val16 and 16#0FFF#) = 16#1000#
                then 1 else 0);
            Registers.F.Carry := (if Registers.HL.Val < Orig16 then 1 else 0);

         when HLA =>
            Val8 := Read_Byte (Registers.HL.Val);
            Write_Register (A, Orig8 + Val8);

            Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
            Registers.F.Subtraction := 0;
            Registers.F.Half_Carry :=
               (if (Orig8 and 16#0F#) + (Val8 and 16#0F#) = 16#10#
                then 1 else 0);
            Registers.F.Carry := (if Registers.A.Val < Orig8 then 1 else 0);

         when SP =>
            Val8 := Read_Byte (Registers.PC + 1);
            Registers.SP := Registers.SP -
               (if (Val8 and 16#80#) > 0 then 1 else 0) * 128 +
               UInt16 (Val8 and 16#7F#);
      end case;
   end ADD;

   procedure ADC (Ops : Arith_Operands) is
      Orig : constant UInt8 := Registers.A.Val;
      Val  : UInt8;
   begin
      case Ops.O_Type is
         when R =>
            Val := Read_Register (Ops.R) + UInt8 (Registers.F.Carry);
         when I =>
            Val := Read_Byte (Registers.PC + 1) + UInt8 (Registers.F.Carry);
         when HLA =>
            Val := Read_Byte (Registers.HL.Val) + UInt8 (Registers.F.Carry);
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Write_Register (A, Orig + Val);

      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry :=
         (if (Orig and 16#0F#) + (Val and 16#0F#) = 16#10# then 1 else 00);
      Registers.F.Carry := (if Registers.A.Val < Orig then 1 else 0);
   end ADC;

   procedure SUB (Ops : Arith_Operands) is
      Orig : constant UInt8 := Registers.A.Val;
      Val  : UInt8;
   begin
      case Ops.O_Type is
         when R =>
            Val := Read_Register (Ops.R);
         when I =>
            Val := Read_Byte (Registers.PC + 1);
         when HLA =>
            Val := Read_Byte (Registers.HL.Val);
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Write_Register (A, Orig - Val);

      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 1;
      Registers.F.Half_Carry :=
         (if (Orig and 16#0F#) + (Val and 16#0F#) = 16#10# then 1 else 0);
      Registers.F.Carry := (if Orig < Val then 1 else 0);
   end SUB;

   procedure SBC (Ops : Arith_Operands) is
      Orig : constant UInt8 := Registers.A.Val;
      Val  : UInt8;
   begin

      case Ops.O_Type is
         when R =>
            Val := Read_Register (Ops.R) + UInt8 (Registers.F.Carry);
         when I =>
            Val := Read_Byte (Registers.PC + 1) + UInt8 (Registers.F.Carry);
         when HLA =>
            Val := Read_Byte (Registers.HL.Val) + UInt8 (Registers.F.Carry);
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;

      Write_Register (A, Orig - Val);

      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 1;
      Registers.F.Half_Carry :=
         (if (Orig and 16#0F#) + (Val and 16#0F#) = 16#10# then 1 else 0);
      Registers.F.Carry := (if Orig < Val then 1 else 0);
   end SBC;

   procedure A_AND (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val and Read_Register (Ops.R));
         when I =>
            Write_Register
               (A, Registers.A.Val and Read_Byte (Registers.PC + 1));
         when HLA =>
            Write_Register
               (A, Registers.A.Val and Read_Byte (Registers.HL.Val));
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry := 1;
      Registers.F.Carry := 0;
   end A_AND;

   procedure A_XOR (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val xor Read_Register (Ops.R));
         when I =>
            Write_Register
               (A, Registers.A.Val xor Read_Byte (Registers.PC + 1));
         when HLA =>
            Write_Register
               (A, Registers.A.Val xor Read_Byte (Registers.HL.Val));
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry := 0;
      Registers.F.Carry := 0;
   end A_XOR;

   procedure A_OR (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val xor Read_Register (Ops.R));
         when I =>
            Write_Register
               (A, Registers.A.Val or Read_Byte (Registers.PC + 1));
         when HLA =>
            Write_Register
               (A, Registers.A.Val or Read_Byte (Registers.HL.Val));
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Registers.F.Zero := (if Registers.A.Val = 0 then 1 else 0);
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry := 0;
      Registers.F.Carry := 0;
   end A_OR;

   procedure CP (Ops : Arith_Operands) is
      Val, Res  : UInt8;
   begin
      case Ops.O_Type is
         when R =>
            Val := Read_Register (Ops.R);
         when I =>
            Val := Read_Byte (Registers.PC + 1);
         when HLA =>
            Val := Read_Byte (Registers.HL.Val);
         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
      Res := Registers.A.Val - Val;

      Registers.F.Zero := (if Res = 0 then 1 else 0);
      Registers.F.Subtraction := 1;
      Registers.F.Half_Carry :=
         (if (Registers.A.Val and 16#0F#) + (Val and 16#0F#) = 16#10#
          then 1 else 0);
      Registers.F.Carry := (if Registers.A.Val < Val then 1 else 0);
   end CP;

   procedure CPL is
   begin
      Registers.A.Val := Registers.A.Val xor 16#FF#;
      Registers.F.Subtraction := 1;
      Registers.F.Half_Carry := 1;
   end CPL;

   --
   --  Jump
   --
   --  I: 8/16-bit Immediate
   --  F_I: Conditional Flag, 8/16-bit Immediate
   --  HL: none
   --
   type Jump_Flag is (Z, NZ, C, NC);
   type Jump_Op_Type is (I, F_I, HL);
   type Jump_Operands (O_Type : Jump_Op_Type) is record
      case O_Type is
         when I | HL =>
            null;
         when F_I =>
            F : Jump_Flag;
      end case;
   end record;

   procedure JP (Ops : Jump_Operands) is
      Val : constant UInt16 := Read_Double (Registers.PC + 1);
   begin
      case Ops.O_Type is
         when I =>
            Registers.PC := Val;
            Inc_PC := False;
         when F_I =>
            case Ops.F is
               when Z  =>
                  if Registers.F.Zero = 1 then
                     Registers.PC := Val;
                     Inc_PC := False;
                  end if;
               when NZ =>
                  if Registers.F.Zero = 0 then
                     Registers.PC := Val;
                     Inc_PC := False;
                  end if;
               when C  =>
                  if Registers.F.Carry = 1 then
                     Registers.PC := Val;
                     Inc_PC := False;
                  end if;
               when NC =>
                  if Registers.F.Carry = 0 then
                     Registers.PC := Val;
                     Inc_PC := False;
                  end if;
            end case;
         when HL =>
            Registers.PC := Registers.HL.Val;
            Inc_PC := False;
      end case;
   end JP;

   procedure JR (Ops : Jump_Operands) is
      Val : UInt8 := Read_Byte (Registers.PC + 1);
      Sub : constant Boolean := (Val and 16#80#) > 0;
   begin
      Val := Val and 16#7F#;
      case Ops.O_Type is
         when I =>
            Registers.PC :=
               Registers.PC - (if Sub then 1 else 0) * 128 + UInt16 (Val);
            Inc_PC := False;
         when F_I =>
            case Ops.F is
               when Z =>
                  if Registers.F.Zero = 1 then
                     Registers.PC := Registers.PC -
                        (if Sub then 1 else 0) * 128 + UInt16 (Val);
                     Inc_PC := False;
                  end if;
               when NZ =>
                  if Registers.F.Zero = 0 then
                     Registers.PC := Registers.PC -
                        (if Sub then 1 else 0) * 128 + UInt16 (Val);
                     Inc_PC := False;
                  end if;
               when C =>
                  if Registers.F.Carry = 1 then
                     Registers.PC := Registers.PC -
                        (if Sub then 1 else 0) * 128 + UInt16 (Val);
                     Inc_PC := False;
                  end if;
               when NC =>
                  if Registers.F.Carry = 0 then
                     Registers.PC := Registers.PC -
                        (if Sub then 1 else 0) * 128 + UInt16 (Val);
                     Inc_PC := False;
                  end if;
            end case;

         when others =>
            raise Invalid_Instruction_Call_Exception;
      end case;
   end JR;

   --
   --  RET
   --
   --  F: Jump_Flag
   --  None: none
   --
   type RET_Op_Type is (F, None);
   type RET_Operands (O_Type : RET_Op_Type) is record
      case O_Type is
         when F =>
            F : Jump_Flag;
         when None =>
            null;
      end case;
   end record;

   procedure RET (Ops : RET_Operands) is
   begin
      case Ops.O_Type is
         when F =>
            case Ops.F is
               when Z =>
                  if Registers.F.Zero = 1 then
                     Registers.PC := Read_Double (Registers.SP);
                     Registers.SP := Registers.SP + 2;
                     Inc_PC := False;
                  end if;
               when NZ =>
                  if Registers.F.Zero = 0 then
                     Registers.PC := Read_Double (Registers.SP);
                     Registers.SP := Registers.SP + 2;
                     Inc_PC := False;
                  end if;
               when C =>
                  if Registers.F.Carry = 1 then
                     Registers.PC := Read_Double (Registers.SP);
                     Registers.SP := Registers.SP + 2;
                     Inc_PC := False;
                  end if;
               when NC =>
                  if Registers.F.Carry = 0 then
                     Registers.PC := Read_Double (Registers.SP);
                     Registers.SP := Registers.SP + 2;
                     Inc_PC := False;
                  end if;
            end case;
         when None =>
            Registers.PC := Read_Double (Registers.SP);
            Registers.SP := Registers.SP + 2;
            Inc_PC := False;
      end case;
   end RET;

   procedure RETI is
   begin
      Registers.PC := Read_Double (Registers.SP);
      Registers.SP := Registers.SP + 2;
      Interrupt_Enable := True;
      Inc_PC := False;
   end RETI;

   --
   --  CALL
   --
   --  F_II: Jump_Flag, 16-bit Immediate
   --  II: 16-bit Immedaite
   --
   type CALL_Op_Type is (F_II, II);
   type CALL_Operands (O_Type : CALL_Op_Type) is record
      case O_Type is
         when F_II =>
            F : Jump_Flag;
         when II =>
            null;
      end case;
   end record;

   procedure CALL (Ops : CALL_Operands) is
   begin
      case Ops.O_Type is
         when F_II =>
            case Ops.F is
               when Z =>
                  if Registers.F.Zero = 1 then
                     Registers.SP := Registers.SP - 2;
                     Write_Double (Registers.PC, Registers.SP);
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when NZ =>
                  if Registers.F.Zero = 1 then
                     Registers.SP := Registers.SP - 2;
                     Write_Double (Registers.PC, Registers.SP);
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when C =>
                  if Registers.F.Zero = 1 then
                     Registers.SP := Registers.SP - 2;
                     Write_Double (Registers.PC, Registers.SP);
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when NC =>
                  if Registers.F.Zero = 1 then
                     Registers.SP := Registers.SP - 2;
                     Write_Double (Registers.PC, Registers.SP);
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
            end case;
         when II =>
            Registers.SP := Registers.SP - 2;
            Write_Double (Registers.PC, Registers.SP);
            Registers.PC := Read_Double (Registers.PC + 1);
            Inc_PC := False;
      end case;
   end CALL;

   procedure RST (Addr : UInt16) is
   begin
      Registers.SP := Registers.SP - 2;
      Write_Double (Registers.PC, Registers.SP);
      Registers.PC := Addr;
      Inc_PC := False;
   end RST;

   procedure Invalid_Instruction is
   begin
      raise Invalid_Instruction_Call_Exception
         with "Instruction reserved or unimplemented: " &
              Hex_Image (Read_Byte (Registers.PC));
   end Invalid_Instruction;

   -----------------------
   -- I/O and Execution --
   -----------------------

   function Instruction_Image (Addr : Address) return String is
      Instr : Instruction;
      OpCode : UInt8;
      Output : String (1 .. 18);
   begin
      OpCode := Read_Byte (Addr);
      Instr := Instr_Info (OpCode);

      Output := Instr.Image;
      for I in Instr.Image'Range loop
         case Instr.Bytes is
            -- 1 8-bit argument --
            when 2 =>
               OpCode := Read_Byte (Addr + 1);

               -- 8-bit Immediate --
               if Instr.Image (I) = 'V' then
                  Delete (Output, I, I);
                  Insert (Output, I, Hex_Image (OpCode), Ada.Strings.Right);
                  exit;

               -- 8-bit Signed Offset from PC --
               elsif Instr.Image (I) = 'M' then
                  declare
                     Double : constant UInt16 :=
                        Registers.PC -
                        (if (OpCode and 16#80#) > 0 then 1 else 0) * 128 +
                        UInt16 (OpCode and 16#7F#);
                  begin
                     Delete (Output, I, I);
                     Insert (Output, I, Hex_Image (Double), Ada.Strings.Right);
                  end;
                  exit;

               -- 8-bit Offset from 0xFF00 --
               elsif Instr.Image (I) = 'K' then
                  declare
                     Double : constant UInt16 := 16#FF00# + UInt16 (OpCode);
                  begin
                     Delete (Output, I, I);
                     Insert (Output, I, Hex_Image (Double), Ada.Strings.Right);
                  end;
                  exit;
               end if;

            -- 1 16-bit argument --
            when 3 =>
               if Instr.Image (I) = 'Y' then
                  declare
                     Double : constant UInt16_Split := (
                        Split => True,
                        Upper => Read_Byte (Addr + 2),
                        Lower => Read_Byte (Addr + 1)
                     );
                  begin
                     Delete (Output, I, I);
                     Insert (Output, I, Hex_Image (Double.Full),
                             Ada.Strings.Right);
                  end;
                  exit;
               end if;

            when others =>
               null;
         end case;
      end loop;

      return Output;
   end Instruction_Image;

   procedure Execute_Instruction is
      OpCode : constant UInt8 := Read_Byte (Registers.PC);
      Instr  : constant Instruction := Instr_Info (OpCode);
   begin
      Inc_PC := True;

      case OpCode is
         when 16#00# => NOP;
         when 16#01# => LD ((RR_II, BC));
         when 16#02# => LD ((RRA_A, BC));
         when 16#03# => INC ((RR, BC));
         when 16#04# => INC ((R, B));
         when 16#05# => DEC ((R, B));
         when 16#06# => LD ((R_I, B));
         --  when 16#07# => RLCA;
         when 16#08# => LD ((O_Type => II_SP));
         when 16#09# => ADD ((RR, BC));
         when 16#0A# => LD ((A_RRA, BC));
         when 16#0B# => DEC ((RR, BC));
         when 16#0C# => INC ((R, C));
         when 16#0D# => DEC ((R, C));
         when 16#0E# => LD ((R_I, C));
         --  when 16#0F# => RRCA;
         when 16#10# => STOP;
         when 16#11# => LD ((RR_II, DE));
         when 16#12# => LD ((RRA_A, DE));
         when 16#13# => INC ((RR, DE));
         when 16#14# => INC ((R, D));
         when 16#15# => DEC ((R, D));
         when 16#16# => LD ((R_I, D));
         --  when 16#17# => RLA;
         when 16#18# => JR ((O_Type => I));
         when 16#19# => ADD ((RR, DE));
         when 16#1A# => LD ((A_RRA, DE));
         when 16#1B# => DEC ((RR, DE));
         when 16#1C# => INC ((R, E));
         when 16#1D# => DEC ((R, E));
         when 16#1E# => LD ((R_I, E));
         --  when 16#1F# => RRA;
         when 16#20# => JR ((F_I, NZ));
         when 16#21# => LD ((RR_II, HL));
         when 16#22# => LDI (HLA_A);
         when 16#23# => INC ((RR, HL));
         when 16#24# => INC ((R, H));
         when 16#25# => DEC ((R, H));
         when 16#26# => LD ((R_I, H));
         --  when 16#27# => DAA;
         when 16#28# => JR ((F_I, Z));
         when 16#29# => ADD ((RR, HL));
         when 16#2A# => LDI (A_HLA);
         when 16#2B# => DEC ((RR, HL));
         when 16#2C# => INC ((R, L));
         when 16#2D# => DEC ((R, L));
         when 16#2E# => LD ((R_I, L));
         when 16#2F# => CPL;
         when 16#30# => JR ((F_I, NC));
         when 16#31# => LD ((RR_II, SP));
         when 16#32# => LDD (HLA_A);
         when 16#33# => INC ((RR, SP));
         when 16#34# => INC ((O_Type => HLA));
         when 16#35# => DEC ((O_Type => HLA));
         when 16#36# => LD ((O_Type => HLA_I));
         when 16#37# => SCF;
         when 16#38# => JR ((F_I, C));
         when 16#39# => ADD ((RR, SP));
         when 16#3A# => LDD (A_HLA);
         when 16#3B# => DEC ((RR, SP));
         when 16#3C# => INC ((R, A));
         when 16#3D# => DEC ((R, A));
         when 16#3E# => LD ((R_I, A));
         when 16#3F# => CCF;
         when 16#40# => LD ((R_R, B, B));
         when 16#41# => LD ((R_R, B, C));
         when 16#42# => LD ((R_R, B, D));
         when 16#43# => LD ((R_R, B, E));
         when 16#44# => LD ((R_R, B, H));
         when 16#45# => LD ((R_R, B, L));
         when 16#46# => LD ((R_HLA, B));
         when 16#47# => LD ((R_R, B, A));
         when 16#48# => LD ((R_R, C, B));
         when 16#49# => LD ((R_R, C, C));
         when 16#4A# => LD ((R_R, C, D));
         when 16#4B# => LD ((R_R, C, E));
         when 16#4C# => LD ((R_R, C, H));
         when 16#4D# => LD ((R_R, C, L));
         when 16#4E# => LD ((R_HLA, C));
         when 16#4F# => LD ((R_R, C, A));
         when 16#50# => LD ((R_R, D, B));
         when 16#51# => LD ((R_R, D, C));
         when 16#52# => LD ((R_R, D, D));
         when 16#53# => LD ((R_R, D, E));
         when 16#54# => LD ((R_R, D, H));
         when 16#55# => LD ((R_R, D, L));
         when 16#56# => LD ((R_HLA, D));
         when 16#57# => LD ((R_R, D, A));
         when 16#58# => LD ((R_R, E, B));
         when 16#59# => LD ((R_R, E, C));
         when 16#5A# => LD ((R_R, E, D));
         when 16#5B# => LD ((R_R, E, E));
         when 16#5C# => LD ((R_R, E, H));
         when 16#5D# => LD ((R_R, E, L));
         when 16#5E# => LD ((R_HLA, E));
         when 16#5F# => LD ((R_R, E, A));
         when 16#60# => LD ((R_R, H, B));
         when 16#61# => LD ((R_R, H, C));
         when 16#62# => LD ((R_R, H, D));
         when 16#63# => LD ((R_R, H, E));
         when 16#64# => LD ((R_R, H, H));
         when 16#65# => LD ((R_R, H, L));
         when 16#66# => LD ((R_HLA, H));
         when 16#67# => LD ((R_R, H, A));
         when 16#68# => LD ((R_R, L, B));
         when 16#69# => LD ((R_R, L, C));
         when 16#6A# => LD ((R_R, L, D));
         when 16#6B# => LD ((R_R, L, E));
         when 16#6C# => LD ((R_R, L, H));
         when 16#6D# => LD ((R_R, L, L));
         when 16#6E# => LD ((R_HLA, L));
         when 16#6F# => LD ((R_R, L, A));
         when 16#70# => LD ((HLA_R, B));
         when 16#71# => LD ((HLA_R, C));
         when 16#72# => LD ((HLA_R, D));
         when 16#73# => LD ((HLA_R, E));
         when 16#74# => LD ((HLA_R, H));
         when 16#75# => LD ((HLA_R, L));
         when 16#76# => HALT;
         when 16#77# => LD ((HLA_R, A));
         when 16#78# => LD ((R_R, A, B));
         when 16#79# => LD ((R_R, A, C));
         when 16#7A# => LD ((R_R, A, D));
         when 16#7B# => LD ((R_R, A, E));
         when 16#7C# => LD ((R_R, A, H));
         when 16#7D# => LD ((R_R, A, L));
         when 16#7E# => LD ((R_HLA, A));
         when 16#7F# => LD ((R_R, A, A));
         when 16#80# => ADD ((R, B));
         when 16#81# => ADD ((R, C));
         when 16#82# => ADD ((R, D));
         when 16#83# => ADD ((R, E));
         when 16#84# => ADD ((R, H));
         when 16#85# => ADD ((R, L));
         when 16#86# => ADD ((O_Type => HLA));
         when 16#87# => ADD ((R, A));
         when 16#88# => ADC ((R, B));
         when 16#89# => ADC ((R, C));
         when 16#8A# => ADC ((R, D));
         when 16#8B# => ADC ((R, E));
         when 16#8C# => ADC ((R, H));
         when 16#8D# => ADC ((R, L));
         when 16#8E# => ADC ((O_Type => HLA));
         when 16#8F# => ADC ((R, A));
         when 16#90# => SUB ((R, B));
         when 16#91# => SUB ((R, C));
         when 16#92# => SUB ((R, D));
         when 16#93# => SUB ((R, E));
         when 16#94# => SUB ((R, H));
         when 16#95# => SUB ((R, L));
         when 16#96# => SUB ((O_Type => HLA));
         when 16#97# => SUB ((R, A));
         when 16#98# => SBC ((R, B));
         when 16#99# => SBC ((R, C));
         when 16#9A# => SBC ((R, D));
         when 16#9B# => SBC ((R, E));
         when 16#9C# => SBC ((R, H));
         when 16#9D# => SBC ((R, L));
         when 16#9E# => SBC ((O_Type => HLA));
         when 16#9F# => SBC ((R, A));
         when 16#A0# => A_AND ((R, B));
         when 16#A1# => A_AND ((R, C));
         when 16#A2# => A_AND ((R, D));
         when 16#A3# => A_AND ((R, E));
         when 16#A4# => A_AND ((R, H));
         when 16#A5# => A_AND ((R, L));
         when 16#A6# => A_AND ((O_Type => HLA));
         when 16#A7# => A_AND ((R, A));
         when 16#A8# => A_XOR ((R, B));
         when 16#A9# => A_XOR ((R, C));
         when 16#AA# => A_XOR ((R, D));
         when 16#AB# => A_XOR ((R, E));
         when 16#AC# => A_XOR ((R, H));
         when 16#AD# => A_XOR ((R, L));
         when 16#AE# => A_XOR ((O_Type => HLA));
         when 16#AF# => A_XOR ((R, A));
         when 16#B0# => A_OR ((R, B));
         when 16#B1# => A_OR ((R, C));
         when 16#B2# => A_OR ((R, D));
         when 16#B3# => A_OR ((R, E));
         when 16#B4# => A_OR ((R, H));
         when 16#B5# => A_OR ((R, L));
         when 16#B6# => A_OR ((O_Type => HLA));
         when 16#B7# => A_OR ((R, A));
         when 16#B8# => CP ((R, B));
         when 16#B9# => CP ((R, C));
         when 16#BA# => CP ((R, D));
         when 16#BB# => CP ((R, E));
         when 16#BC# => CP ((R, H));
         when 16#BD# => CP ((R, L));
         when 16#BE# => CP ((O_Type => HLA));
         when 16#BF# => CP ((R, A));
         when 16#C0# => RET ((F, NZ));
         when 16#C1# => POP (BC);
         when 16#C2# => JP ((F_I, NZ));
         when 16#C3# => JP ((O_Type => I));
         when 16#C4# => CALL ((F_II, NZ));
         when 16#C5# => PUSH (BC);
         when 16#C6# => ADD ((O_Type => I));
         when 16#C7# => RST (16#00#);
         when 16#C8# => RET ((F, Z));
         when 16#C9# => RET ((O_Type => None));
         when 16#CA# => JP ((F_I, Z));
         --  when 16#CB# => Prefix;
         when 16#CC# => CALL ((F_II, Z));
         when 16#CD# => CALL ((O_Type => II));
         when 16#CE# => ADC ((O_Type => I));
         when 16#CF# => RST (16#08#);
         when 16#D0# => RET ((F, NC));
         when 16#D1# => POP (DE);
         when 16#D2# => JP ((F_I, NC));
         when 16#D4# => CALL ((F_II, NC));
         when 16#D5# => PUSH (DE);
         when 16#D6# => SUB ((O_Type => I));
         when 16#D7# => RST (16#10#);
         when 16#D8# => RET ((F, C));
         when 16#D9# => RETI;
         when 16#DA# => JP ((F_I, C));
         when 16#DC# => CALL ((F_II, C));
         when 16#DE# => ADC ((O_Type => I));
         when 16#DF# => RST (16#18#);
         when 16#E0# => LDH (IA_A);
         when 16#E1# => POP (HL);
         when 16#E2# => LD ((O_Type => CA_A));
         when 16#E5# => PUSH (HL);
         when 16#E6# => A_AND ((O_Type => I));
         when 16#E7# => RST (16#20#);
         when 16#E8# => ADD ((O_Type => SP));
         when 16#E9# => JP ((O_Type => HL));
         when 16#EA# => LD ((O_Type => II_A));
         when 16#EE# => A_XOR ((O_Type => I));
         when 16#EF# => RST (16#28#);
         when 16#F0# => LDH (A_IA);
         when 16#F1# => POP (AF);
         when 16#F2# => LD ((O_Type => A_CA));
         when 16#F3# => DI;
         when 16#F5# => PUSH (AF);
         when 16#F6# => A_OR ((O_Type => I));
         when 16#F7# => RST (16#30#);
         when 16#F8# => LD ((O_Type => HL_SP_SI));
         when 16#F9# => LD ((O_Type => SP_HL));
         when 16#FA# => LD ((O_Type => A_II));
         when 16#FB# => EI;
         when 16#FE# => CP ((O_Type => I));
         when 16#FF# => RST (16#38#);

         when others => Invalid_Instruction;
      end case;

      if Inc_PC then
         Registers.PC := Registers.PC + UInt16 (Instr.Bytes);
      end if;
   end Execute_Instruction;

end CPU.Logic;
