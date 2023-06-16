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

   procedure STOP is
   begin
      Halted := True;
   end STOP;

   procedure SCF is
   begin
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry  := 0;
      Registers.F.Carry       := 1;
   end SCF;

   procedure CCF is
   begin
      Registers.F.Subtraction := 0;
      Registers.F.Half_Carry  := 0;
      Registers.F.Carry       := not Registers.F.Carry;
   end CCF;

   procedure HALT is
   begin
      Halted := True;
   end HALT;

   procedure DI is
   begin
      Interrupt_Enable := False;
   end DI;

   procedure EI is
   begin
      Interrupt_Enable := True;
   end EI;

   --
   --  LD
   --
   --  R_R: 8-bit Register, 8-bit Register
   --  R_I: 8-bit Register, 8-bit Immediate
   --  RR_II: 16-bit Register, 16-bit Immediate
   --  RRA_A, A_RRA: 16-bit Register
   --  II_A, A_II: none
   --  II_SP: none
   --  R_HLA, HLA_R: 8-bit Register
   --  CA_A, A_CA: none
   --  SP_HL: none
   --
   type LD_Op_Type is (R_R, R_I, RR_II, RRA_A, A_RRA, II_A, A_II,
                       II_SP, R_HLA, HLA_R, CA_A, A_CA, SP_HL);
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
         when II_A | A_II | II_SP | CA_A | A_CA | SP_HL =>
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
         when SP_HL =>
            Registers.SP := Registers.HL.Val;
      end case;
   end LD;

   type LDID_Op_Type is (HLA_A, A_HLA);
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

   procedure PUSH (RR : Register_16) is
   begin
      Write_Double (Read_Register (RR), Registers.SP - 1);
      Registers.SP := Registers.SP - 2;
   end PUSH;

   procedure POP (RR : Register_16) is
   begin
      Write_Register (RR, Read_Double (Registers.SP + 1));
      Registers.SP := Registers.SP + 2;
   end POP;

   --
   --  Arithmetic Operation Operands
   --
   --  R: 8-bit Register
   --  I: 8-bit Immediate
   --  RR: 16-bit Register
   --  II: 16-bit Immediate
   --  HLA: none
   --
   type Arith_Op_Type is (R, I, RR, HLA);
   type Arith_Operands (O_Type : Arith_Op_Type) is record
      case O_Type is
         when R =>
            R : Register_8;
         when RR =>
            RR : Register_16;
         when I | HLA =>
            null;
      end case;
   end record;

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
               (if (Orig16 and 16#0FFF#) + (Orig16 and 16#0FFF#) = 16#1000#
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

   --
   --  Jumps
   --
   --  II: 16-bit Immediate
   --  C_II: Conditional Flag, 16-bit Immediate
   --
   type Jump_Flag is (Z, NZ, C, NC);
   type Jump_Op_Type is (II, C_II);
   type Jump_Operands (O_Type : Jump_Op_Type) is record
      case O_Type is
         when II =>
            null;
         when C_II =>
            F : Jump_Flag;
      end case;
   end record;

   procedure JP (Ops : Jump_Operands) is
   begin
      case Ops.O_Type is
         when II =>
            Registers.PC := Read_Double (Registers.PC + 1);
            Inc_PC := False;
         when C_II =>
            case Ops.F is
               when Z  =>
                  if Registers.F.Zero = 1 then
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when NZ =>
                  if Registers.F.Zero = 0 then
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when C  =>
                  if Registers.F.Carry = 1 then
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
               when NC =>
                  if Registers.F.Carry = 0 then
                     Registers.PC := Read_Double (Registers.PC + 1);
                     Inc_PC := False;
                  end if;
            end case;
      end case;
   end JP;

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

         when 16#08# => LD ((O_Type => II_SP));
         when 16#09# => ADD ((RR, BC));
         when 16#0A# => LD ((A_RRA, BC));
         when 16#0B# => DEC ((RR, BC));
         when 16#0C# => INC ((R, C));
         when 16#0D# => DEC ((R, C));
         when 16#0E# => LD ((R_I, C));

         when 16#10# => STOP;
         when 16#11# => LD ((RR_II, DE));
         when 16#12# => LD ((RRA_A, DE));
         when 16#13# => INC ((RR, DE));
         when 16#14# => INC ((R, D));
         when 16#15# => DEC ((R, D));
         when 16#16# => LD ((R_I, D));

         when 16#19# => ADD ((RR, DE));
         when 16#1A# => LD ((A_RRA, DE));
         when 16#1B# => DEC ((RR, DE));
         when 16#1C# => INC ((R, E));
         when 16#1D# => DEC ((R, E));
         when 16#1E# => LD ((R_I, E));

         when 16#21# => LD ((RR_II, HL));
         when 16#22# => LDI (HLA_A);
         when 16#23# => INC ((RR, HL));
         when 16#24# => INC ((R, H));
         when 16#25# => DEC ((R, H));
         when 16#26# => LD ((R_I, H));

         when 16#29# => ADD ((RR, HL));
         when 16#2A# => LDI (A_HLA);
         when 16#2B# => DEC ((RR, HL));
         when 16#2C# => INC ((R, L));
         when 16#2D# => DEC ((R, L));
         when 16#2E# => LD ((R_I, L));

         when 16#31# => LD ((RR_II, SP));
         when 16#32# => LDD (HLA_A);
         when 16#33# => INC ((RR, SP));

         when 16#37# => SCF;

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
         when 16#BB# => ADC ((R, E));
         when 16#BC# => ADC ((R, H));
         when 16#BD# => ADC ((R, L));
         when 16#BE# => ADC ((O_Type => HLA));
         when 16#BF# => ADC ((R, A));
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

         when 16#C1# => POP (BC);
         when 16#C2# => JP ((C_II, NZ));
         when 16#C3# => JP ((O_Type => II));

         when 16#C5# => PUSH (BC);
         when 16#C6# => ADD ((O_Type => I));

         when 16#CA# => JP ((C_II, Z));

         when 16#CE# => ADC ((O_Type => I));

         when 16#D1# => POP (DE);
         when 16#D2# => JP ((C_II, NC));

         when 16#D5# => PUSH (DE);
         when 16#D6# => SUB ((O_Type => I));

         when 16#DA# => JP ((C_II, C));

         when 16#DE# => ADC ((O_Type => I));

         when 16#E1# => POP (HL);
         when 16#E2# => LD ((O_Type => CA_A));
         when 16#E5# => PUSH (HL);
         when 16#E6# => A_AND ((O_Type => I));

         when 16#EA# => LD ((O_Type => II_A));
         when 16#EE# => A_XOR ((O_Type => I));

         when 16#F1# => POP (AF);
         when 16#F2# => LD ((O_Type => A_CA));
         when 16#F3# => DI;
         when 16#F5# => POP (AF);
         when 16#F6# => A_OR ((O_Type => I));

         when 16#F9# => LD ((O_Type => SP_HL));
         when 16#FA# => LD ((O_Type => A_II));
         when 16#FB# => EI;

         when others => Invalid_Instruction;
      end case;

      if Inc_PC then
         Registers.PC := Registers.PC + UInt16 (Instr.Bytes);
      end if;
   end Execute_Instruction;

end CPU.Logic;
