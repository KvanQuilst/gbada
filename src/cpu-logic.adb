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

   --
   --  LD
   --
   --  R_R: 8-bit Register, 8-bit Register
   --  R_I: 8-bit Register, Immediate
   --
   type LD_Op_Type is (R_R, R_I);
   type LD_Operands (O_Type : LD_Op_Type) is record
      case O_Type is
         when R_R =>
            Op1 : Register_8;
            Op2 : Register_8;
         when R_I =>
            Op : Register_8;
      end case;
   end record;

   procedure LD (Ops : LD_Operands) is
   begin
      case Ops.O_Type is
         when R_R =>
            Write_Register (Ops.Op1, Read_Register (Ops.Op2));
         when R_I =>
            Write_Register (Ops.Op, Read_Byte (Registers.PC + 1));
      end case;
   end LD;

   --
   --  16-bit Arithmetic Operation Operands
   --
   --  RR: 16-bit Register
   --
   type Arith16_Op_Type is (RR);
   type Arith16_Operands (O_Type : Arith16_Op_Type) is record
      case O_Type is
         when RR =>
            Op : Register_16;
      end case;
   end record;

   procedure INC16 (Ops : Arith16_Operands) is
   begin
      case Ops.O_Type is
         when RR =>
            Write_Register (Ops.Op, Read_Register (Ops.Op) + 1);
      end case;
   end INC16;

   procedure DEC16 (Ops : Arith16_Operands) is
   begin
      case Ops.O_Type is
         when RR =>
            Write_Register (Ops.Op, Read_Register (Ops.Op) - 1);
      end case;
   end DEC16;

   --
   --  8-bit Arithmetic Operation Operands
   --
   --  R: 8-bit Register
   --
   type Arith_Op_Type is (R);
   type Arith_Operands (O_Type : Arith_Op_Type) is record
      case O_Type is
         when R =>
            Op : Register_8;
      end case;
   end record;

   --  TODO:Flags
   procedure INC (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (Ops.Op, Read_Register (Ops.Op) + 1);
      end case;
   end INC;

   --  TODO: Flags
   procedure DEC (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (Ops.Op, Read_Register (Ops.Op) - 1);
      end case;
   end DEC;

   --  TODO: Flags
   procedure ADD (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val + Read_Register (Ops.Op));
      end case;
   end ADD;

   --  TODO: Flags
   procedure SUB (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val - Read_Register (Ops.Op));
      end case;
   end SUB;

   procedure A_AND (Ops : Arith_Operands) is
   begin
      case Ops.O_Type is
         when R =>
            Write_Register (A, Registers.A.Val and Read_Register (Ops.Op));
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
            Write_Register (A, Registers.A.Val xor Read_Register (Ops.Op));
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
            Write_Register (A, Registers.A.Val xor Read_Register (Ops.Op));
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

         when 16#03# => INC16 ((RR, BC));
         when 16#04# => INC ((R, B));
         when 16#05# => DEC ((R, B));
         when 16#06# => LD ((R_I, B));

         when 16#0B# => DEC16 ((RR, BC));
         when 16#0C# => INC ((R, C));
         when 16#0D# => DEC ((R, C));
         when 16#0E# => LD ((R_I, C));

         when 16#13# => INC16 ((RR, DE));
         when 16#14# => INC ((R, D));
         when 16#15# => DEC ((R, D));
         when 16#16# => LD ((R_I, D));

         when 16#1B# => DEC16 ((RR, DE));
         when 16#1C# => INC ((R, E));
         when 16#1D# => DEC ((R, E));
         when 16#1E# => LD ((R_I, E));

         when 16#23# => INC16 ((RR, HL));
         when 16#24# => INC ((R, H));
         when 16#25# => DEC ((R, H));
         when 16#26# => LD ((R_I, H));

         when 16#2B# => DEC16 ((RR, HL));
         when 16#2C# => INC ((R, L));
         when 16#2D# => DEC ((R, L));
         when 16#2E# => LD ((R_I, L));

         when 16#33# => INC16 ((RR, SP));

         when 16#3B# => DEC16 ((RR, SP));
         when 16#3C# => INC ((R, A));
         when 16#3D# => DEC ((R, A));
         when 16#3E# => LD ((R_I, A));

         when 16#40# => LD ((R_R, B, B));
         when 16#41# => LD ((R_R, B, C));
         when 16#42# => LD ((R_R, B, D));
         when 16#43# => LD ((R_R, B, E));
         when 16#44# => LD ((R_R, B, H));
         when 16#45# => LD ((R_R, B, L));
         --  when 16#46# =>
         when 16#47# => LD ((R_R, B, A));
         when 16#48# => LD ((R_R, C, B));
         when 16#49# => LD ((R_R, C, C));
         when 16#4A# => LD ((R_R, C, D));
         when 16#4B# => LD ((R_R, C, E));
         when 16#4C# => LD ((R_R, C, H));
         when 16#4D# => LD ((R_R, C, L));
         --  when 16#4E# =>
         when 16#4F# => LD ((R_R, C, A));
         when 16#50# => LD ((R_R, D, B));
         when 16#51# => LD ((R_R, D, C));
         when 16#52# => LD ((R_R, D, D));
         when 16#53# => LD ((R_R, D, E));
         when 16#54# => LD ((R_R, D, H));
         when 16#55# => LD ((R_R, D, L));
         --  when 16#56# =>
         when 16#57# => LD ((R_R, D, A));
         when 16#58# => LD ((R_R, E, B));
         when 16#59# => LD ((R_R, E, C));
         when 16#5A# => LD ((R_R, E, D));
         when 16#5B# => LD ((R_R, E, E));
         when 16#5C# => LD ((R_R, E, H));
         when 16#5D# => LD ((R_R, E, L));
         --  when 16#5E# =>
         when 16#5F# => LD ((R_R, E, A));
         when 16#60# => LD ((R_R, H, B));
         when 16#61# => LD ((R_R, H, C));
         when 16#62# => LD ((R_R, H, D));
         when 16#63# => LD ((R_R, H, E));
         when 16#64# => LD ((R_R, H, H));
         when 16#65# => LD ((R_R, H, L));
         --  when 16#66# =>
         when 16#67# => LD ((R_R, H, A));
         when 16#68# => LD ((R_R, L, B));
         when 16#69# => LD ((R_R, L, C));
         when 16#6A# => LD ((R_R, L, D));
         when 16#6B# => LD ((R_R, L, E));
         when 16#6C# => LD ((R_R, L, H));
         when 16#6D# => LD ((R_R, L, L));
         --  when 16#6E# =>
         when 16#6F# => LD ((R_R, L, A));
         when 16#80# => ADD ((R, B));
         when 16#81# => ADD ((R, C));
         when 16#82# => ADD ((R, D));
         when 16#83# => ADD ((R, E));
         when 16#84# => ADD ((R, H));
         when 16#85# => ADD ((R, L));
         --  when 16#86# =>
         when 16#87# => ADD ((R, A));
         --  when 16#88# .. 16#8F# =>
         when 16#90# => SUB ((R, B));
         when 16#91# => SUB ((R, C));
         when 16#92# => SUB ((R, D));
         when 16#93# => SUB ((R, E));
         when 16#94# => SUB ((R, H));
         when 16#95# => SUB ((R, L));
         --  when 16#96# =>
         when 16#97# => SUB ((R, A));
         --  when 16#98 .. 16#9F# =>
         when 16#A0# => A_AND ((R, B));
         when 16#A1# => A_AND ((R, C));
         when 16#A2# => A_AND ((R, D));
         when 16#A3# => A_AND ((R, E));
         when 16#A4# => A_AND ((R, H));
         when 16#A5# => A_AND ((R, L));
         --  when 16#A6# =>
         when 16#A7# => A_AND ((R, A));
         when 16#A8# => A_XOR ((R, B));
         when 16#A9# => A_XOR ((R, C));
         when 16#AA# => A_XOR ((R, D));
         when 16#AB# => A_XOR ((R, E));
         when 16#AC# => A_XOR ((R, H));
         when 16#AD# => A_XOR ((R, L));
         --  when 16#AE# =>
         when 16#AF# => A_XOR ((R, A));
         when 16#B0# => A_OR ((R, B));
         when 16#B1# => A_OR ((R, C));
         when 16#B2# => A_OR ((R, D));
         when 16#B3# => A_OR ((R, E));
         when 16#B4# => A_OR ((R, H));
         when 16#B5# => A_OR ((R, L));
         --  when 16#B6# =>
         when 16#B7# => A_OR ((R, A));

         when 16#C2# => JP ((C_II, NZ));
         when 16#C3# => JP ((O_Type => II));

         when 16#CA# => JP ((C_II, Z));

         when 16#D2# => JP ((C_II, NC));

         when 16#DA# => JP ((C_II, C));
         when others => Invalid_Instruction;
      end case;

      if Inc_PC then
         Registers.PC := Registers.PC + UInt16 (Instr.Bytes);
      end if;
   end Execute_Instruction;

end CPU.Logic;
