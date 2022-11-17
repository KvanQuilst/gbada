with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

--TODO: Include cycle delay with instructions!

package body CPU.Instructions is
  
  -------------
  -- Helpers --
  -------------
  
  procedure CheckZero is
  begin
    Reg.F.Zero_Flag := (if Reg.A.Val = 16#00# then 2#1# else 2#0#);
  end CheckZero;


  ----------
  -- Load --
  ----------

  -- 16-bit push register onto stack
  procedure PUSH_Instr (Source : Register16) is
  begin
    WriteDouble (Reg.SP.Val, Source.Val);
    Reg.SP.Val := Reg.SP.Val - 2;
  end PUSH_Instr;

  -- 16-bit pop stack into register
  procedure POP_Instr (Dest : in out Register16) is
  begin
    Dest.Val := ReadDouble (Reg.SP.Val);
    Reg.SP.Val := Reg.SP.Val + 2;
  end POP_Instr;

  ----------------
  -- Arithmetic --
  ----------------
  
  -- 8-bit add operand to Reg.A
  procedure ADD_Instr (Operand : Register) is
  begin
    Reg.F.Carry_Flag := (if Reg.A.Val + Operand.Val < Reg.A.Val then 2#1# else 2#0#);
    Reg.F.HalfCarry_Flag := (if Reg.A.Val < 16#F# and Reg.A.Val + Operand.Val > 16#F# then 2#1# else 2#0#);
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.A.Val := Reg.A.Val + Operand.Val;
    CheckZero;
  end ADD_Instr;

  -- 16-bit add operand to Reg.HL
  procedure ADD_Instr (Operand : Register16) is
  begin
    Reg.F.Carry_Flag := (if Reg.HL.Val + Operand.Val < Reg.HL.Val then 2#1# else 2#0#);
    Reg.F.HalfCarry_Flag := (if Reg.HL.Val < 16#FFF# and Reg.HL.Val + Operand.Val > 16#FFF#
                             then 2#1# else 2#0#);
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.HL.Val := Reg.HL.Val + Operand.Val;
  end ADD_Instr;

  -- 16-bit add operand to Reg.SP
  procedure ADD_Instr (Operand : UInt8) is
    Sub : Boolean := (Operand and 2#10000000#) /= 2#0#;
    Val : UInt16_Split := (False, 16#00#, (Operand and 2#01111111#));
  begin
    Reg.F.Zero_Flag := 2#0#;
    Reg.F.Subtraction_Flag := 2#0#;
    --TODO: Carry Flags
    if Sub then Reg.SP.Val := Reg.SP.Val - (128 - Val.Val);
           else Reg.SP.Val := Reg.SP.Val + Val.Val;
    end if;
  end ADD_Instr;

  
  -- 8-bit add operand and carry flag to Reg.A
  procedure ADC_Instr (Operand : Register) is
    C : UInt8 := (if Reg.F.Carry_Flag = 2#1# then 2#1# else 2#0#);
  begin
    Reg.F.Carry_Flag := (if Reg.A.Val + Operand.Val + C < Reg.A.Val then 2#1# else 2#0#);
    Reg.F.HalfCarry_Flag := (if Reg.A.Val < 16#F# and Reg.A.Val + Operand.Val + C > 16#F#
                             then 2#1# else 2#0#);
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.A.Val := Reg.A.Val + Operand.Val + C;
    CheckZero;
  end ADC_Instr;


  -- 8-bit subtract operand from Reg.A
  procedure SUB_Instr (Operand : Register) is
  begin
    --TODO: Carry Flags
    Reg.F.Subtraction_Flag := 2#1#;
    Reg.A.Val := Reg.A.Val - Operand.Val; 
    CheckZero;
  end SUB_Instr;


  -- 8-bit subtract operand and carry flag from Reg.A
  procedure SBC_Instr (Operand : Register) is
    C : UInt8 := (if Reg.F.Carry_Flag = 2#1# then 2#1# else 2#0#);
  begin
    --TODO: Carry Flags
    Reg.F.Subtraction_Flag := 2#1#;
    Reg.A.Val := Reg.A.Val - Operand.Val - C;
    CheckZero;
  end SBC_Instr;

  
  -- 8-bit and operand and Reg.A
  procedure AND_Instr (Operand : Register) is
  begin
    Reg.F.Carry_Flag := 2#0#;
    Reg.F.HalfCarry_Flag := 2#1#;
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.A.Val := (Reg.A.Val and Operand.Val);
    CheckZero;
  end AND_Instr;


  -- 8-bit or operand and Reg.A
  procedure OR_Instr (Operand : Register) is
  begin
    Reg.F.Carry_Flag := 2#0#;
    Reg.F.HalfCarry_Flag := 2#0#;
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.A.Val := (Reg.A.Val or Operand.Val);
    CheckZero;
  end OR_Instr;


  -- 8-bit xor operand and Reg.A
  procedure XOR_Instr (Operand : Register) is
  begin
    Reg.F.Carry_Flag := 2#0#;
    Reg.F.HalfCarry_Flag := 2#0#;
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.A.Val := (Reg.A.Val xor Operand.Val);
    CheckZero;
  end XOR_Instr;


  -- 8-bit compare operand and Reg.A
  procedure CP_Instr (Operand : Register) is
    R : UInt8 := Reg.A.Val - Operand.Val;
  begin
    --TODO: Carry Flags
    Reg.F.Subtraction_Flag := 2#1#;
    Reg.F.Zero_Flag := (if R = 16#00# then 2#1# else 2#0#);
  end CP_Instr;


  -- 8-bit increment register operand
  procedure INC_Instr (Operand : in out Register) is
  begin
    Reg.F.HalfCarry_Flag := (if Operand.Val < 16#F# then 2#1# else 2#0#);
    Reg.F.Subtraction_Flag := 2#0#;
    Operand.Val := Operand.Val + 1;
    Reg.F.Zero_Flag := (if Operand.Val = 16#00# then 2#1# else 2#0#);
  end INC_Instr;

  -- 16-bit increment register operand
  procedure INC_Instr (Operand : in out Register16) is
  begin
    Operand.Val := Operand.Val + 1;
  end INC_Instr;


  -- 8-bit decrement register operand
  procedure DEC_Instr (Operand : in out Register) is
  begin
    --TODO: Half Carry Flag 
    Reg.F.Subtraction_Flag := 2#1#;
    Operand.Val := Operand.Val - 1;
    Reg.F.Zero_Flag := (if Operand.Val = 16#00# then 2#1# else 2#0#);
  end DEC_Instr;

  -- 16-bit decrement register operand
  procedure DEC_Instr (Operand : in out Register16) is
  begin
    Operand.Val := Operand.Val + 1;
  end DEC_Instr;


  -----------
  -- Jumps --
  -----------

  -- Jump to combined address
  procedure JP_Instr (Addr : Address) is
  begin
    Reg.PC := Addr;
  end JP_Instr;

  -- Conditional jump to address
  type Conditional is (NZ, Z, NC, C);
  procedure JP_Instr (Cond : Conditional; Addr : Address) is
    Jump : Boolean;
  begin 
    case Cond is
      when NZ => Jump := (Reg.F.Zero_Flag = 2#0#);
      when Z => Jump := (Reg.F.Zero_Flag = 2#1#);
      when NC => Jump := (Reg.F.Carry_Flag = 2#0#);
      when C => Jump := (Reg.F.Carry_Flag = 2#1#);
    end case;
    Reg.PC := (if Jump then Addr else Reg.PC);
  end JP_Instr;


  -- Add operand to Reg.PC and jump
  procedure JR_Instr (Operand : UInt8) is
    Sub : Boolean := (Operand and 2#10000000#) /= 0;
    Val : UInt16_Split := (False, 16#00#, (Operand and 2#01111111#));
  begin
    if Sub then Reg.PC := Reg.PC - Val.Val - 1;
           else Reg.PC := Reg.PC + Val.Val;
    end if;
  end JR_Instr;

  -- Conditionally add operand to Reg.PC and jump
  procedure JR_Instr (Cond : Conditional; Operand : UInt8) is
    Sub : Boolean := (Operand and 2#10000000#) /= 0;
    Val : UInt16_Split := (False, 16#00#, (Operand and 2#01111111#));
    Jump : Boolean;
  begin
    case Cond is
      when NZ => Jump := (Reg.F.Zero_Flag = 2#0#);
      when Z => Jump := (Reg.F.Zero_Flag = 2#1#);
      when NC => Jump := (Reg.F.Carry_Flag = 2#0#);
      when C => Jump := (Reg.F.Carry_Flag = 2#1#);
    end case;
    if Sub then Reg.PC := (if Jump then Reg.PC - Val.Val - 1 else Reg.PC);
           else Reg.PC := (if Jump then Reg.PC + Val.Val else Reg.PC);
    end if;
  end JR_Instr;


  -----------
  -- Calls --
  -----------

  procedure CALL_Instr (Addr : Address) is
  begin
    WriteDouble (Reg.SP.Val, Reg.PC + 1);
    Reg.SP.Val := Reg.SP.Val - 1;
    JP_Instr (Addr);
  end CALL_Instr;

  procedure CALL_Instr (Cond : Conditional; Addr : Address) is
    Jump : Boolean;
  begin
    case Cond is
      when NZ => Jump := (Reg.F.Zero_Flag = 2#0#);
      when Z => Jump := (Reg.F.Zero_Flag = 2#1#);
      when NC => Jump := (Reg.F.Carry_Flag = 2#0#);
      when C => Jump := (Reg.F.Carry_Flag = 2#1#);
    end case;
    if Jump then 
      WriteDouble (Reg.SP.Val, Reg.PC + 1);
      Reg.SP.Val := Reg.SP.Val - 1;
      JP_Instr (Addr);
    end if;
  end CALL_Instr;

  ---------------
  -- Execution --
  ---------------

  procedure Read_Instruction is
  begin
    case MemMap (Reg.PC) is
      ----------
      -- Load --
      ----------
      
      when 16#C5# => PUSH_Instr (Reg.BC);
      when 16#D5# => PUSH_Instr (Reg.DE);
      when 16#E5# => PUSH_Instr (Reg.HL);
      when 16#F5# => PUSH_Instr (Reg.AF);

      when 16#C1# => POP_Instr (Reg.BC);
      when 16#D1# => POP_Instr (Reg.DE);
      when 16#E1# => POP_Instr (Reg.HL);
      when 16#F1# => POP_Instr (Reg.AF);

      ----------------
      -- Arithmetic --
      ----------------

      when 16#09# => ADD_Instr (Reg.BC);
      when 16#19# => ADD_Instr (Reg.DE);
      when 16#29# => ADD_Instr (Reg.HL);
      when 16#39# => ADD_Instr (Reg.SP);
      when 16#80# => ADD_Instr (Reg.B);
      when 16#81# => ADD_Instr (Reg.C);
      when 16#82# => ADD_Instr (Reg.D);
      when 16#83# => ADD_Instr (Reg.E);
      when 16#84# => ADD_Instr (Reg.H);
      when 16#85# => ADD_Instr (Reg.L);
      when 16#86# => ADD_Instr (ReadByte (Reg.HL.Val));
      when 16#87# => ADD_Instr (Reg.A);
      when 16#E8# => Reg.PC := Reg.PC + 1; ADD_Instr (ReadByte (Reg.PC)); 

      when 16#88# => ADC_Instr (Reg.B);
      when 16#89# => ADC_Instr (Reg.C);
      when 16#8A# => ADC_Instr (Reg.D);
      when 16#8B# => ADC_Instr (Reg.E);
      when 16#8C# => ADC_Instr (Reg.H);
      when 16#8D# => ADC_Instr (Reg.L);
      --TODO when 16#8E# => ADC_Instr (ReadByte (Reg.HL.Val));
      when 16#8F# => ADC_Instr (Reg.A);
      --TODO when 16#CE# => Reg.PC := Reg.PC + 1; ADC_Instr (ReadByte (Reg.PC));
      
      when 16#90# => SUB_Instr (Reg.B);
      when 16#91# => SUB_Instr (Reg.C);
      when 16#92# => SUB_Instr (Reg.D);
      when 16#93# => SUB_Instr (Reg.E);
      when 16#94# => SUB_Instr (Reg.H);
      when 16#95# => SUB_Instr (Reg.L);
      --TODO when 16#96# => SUB_Instr (ReadByte (Reg.HL.Val));
      when 16#97# => SUB_Instr (Reg.A);
      -- TODO when 16#D6# => Reg.PC := Reg.PC + 1; SUB_Instr (ReadByte (Reg.PC));
      
      when 16#98# => SBC_Instr (Reg.B);
      when 16#99# => SBC_Instr (Reg.C);
      when 16#9A# => SBC_Instr (Reg.D);
      when 16#9B# => SBC_Instr (Reg.E);
      when 16#9C# => SBC_Instr (Reg.H);
      when 16#9D# => SBC_Instr (Reg.L);
      --TODO when 16#9E# => SBC_Instr (ReadByte (Reg.HL.Val));
      when 16#9F# => SBC_Instr (Reg.A);
      --TODO when 16#DE# => Reg.PC := Reg.PC + 1; SBC_Instr (ReadByte (Reg.PC));
      
      when 16#A0# => AND_Instr (Reg.B);
      when 16#A1# => AND_Instr (Reg.C);
      when 16#A2# => AND_Instr (Reg.D);
      when 16#A3# => AND_Instr (Reg.E);
      when 16#A4# => AND_Instr (Reg.H);
      when 16#A5# => AND_Instr (Reg.L);
      --TODO when 16#A6# => AND_Instr (ReadByte (Reg.HL.Val));
      when 16#A7# => AND_Instr (Reg.A);
      --TODO when 16#E6# => Reg.PC := Reg.PC + 1; AND_Instr (ReadByte (Reg.PC));
      
      when 16#B0# => OR_Instr (Reg.B);
      when 16#B1# => OR_Instr (Reg.C);
      when 16#B2# => OR_Instr (Reg.D);
      when 16#B3# => OR_Instr (Reg.E);
      when 16#B4# => OR_Instr (Reg.H);
      when 16#B5# => OR_Instr (Reg.L);
      --TODO when 16#B6# => OR_Instr (ReadByte (Reg.HL.Val));
      when 16#B7# => OR_Instr (Reg.A);
      --TODO when 16#F6# => Reg.PC := Reg.PC + 1; OR_Instr (ReadByte (Reg.PC));
      
      when 16#A8# => XOR_Instr (Reg.B);
      when 16#A9# => XOR_Instr (Reg.C);
      when 16#AA# => XOR_Instr (Reg.D);
      when 16#AB# => XOR_Instr (Reg.E);
      when 16#AC# => XOR_Instr (Reg.H);
      when 16#AD# => XOR_Instr (Reg.L);
      --TODO when 16#AE# => XOR_Instr (ReadByte (Reg.HL.Val));
      when 16#AF# => XOR_Instr (Reg.A);
      -- TODO when 16#EE# => Reg.PC := Reg.PC + 1; XOR_Instr (ReadByte (Reg.PC));

      when 16#B8# => CP_Instr (Reg.B);
      when 16#B9# => CP_Instr (Reg.C);
      when 16#BA# => CP_Instr (Reg.D);
      when 16#BB# => CP_Instr (Reg.E);
      when 16#BC# => CP_Instr (Reg.H);
      when 16#BD# => CP_Instr (Reg.L);
      --TODO when 16#BE# => CP_Instr (ReadByte (Reg.HL.Val));
      when 16#BF# => CP_Instr (Reg.A);
      --TODO when 16#FE# => Reg.PC := Reg.PC + 1; CP_Instr (ReadByte (Reg.PC));

      when 16#03# => INC_Instr (Reg.BC);
      when 16#04# => INC_Instr (Reg.B);
      when 16#0C# => INC_Instr (Reg.C);
      when 16#13# => INC_Instr (Reg.DE);
      when 16#14# => INC_Instr (Reg.D);
      when 16#1C# => INC_Instr (Reg.E);
      when 16#23# => INC_Instr (Reg.HL);
      when 16#24# => INC_Instr (Reg.H);
      when 16#2C# => INC_Instr (Reg.L);
      when 16#33# => INC_Instr (Reg.SP);
      --TODO when 16#34# => INC_Instr ( (HL) );
      when 16#3C# => INC_Instr (Reg.A);

      when 16#05# => DEC_Instr (Reg.B);
      when 16#0B# => DEC_Instr (Reg.BC);
      when 16#0D# => DEC_Instr (Reg.C);
      when 16#15# => DEC_Instr (Reg.D);
      when 16#1B# => DEC_Instr (Reg.DE);
      when 16#1D# => DEC_Instr (Reg.E);
      when 16#25# => DEC_Instr (Reg.H);
      when 16#2B# => DEC_Instr (Reg.HL);
      when 16#2D# => DEC_Instr (Reg.L);
      --TODO when 16#35# => DEC_Instr ( (HL) );
      when 16#3B# => DEC_Instr (Reg.SP);
      when 16#3D# => DEC_Instr (Reg.A);

      -----------
      -- Jumps --
      -----------
      
      when 16#C2# => Reg.PC := Reg.PC + 2; JP_Instr (NZ, ReadDouble (Reg.PC - 1));
      when 16#C3# => Reg.PC := Reg.PC + 2; JP_Instr (ReadDouble (Reg.PC - 1));
      when 16#CA# => Reg.PC := Reg.PC + 2; JP_Instr (Z, ReadDouble (Reg.PC - 1));
      when 16#D2# => Reg.PC := Reg.PC + 2; JP_Instr (NC, ReadDouble (Reg.PC - 1));
      when 16#DA# => Reg.PC := Reg.PC + 2; JP_Instr (C, ReadDouble (Reg.PC - 1));
      when 16#E9# => JP_Instr (Reg.HL.Val);

      when 16#18# => Reg.PC := Reg.PC + 1; JR_Instr (ReadByte (Reg.PC));
      when 16#20# => Reg.PC := Reg.PC + 1; JR_Instr (NZ, ReadByte (Reg.PC));
      when 16#28# => Reg.PC := Reg.PC + 1; JR_Instr (Z, ReadByte (Reg.PC));
      when 16#30# => Reg.PC := Reg.PC + 1; JR_Instr (NC, ReadByte (Reg.PC));
      when 16#38# => Reg.PC := Reg.PC + 1; JR_Instr (C, ReadByte (Reg.PC));

      -----------
      -- Calls --
      -----------
      
      when 16#C4# => Reg.PC := Reg.PC + 2; Call_Instr (NZ, ReadDouble (Reg.PC - 1));
      when 16#CC# => Reg.PC := Reg.PC + 2; Call_Instr (Z, ReadDouble (Reg.PC - 1));
      when 16#CD# => Reg.PC := Reg.PC + 2; Call_Instr (ReadDouble (Reg.PC - 1));
      when 16#D4# => Reg.PC := Reg.PC + 2; Call_Instr (NC, ReadDouble (Reg.PC - 1));
      when 16#DC# => Reg.PC := Reg.PC + 2; Call_Instr (C, ReadDouble (Reg.PC - 1));


      when others => raise Invalid_Instruction_Call_Exception
        with "Instruction not implemented!";
    end case;
    Reg.PC := Reg.PC + 1;
  end Read_Instruction;
end CPU.Instructions;
