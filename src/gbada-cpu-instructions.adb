with Ada.Text_IO; use Ada.Text_IO;
--TODO: Include cycle delay with instructions!

package body Gbada.CPU.Instructions is
  
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
  
  -- 8-bit load value of source register into dest register
  procedure LD_Instr (Dest : out Register; Source : Register) is
  begin
    Dest.Val := Source.Val; 
  end LD_Instr;

  -- 8-bit load value at address into dest register
  procedure LD_Instr (Dest : out Register; Addr : Address) is
  begin
    Dest.Val := ReadByte (Addr);
  end LD_Instr;

  -- 8-bit load value of source regsiter into address
  procedure LD_Instr (Addr : Address; Source : Register) is
  begin
    WriteByte (Addr, Source.Val);
  end LD_Instr;

  -- 8-bit load operand into register
  procedure LD_Instr (Dest : out Register; Operand : UInt8) is
  begin
    Dest.Val := Operand;
  end LD_Instr;

  -- 8-bit load operand into address
  procedure LD_Instr (Addr : Address; Operand : UInt8) is
  begin
    WriteByte (Addr, Operand);
  end LD_Instr;

  -- 8-bit load $FF00 + Reg.C into A or vice versa
  type C_IO is (In_C, Out_C);
  procedure LD_Instr (IO : C_IO) is
    Addr : UInt16_Split := (False, 16#FF#, Reg.C.Val);
  begin
    case IO is
      when In_C => WriteByte (Addr.Val, Reg.A.Val);
      when Out_C => Reg.A.Val := ReadByte (Addr.Val);
    end case;
  end LD_Instr;

  -- 16-bit load operand into destination register
  procedure LD_Instr (Dest : out Register16; Operand : UInt16) is
  begin 
    Dest.Val := Operand;
  end LD_Instr;

  -- 16-bit load source register into dest register
  procedure LD_Instr (Dest : out Register16; Source : Register16) is
  begin
    Dest.Val := Source.Val;
  end LD_Instr;

  -- 16-bit put Reg.SP at addr
  procedure LD_Instr (Addr : Address) is
  begin
    WriteDouble (Addr, Reg.SP.Val);
  end LD_Instr;


  -- 8-bit load (HL) into Reg.A or vice versa and decrement HL
  type HL_IO is (In_HL, Out_HL);
  procedure LDD_Instr (IO : HL_IO) is
  begin
    case IO is
      when In_HL => WriteByte (Reg.HL.Val, Reg.A.Val);
      when Out_HL => Reg.A.Val := ReadByte (Reg.HL.Val);
    end case;

    --TODO: Carry Flags
    Reg.F.Subtraction_Flag := 2#1#;
    Reg.HL.Val := Reg.HL.Val - 1;
    Reg.F.Zero_Flag := (if Reg.HL.Val = 0 then 2#1# else 2#0#);
  end LDD_Instr;


  -- 8-bit load (HL) into Reg.A or vice versa and decrement HL
  procedure LDI_Instr (IO : HL_IO) is
  begin
    case IO is
      when In_HL => WriteByte (Reg.HL.Val, Reg.A.Val);
      when Out_HL => Reg.A.Val := ReadByte (Reg.HL.Val);
    end case;

    Reg.F.HalfCarry_Flag := (if Reg.HL.Val < 16#FFF# then 2#1# else 2#0#);
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.HL.Val := Reg.HL.Val + 1;
    Reg.F.Zero_Flag := (if Reg.HL.Val = 0 then 2#1# else 2#0#);
  end LDI_Instr;


  -- 8-bit load $FF00 + operand into Reg.A or vice versa
  type FF00_IO is (In_FF00, Out_FF00);
  procedure LDH_Instr (Operand : UInt8; IO : FF00_IO) is
    Addr : UInt16_Split := (False, 16#FF#, Operand);
  begin
    case IO is
      when In_FF00 => WriteByte (Addr.Val, Reg.A.Val);
      when Out_FF00 => Reg.A.Val := ReadByte (Addr.Val);
    end case;
  end LDH_Instr;


  -- 16-bit load Reg.SP + operand into Reg.HL
  procedure LDHL_Instr (Operand : UInt8) is
    Sub : Boolean := (Operand and 2#10000000#) > 0;
    Val : UInt16_Split := (False, 16#00#, (Operand and 2#01111111#));
  begin
    --TODO: Carry Flags
    Reg.F.Subtraction_Flag := 2#0#;
    Reg.F.Zero_Flag := 2#0#;
    if Sub then Reg.HL.Val := Reg.SP.Val - Val.Val - 1;
           else Reg.HL.Val := Reg.SP.Val + Val.Val;
    end if;
  end LDHL_Instr;


  -- 16-bit push register onto stack
  procedure PUSH_Instr (Source : Register16) is
  begin
    WriteDouble (Reg.SP.Val, Source.Val);
    Reg.SP.Val := Reg.SP.Val - 2;
  end PUSH_Instr;


  -- 16-bit pop stack into register
  procedure POP_Instr (Dest : out Register16) is
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

  procedure CP_Instr (Operand : UInt8) is
    R : UInt8 := Reg.A.Val - Operand;
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
    Operand.Val := Operand.Val - 1;
  end DEC_Instr;


  ----------
  -- Misc --
  ----------

  procedure NOP_Instr is
  begin
    null;
  end NOP_Instr;

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
    Val : UInt16_Split := (False, (Operand and 2#01111111#), 16#00#);
  begin
    if Sub then Reg.PC := Reg.PC - (128 - Val.Val);
           else Reg.PC := Reg.PC + Val.Val;
    end if;
  end JR_Instr;

  -- Conditionally add operand to Reg.PC and jump
  procedure JR_Instr (Cond : Conditional; Operand : UInt8) is
    Sub : Boolean := (Operand and 2#10000000#) /= 0;
    Val : UInt16_Split := (False, (Operand and 2#01111111#), 16#00#);
    Jump : Boolean;
  begin
    case Cond is
      when NZ => Jump := (Reg.F.Zero_Flag = 2#0#);
      when Z => Jump := (Reg.F.Zero_Flag = 2#1#);
      when NC => Jump := (Reg.F.Carry_Flag = 2#0#);
      when C => Jump := (Reg.F.Carry_Flag = 2#1#);
    end case;
    if Sub then Reg.PC := (if Jump then Reg.PC - (128 - Val.Val) else Reg.PC);
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
    ByteCode : UInt8 := ReadByte (Reg.PC);
    Operand8 : UInt8;
    Operand16 : UInt16;
  begin
    Operand8 := (if Instr_Info (ByteCode).Operands = 2
                 then ReadByte (Reg.PC + 1) else 0);
    Operand16 := (if Instr_Info (ByteCode).Operands = 3
                  then ReadDouble (Reg.PC + 1) else 0);

    case ByteCode is
      when 16#00# => NOP_Instr;
      when 16#01# => LD_Instr (Reg.BC, Operand16);
      when 16#02# => LD_Instr (Reg.BC.Val, Reg.A);
      when 16#03# => INC_Instr (Reg.BC);
      when 16#04# => INC_Instr (Reg.B);
      when 16#05# => DEC_Instr (Reg.B);
      when 16#06# => LD_Instr (Reg.B, Operand8);
      when 16#08# => LD_Instr (Operand16);
      when 16#09# => ADD_Instr (Reg.BC);
      when 16#0A# => LD_Instr (Reg.A, Reg.BC.Val);
      when 16#0B# => DEC_Instr (Reg.BC);
      when 16#0C# => INC_Instr (Reg.C);
      when 16#0D# => DEC_Instr (Reg.C);
      when 16#0E# => LD_Instr (Reg.C, Operand8);
      when 16#11# => LD_Instr (Reg.DE, Operand16);
      when 16#12# => LD_Instr (Reg.DE.Val , Reg.A);
      when 16#13# => INC_Instr (Reg.DE);
      when 16#14# => INC_Instr (Reg.D);
      when 16#15# => DEC_Instr (Reg.D);
      when 16#18# => JR_Instr (Operand8);
      when 16#19# => ADD_Instr (Reg.DE);
      when 16#1A# => LD_Instr (Reg.A, Reg.DE.Val);
      when 16#1B# => DEC_Instr (Reg.DE);
      when 16#1C# => INC_Instr (Reg.E);
      when 16#1D# => DEC_Instr (Reg.E);
      when 16#20# => JR_Instr (NZ, Operand8);
      when 16#21# => LD_Instr (Reg.HL, Operand16);
      when 16#22# => LDI_Instr (In_HL);
      when 16#23# => INC_Instr (Reg.HL);
      when 16#24# => INC_Instr (Reg.H);
      when 16#25# => DEC_Instr (Reg.H);
      when 16#28# => JR_Instr (Z, Operand8);
      when 16#29# => ADD_Instr (Reg.HL);
      when 16#2A# => LDI_Instr (Out_HL);
      when 16#2B# => DEC_Instr (Reg.HL);
      when 16#2C# => INC_Instr (Reg.L);
      when 16#2D# => DEC_Instr (Reg.L);
      when 16#30# => JR_Instr (NC, Operand8);
      when 16#31# => LD_Instr (Reg.SP, Operand16);
      when 16#32# => LDD_Instr (In_HL);
      when 16#33# => INC_Instr (Reg.SP);
      --TODO when 16#34# => INC_Instr ( (HL) );
      --TODO when 16#35# => DEC_Instr ( (HL) );
      when 16#36# => LD_Instr (Reg.HL.Val, Operand8);
      when 16#38# => JR_Instr (C, Operand8);
      when 16#39# => ADD_Instr (Reg.SP);
      when 16#3A# => LDD_Instr (Out_HL);
      when 16#3B# => DEC_Instr (Reg.SP);
      when 16#3C# => INC_Instr (Reg.A);
      when 16#3D# => DEC_Instr (Reg.A);
      when 16#3E# => LD_Instr (Reg.A, Operand8);
      when 16#40# => LD_Instr (Reg.B, Reg.B);
      when 16#41# => LD_Instr (Reg.B, Reg.C);
      when 16#42# => LD_Instr (Reg.B, Reg.D);
      when 16#43# => LD_Instr (Reg.B, Reg.E);
      when 16#44# => LD_Instr (Reg.B, Reg.H);
      when 16#45# => LD_Instr (Reg.B, Reg.L);
      when 16#46# => LD_Instr (Reg.B, Reg.HL.Val);
      when 16#47# => LD_Instr (Reg.B, Reg.A);
      when 16#48# => LD_Instr (Reg.C, Reg.B);
      when 16#49# => LD_Instr (Reg.C, Reg.C);
      when 16#4A# => LD_Instr (Reg.C, Reg.D);
      when 16#4B# => LD_Instr (Reg.C, Reg.E);
      when 16#4C# => LD_Instr (Reg.C, Reg.H);
      when 16#4D# => LD_Instr (Reg.C, Reg.L);
      when 16#4E# => LD_Instr (Reg.C, Reg.HL.Val);
      when 16#4F# => LD_Instr (Reg.C, Reg.A);
      when 16#50# => LD_Instr (Reg.D, Reg.B);
      when 16#51# => LD_Instr (Reg.D, Reg.C);
      when 16#52# => LD_Instr (Reg.D, Reg.D);
      when 16#53# => LD_Instr (Reg.D, Reg.E);
      when 16#54# => LD_Instr (Reg.D, Reg.H);
      when 16#55# => LD_Instr (Reg.D, Reg.L);
      when 16#56# => LD_Instr (Reg.D, Reg.HL.Val);
      when 16#57# => LD_Instr (Reg.D, Reg.A);
      when 16#58# => LD_Instr (Reg.E, Reg.B);
      when 16#59# => LD_Instr (Reg.E, Reg.C);
      when 16#5A# => LD_Instr (Reg.E, Reg.D);
      when 16#5B# => LD_Instr (Reg.E, Reg.E);
      when 16#5C# => LD_Instr (Reg.E, Reg.H);
      when 16#5D# => LD_Instr (Reg.E, Reg.L);
      when 16#5E# => LD_Instr (Reg.E, Reg.HL.Val);
      when 16#5F# => LD_Instr (Reg.E, Reg.A);
      when 16#60# => LD_Instr (Reg.H, Reg.B);
      when 16#61# => LD_Instr (Reg.H, Reg.C);
      when 16#62# => LD_Instr (Reg.H, Reg.D);
      when 16#63# => LD_Instr (Reg.H, Reg.E);
      when 16#64# => LD_Instr (Reg.H, Reg.H);
      when 16#65# => LD_Instr (Reg.H, Reg.L);
      when 16#66# => LD_Instr (Reg.H, Reg.HL.Val);
      when 16#67# => LD_Instr (Reg.H, Reg.A);
      when 16#68# => LD_Instr (Reg.L, Reg.B);
      when 16#69# => LD_Instr (Reg.L, Reg.C);
      when 16#6A# => LD_Instr (Reg.L, Reg.D);
      when 16#6B# => LD_Instr (Reg.L, Reg.E);
      when 16#6C# => LD_Instr (Reg.L, Reg.H);
      when 16#6D# => LD_Instr (Reg.L, Reg.L);
      when 16#6E# => LD_Instr (Reg.L, Reg.HL.Val);
      when 16#70# => LD_Instr (Reg.HL.Val, Reg.B);
      when 16#71# => LD_Instr (Reg.HL.Val, Reg.C);
      when 16#72# => LD_Instr (Reg.HL.Val, Reg.D);
      when 16#73# => LD_Instr (Reg.HL.Val, Reg.E);
      when 16#74# => LD_Instr (Reg.HL.Val, Reg.H);
      when 16#75# => LD_Instr (Reg.HL.Val, Reg.L);
      when 16#77# => LD_Instr (Reg.HL.Val, Reg.A);
      when 16#78# => LD_Instr (Reg.A, Reg.B);
      when 16#79# => LD_Instr (Reg.A, Reg.C);
      when 16#7A# => LD_Instr (Reg.A, Reg.D);
      when 16#7B# => LD_Instr (Reg.A, Reg.E);
      when 16#7C# => LD_Instr (Reg.A, Reg.H);
      when 16#7D# => LD_Instr (Reg.A, Reg.L);
      when 16#7E# => LD_Instr (Reg.A, Reg.HL.Val);
      when 16#7F# => LD_Instr (Reg.A, Reg.A);
      when 16#80# => ADD_Instr (Reg.B);
      when 16#81# => ADD_Instr (Reg.C);
      when 16#82# => ADD_Instr (Reg.D);
      when 16#83# => ADD_Instr (Reg.E);
      when 16#84# => ADD_Instr (Reg.H);
      when 16#85# => ADD_Instr (Reg.L);
      when 16#86# => ADD_Instr (ReadByte (Reg.HL.Val));
      when 16#87# => ADD_Instr (Reg.A);
      when 16#88# => ADC_Instr (Reg.B);
      when 16#89# => ADC_Instr (Reg.C);
      when 16#8A# => ADC_Instr (Reg.D);
      when 16#8B# => ADC_Instr (Reg.E);
      when 16#8C# => ADC_Instr (Reg.H);
      when 16#8D# => ADC_Instr (Reg.L);
      --TODO when 16#8E# => ADC_Instr (ReadByte (Reg.HL.Val));
      when 16#8F# => ADC_Instr (Reg.A);
      when 16#90# => SUB_Instr (Reg.B);
      when 16#91# => SUB_Instr (Reg.C);
      when 16#92# => SUB_Instr (Reg.D);
      when 16#93# => SUB_Instr (Reg.E);
      when 16#94# => SUB_Instr (Reg.H);
      when 16#95# => SUB_Instr (Reg.L);
      --TODO when 16#96# => SUB_Instr (ReadByte (Reg.HL.Val));
      when 16#97# => SUB_Instr (Reg.A);
      when 16#98# => SBC_Instr (Reg.B);
      when 16#99# => SBC_Instr (Reg.C);
      when 16#9A# => SBC_Instr (Reg.D);
      when 16#9B# => SBC_Instr (Reg.E);
      when 16#9C# => SBC_Instr (Reg.H);
      when 16#9D# => SBC_Instr (Reg.L);
      --TODO when 16#9E# => SBC_Instr (ReadByte (Reg.HL.Val));
      when 16#9F# => SBC_Instr (Reg.A);
      when 16#A0# => AND_Instr (Reg.B);
      when 16#A1# => AND_Instr (Reg.C);
      when 16#A2# => AND_Instr (Reg.D);
      when 16#A3# => AND_Instr (Reg.E);
      when 16#A4# => AND_Instr (Reg.H);
      when 16#A5# => AND_Instr (Reg.L);
      --TODO when 16#A6# => AND_Instr (ReadByte (Reg.HL.Val));
      when 16#A7# => AND_Instr (Reg.A);
      when 16#A8# => XOR_Instr (Reg.B);
      when 16#A9# => XOR_Instr (Reg.C);
      when 16#AA# => XOR_Instr (Reg.D);
      when 16#AB# => XOR_Instr (Reg.E);
      when 16#AC# => XOR_Instr (Reg.H);
      when 16#AD# => XOR_Instr (Reg.L);
      --TODO when 16#AE# => XOR_Instr (ReadByte (Reg.HL.Val));
      when 16#AF# => XOR_Instr (Reg.A);
      when 16#B0# => OR_Instr (Reg.B);
      when 16#B1# => OR_Instr (Reg.C);
      when 16#B2# => OR_Instr (Reg.D);
      when 16#B3# => OR_Instr (Reg.E);
      when 16#B4# => OR_Instr (Reg.H);
      when 16#B5# => OR_Instr (Reg.L);
      --TODO when 16#B6# => OR_Instr (ReadByte (Reg.HL.Val));
      when 16#B7# => OR_Instr (Reg.A);
      when 16#B8# => CP_Instr (Reg.B);
      when 16#B9# => CP_Instr (Reg.C);
      when 16#BA# => CP_Instr (Reg.D);
      when 16#BB# => CP_Instr (Reg.E);
      when 16#BC# => CP_Instr (Reg.H);
      when 16#BD# => CP_Instr (Reg.L);
      --TODO when 16#BE# => CP_Instr (ReadByte (Reg.HL.Val));
      when 16#BF# => CP_Instr (Reg.A);
      when 16#E8# => ADD_Instr (Operand8); 
      when 16#C1# => POP_Instr (Reg.BC);
      when 16#C2# => JP_Instr (NZ, Operand16);
      when 16#C3# => JP_Instr (Operand16);
      when 16#C4# => Call_Instr (NZ, Operand16);
      when 16#C5# => PUSH_Instr (Reg.BC);
      when 16#CA# => JP_Instr (Z, Operand16);
      when 16#CC# => Call_Instr (Z, Operand16);
      when 16#CD# => Call_Instr (Operand16);
      --TODO when 16#CE# => Reg.PC := Reg.PC + 1; ADC_Instr (ReadByte (Reg.PC));
      when 16#D1# => POP_Instr (Reg.DE);
      when 16#D2# => JP_Instr (NC, Operand16);
      when 16#D4# => Call_Instr (NC, Operand16);
      when 16#D5# => PUSH_Instr (Reg.DE);
      --TODO when 16#D6# => Reg.PC := Reg.PC + 1; SUB_Instr (ReadByte (Reg.PC));
      when 16#DA# => JP_Instr (C, Operand16);
      when 16#DC# => Call_Instr (C, Operand16);
      --TODO when 16#DE# => Reg.PC := Reg.PC + 1; SBC_Instr (ReadByte (Reg.PC));
      when 16#E0# => LDH_Instr (Operand8, In_FF00);
      when 16#E1# => POP_Instr (Reg.HL);
      when 16#E2# => LD_Instr (In_C);
      when 16#E5# => PUSH_Instr (Reg.HL);
      --TODO when 16#E6# => Reg.PC := Reg.PC + 1; AND_Instr (ReadByte (Reg.PC));
      when 16#E9# => JP_Instr (Reg.HL.Val);
      when 16#EA# => LD_Instr (Operand16, Reg.A);
      --TODO when 16#EE# => XOR_Instr (Operand8);
      when 16#F0# => LDH_Instr (Operand8, Out_FF00);
      when 16#F1# => POP_Instr (Reg.AF);
      when 16#F2# => LD_Instr (Out_C);
      when 16#F5# => PUSH_Instr (Reg.AF);
      --TODO when 16#F6# => Reg.PC := Reg.PC + 1; OR_Instr (ReadByte (Reg.PC));
      when 16#F8# => LDHL_Instr (Operand8);
      when 16#F9# => LD_Instr (Reg.SP, Reg.HL);
      when 16#FA# => LD_Instr (Reg.A, ReadByte (Operand16));
      when 16#FE# => CP_Instr (Operand8);

      when others => raise Invalid_Instruction_Call_Exception
                      with "Instruction not implemented!";
    end case;
    Reg.PC := Reg.PC + Instr_Info (ReadByte (Reg.PC)).Operands;
  end Read_Instruction;


  function Instruction_String return String is
  begin
    return Instr_Info (ReadByte (Reg.PC)).Image;
  end Instruction_String;

end Gbada.CPU.Instructions;
