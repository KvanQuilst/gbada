with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

package CPU is

  type Register (Flag : Boolean := True) is 
    record
      case Flag is 
        when True =>
          Reserved_0_3 : UInt4 := 16#0#;
          Carry_Flag : Bit := 2#0#;
          HalfCarry_Flag : Bit := 2#0#;
          Subtraction_Flag : Bit := 2#0#;
          Zero_Flag : Bit := 2#0#;
        when False =>
          Val : UInt8 := 16#00#;
      end case;
  end record
    with Unchecked_Union;

  for Register use record
    Reserved_0_3 at 0 range 0..3;
    Carry_Flag at 0 range 4..4;
    HalfCarry_Flag at 0 range 5..5;
    Subtraction_Flag at 0 range 6..6;
    Zero_Flag at 0 range 7..7;
    Val at 0 range 0..7;
  end record;

  type Register16 (Raw : Boolean := False) is
    record
      case Raw is
        when True =>
          Val : Address;
        when False =>
          Upper : Register;
          Lower : Register;
      end case;
    end record
      with Unchecked_Union;

  for Register16 use record
    Val at 0 range 0..15;
    Upper at 0 range 0..7;
    Lower at 1 range 0..7;
  end record;

  type Registers (Paired : Boolean := False) is 
    record
      A : Register;
      F : Register;
      SP : Register16;
      PC : Address;
      case Paired is
        when True =>
          BC : Register16;
          DE : Register16;
          HL : Register16;
        when False =>
          B : Register;
          C : Register;
          D : Register;
          E : Register;
          H : Register;
          L : Register;
      end case;
    end record
      with Unchecked_Union;

  for Registers use record
    A  at 16#0# range 0..7;
    F  at 16#1# range 0..7;
    BC at 16#2# range 0..15;
    B  at 16#2# range 0..7;
    C  at 16#3# range 0..7;
    DE at 16#4# range 0..15;
    D  at 16#4# range 0..7;
    E  at 16#5# range 0..7;
    HL at 16#6# range 0..15;
    H  at 16#6# range 0..7;
    L  at 16#7# range 0..7;
    SP at 16#8# range 0..15;
    PC at 16#10# range 0..15;
  end record;

  Reg : Registers := (Paired => False,
    A => (False, 16#00#),
    F => (False, 16#00#),
    B => (False, 16#00#),
    C => (False, 16#00#),
    D => (False, 16#00#),
    E => (False, 16#00#),
    H => (False, 16#00#),
    L => (False, 16#00#),
    SP => (True, 16#0000#),
    PC => 16#0000#
  );

end CPU;
