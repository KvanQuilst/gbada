with GBADA.Types; use GBADA.Types;
with Memory; use Memory;

package CPU is

  type Register (Flag : Boolean := True) is 
    record
      case Flag is 
        when True =>
          Reserved_0_4 : UInt4 := 16#0#;
          Carry_Flag : Bit := 2#0#;
          HalfCarry_Flag : Bit := 2#0#;
          Subtraction_Flag : Bit := 2#0#;
          Zero_Flag : Bit := 2#0#;
        when False =>
          Val : UInt8 := 16#00#;
      end case;
  end record
    with Unchecked_Union;

  type Paired_Register (Raw : Boolean := False) is
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

  type Registers (Paired : Boolean := False) is 
    record
      case Paired is
        when True =>
          AF : Paired_Register;
          BC : Paired_Register;
          DE : Paired_Register;
          HL : Paired_Register;
        when False =>
          A : Register;
          F : Register;
          B : Register;
          C : Register;
          D : Register;
          E : Register;
          H : Register;
          L : Register;
      end case;
    end record
      with Unchecked_Union;

  Reg : Registers := (Paired => False,
    A => (False, 16#00#),
    F => (False, 16#00#),
    B => (False, 16#00#),
    C => (False, 16#00#),
    D => (False, 16#00#),
    E => (False, 16#00#),
    H => (False, 16#00#),
    L => (False, 16#00#)
  );

  Reg_SP : Address := 16#00#;
  Reg_PC : Address := 16#00#;

end CPU;
