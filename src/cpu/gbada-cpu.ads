with Ada.Real_Time; use Ada.Real_Time;
with Gbada.Types; use Gbada.Types;
with Gbada.Memory; use Gbada.Memory;

package Gbada.CPU is

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
    Lower at 0 range 0..7;
    Upper at 1 range 0..7;
  end record;

  type Registers (Paired : Boolean := False) is 
    record
      SP : Register16;
      PC : Address;
      case Paired is
        when True =>
          AF : Register16;
          BC : Register16;
          DE : Register16;
          HL : Register16;
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

  for Registers use record
    AF at 16#0# range 0..15;
    A  at 16#1# range 0..7;
    F  at 16#0# range 0..7;
    BC at 16#2# range 0..15;
    B  at 16#3# range 0..7;
    C  at 16#2# range 0..7;
    DE at 16#4# range 0..15;
    D  at 16#5# range 0..7;
    E  at 16#4# range 0..7;
    HL at 16#6# range 0..15;
    H  at 16#7# range 0..7;
    L  at 16#6# range 0..7;
    SP at 16#8# range 0..15;
    PC at 16#10# range 0..15;
  end record;

  Reg : Registers := (Paired => True,
    AF => (True, 16#01B0#),
    BC => (True, 16#0013#),
    DE => (True, 16#00D8#),
    HL => (True, 16#014D#),
    SP => (True, 16#FFFF#),
    PC => 16#0150#
  );

  type Register_Name is (A, B, C, D, E, F, H, L, AF, BC, DE, HL, SP, PC);

  procedure Start_Clock;
  procedure Print_Register (R : Register_Name); 
  procedure Print_Registers (Header : Boolean);

  private

    Instr_Start : Time;
    Instr_Delay : Time_Span;
    M_Cycle : constant Time_Span := Nanoseconds (4 * 238);

    IME : Boolean := True;

end Gbada.CPU;
