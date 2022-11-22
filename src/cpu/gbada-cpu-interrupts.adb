with Gbada.Memory;
with Gbada.Types;

package body Gbada.CPU.Interrupts is

  procedure Handle_Interrupts is
    type Int_Flag_Register (Raw : Boolean := False) is record
      case Raw is
        when True =>
          Register : UInt8;
        when False =>
          VBlank : Bit;
          LCD_STAT : Bit;
          Timer : Bit;
          Serial : Bit;
          Joypad : Bit;
      end case;
    end record
      with Unchecked_Union;

    for Int_Flag_Register use record
      Register at 0 range 0..7;
      VBlank at 0 range 0..0;
      LCD_STAT at 0 range 1..1;
      Timer at 0 range 2..2;
      Serial at 0 range 3..3;
      Joypad at 0 range 4..4;
    end record;

    Flags : Int_Flag_Register := (True, ReadByte (Interrupt_Flag));  

    SVR : Time;
    SVR_Time : Time_Span := 5 * M_Cycle;

  begin
    if not IME then
      return;
    end if;

    SVR := Clock;
    if Flags.VBlank = 2#1# then
      IME := False;
      Flags.VBlank := 2#0#;
      WriteDouble (Reg.SP.Val, Reg.PC);
      Reg.SP.Val := Reg.SP.Val - 2;
      Reg.PC := Interrupt_VBlank;
      delay until SVR + SVR_Time;
    elsif Flags.LCD_STAT = 2#1# then
      IME := False;
      Flags.LCD_STAT := 2#0#;
      WriteDouble (Reg.SP.Val, Reg.PC);
      Reg.SP.Val := Reg.SP.Val - 2;
      Reg.PC := Interrupt_LCD_STAT;
      delay until SVR + SVR_Time;
    elsif Flags.Timer = 2#1# then
      IME := False;
      Flags.Timer := 2#0#;
      WriteDouble (Reg.SP.Val, Reg.PC);
      Reg.SP.Val := Reg.SP.Val - 2;
      Reg.PC := Interrupt_Timer;
      delay until SVR + SVR_Time;
    elsif Flags.Serial = 2#1# then
      IME := False;
      Flags.Serial := 2#0#;
      WriteDouble (Reg.SP.Val, Reg.PC);
      Reg.SP.Val := Reg.SP.Val - 2;
      Reg.PC := Interrupt_Serial;
      delay until SVR + SVR_Time;
    elsif Flags.Joypad = 2#1# then
      IME := False;
      Flags.Joypad := 2#0#;
      WriteDouble (Reg.SP.Val, Reg.PC);
      Reg.SP.Val := Reg.SP.Val - 2;
      Reg.PC := Interrupt_Joypad;
      delay until SVR + SVR_Time;
    end if;
  end Handle_Interrupts;

end Gbada.CPU.Interrupts;
