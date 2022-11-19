with Ada.Text_IO;

package Gbada.Types is

  type Bit is mod 2**1;
  type UInt4 is mod 2**4;
  type UInt8 is mod 2**8;
  type UInt16 is mod 2**16;

  type UInt16_Split (Raw : Boolean := True) is
    record
      case Raw is
        when True =>
          Val : UInt16;
        when False =>
          Upper : UInt8;
          Lower : UInt8;
      end case;
    end record
      with Unchecked_Union;

  type ByteArr is array (Positive range <>) of UInt8;

  package UInt8_IO is new Ada.Text_IO.Modular_IO (UInt8);
  package UInt16_IO is new Ada.Text_IO.Modular_IO (UInt16);

end Gbada.Types;
