with Ada.Text_IO;

package Types is

   type Bit    is mod 2**1;
   type UInt4  is mod 2**4;
   type UInt8  is mod 2**8;
   type UInt16 is mod 2**16;

   type UInt16_Split (Split : Boolean := True) is record
     case Split is
        when True =>
           Upper : UInt8;
           Lower : UInt8;
        when False =>
           Full  : UInt16;
     end case;
   end record
      with Unchecked_Union;

   for UInt16_Split use record
      Upper at 1 range 0 .. 7;
      Lower at 0 range 0 .. 7;
      Full  at 0 range 0 .. 15;
   end record;

   subtype Address is UInt16;

   package UInt8_IO  is new Ada.Text_IO.Modular_IO (UInt8);
   package UInt16_IO is new Ada.Text_IO.Modular_IO (UInt16);

end Types;
