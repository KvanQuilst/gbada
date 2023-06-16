with Types; use Types;

package Hex_IO is

   procedure Put (Item : UInt8; Width : Positive := 4);
   procedure Put (Item : UInt16; Width : Positive := 6);
   function Hex_Image (Item : UInt8) return String;
   function Hex_Image (Item : UInt16) return String;

end Hex_IO;
