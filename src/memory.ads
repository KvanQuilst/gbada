with Ada.Sequential_IO;

with Types; use Types;

package Memory is

   procedure Read_Cart (File_Name : String);

   procedure Write_Byte (Item : UInt8; Addr : Address);
   function  Read_Byte  (Addr : Address) return UInt8;

   procedure Write_Double (Item : UInt16; Addr : Address);
   function Read_Double (Addr : Address) return UInt16;

private

   Mem_Map : array (Address'Range) of UInt8;

   package UInt8_Seq_IO is new Ada.Sequential_IO (UInt8); use UInt8_Seq_IO;

   Invalid_Memory_Reference_Exception : exception;

end Memory;
