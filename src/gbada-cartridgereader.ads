with Ada.Sequential_IO;
with Gbada.Types; use Gbada.Types;

package Gbada.CartridgeReader is
  package UInt8_IO is new Ada.Sequential_IO (UInt8); use UInt8_IO;
  
  procedure ReadRom (File : File_Type);
end Gbada.CartridgeReader;
