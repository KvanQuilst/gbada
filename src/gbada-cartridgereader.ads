with Ada.Sequential_IO;
with Gbada.Types; use Gbada.Types;

package Gbada.CartridgeReader is
  package UInt8_SIO is new Ada.Sequential_IO (UInt8); use UInt8_SIO;
  
  procedure ReadRom (File_Name : String);
end Gbada.CartridgeReader;
