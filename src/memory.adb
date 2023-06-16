package body Memory is

   procedure Read_Cart (File_Name : String) is
      F : File_Type;
      I : Address := 0;
      Val : UInt8;
   begin
      Open (F, In_File, File_Name);

      -- TODO: Write magic number and filetype check --

      while not End_Of_File (F) loop
         Read (F, Val);
         Write_Byte (Val, I);
         I := I + 1;
      end loop;

      Close (F);
   end Read_Cart;

   procedure Write_Byte (Item : UInt8; Addr : Address) is
   begin
      Mem_Map (Addr) := Item;
   end Write_Byte;

   function Read_Byte (Addr : Address) return UInt8 is
   begin
      return Mem_Map (Addr);
   end Read_Byte;

   function Read_Double (Addr : Address) return UInt16 is
      Double : constant UInt16_Split :=
         (True, Mem_Map (Addr + 1), Mem_Map (Addr));
   begin
      return Double.Full;
   end Read_Double;

end Memory;
