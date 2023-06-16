with Ada.Text_IO; use Ada.Text_IO;

package body Hex_IO is

   -- TODO: Abstract hex conversion --

   procedure Put (Item : UInt8; Width : Positive := 4) is
      Idx, Off : Positive := 1;
      Val, Remainder : UInt8;
      Output : String (1 .. Width);
   begin
      if Width < 4 then
         return;
      elsif Width > 4 then
         Off := Width - 3;
         for I in 1 .. Off loop
            Output (I) := ' ';
         end loop;
      end if;

      Output (Off)     := '0';
      Output (Off + 1) := 'x';

      Val := Item;
      Idx := Width;
      while Idx > Off + 1 loop
         Remainder := Val mod 16;
         case Remainder is
            when 10 => Output (Idx) := 'A';
            when 11 => Output (Idx) := 'B';
            when 12 => Output (Idx) := 'C';
            when 13 => Output (Idx) := 'D';
            when 14 => Output (Idx) := 'E';
            when 15 => Output (Idx) := 'F';
            when others =>
               Output (Idx) := Character'Val (Remainder + Character'Pos ('0'));
         end case;
         Idx := Idx - 1;
         Val := Val / 16;
      end loop;
      Put (Output);
   end Put;

   procedure Put (Item : UInt16; Width : Positive := 6) is
      Idx, Off : Positive := 1;
      Val, Remainder : UInt16;
      Output : String (1 .. Width);
   begin
      if Width < 6 then
         return;
      elsif Width > 6 then
         Off := Width - 5;
         for I in 1 .. Off loop
            Output (I) := ' ';
         end loop;
      end if;

      Output (Off)     := '0';
      Output (Off + 1) := 'x';

      Val := Item;
      Idx := Width;
      while Idx > Off + 1 loop
         Remainder := Val mod 16;
         case Remainder is
            when 10 => Output (Idx) := 'A';
            when 11 => Output (Idx) := 'B';
            when 12 => Output (Idx) := 'C';
            when 13 => Output (Idx) := 'D';
            when 14 => Output (Idx) := 'E';
            when 15 => Output (Idx) := 'F';
            when others =>
               Output (Idx) := Character'Val (Remainder + Character'Pos ('0'));
         end case;
         Idx := Idx - 1;
         Val := Val / 16;
      end loop;
      Put (Output);
   end Put;

   function Hex_Image (Item : UInt8) return String is
      Idx : Positive := 4;
      Val, Remainder : UInt8;
      Hex_Str : String := "0xXX";
   begin
      Val := Item;
      while Idx > 2 loop
         Remainder := Val mod 16;
         case Remainder is
            when 10 => Hex_Str (Idx) := 'A';
            when 11 => Hex_Str (Idx) := 'B';
            when 12 => Hex_Str (Idx) := 'C';
            when 13 => Hex_Str (Idx) := 'D';
            when 14 => Hex_Str (Idx) := 'E';
            when 15 => Hex_Str (Idx) := 'F';
            when others =>
               Hex_Str (Idx) :=
                  Character'Val (Remainder + Character'Pos ('0'));
         end case;
         Idx := Idx - 1;
         Val := Val / 16;
      end loop;

      return Hex_Str;
   end Hex_Image;

   function Hex_Image (Item : UInt16) return String is
      Idx : Positive := 6;
      Val, Remainder : UInt16;
      Hex_Str : String := "0xXXXX";
   begin
      Val := Item;
      while Idx > 2 loop
         Remainder := Val mod 16;
         case Remainder is
            when 10 => Hex_Str (Idx) := 'A';
            when 11 => Hex_Str (Idx) := 'B';
            when 12 => Hex_Str (Idx) := 'C';
            when 13 => Hex_Str (Idx) := 'D';
            when 14 => Hex_Str (Idx) := 'E';
            when 15 => Hex_Str (Idx) := 'F';
            when others =>
               Hex_Str (Idx) :=
                  Character'Val (Remainder + Character'Pos ('0'));
         end case;
         Idx := Idx - 1;
         Val := Val / 16;
      end loop;

      return Hex_Str;
   end Hex_Image;

end Hex_IO;
