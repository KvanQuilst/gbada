with Ada.Text_IO;

with Gbada.Memory; use Gbada.Memory;
with Gbada.Types; use Gbada.Types;

package body Gbada.CartridgeReader is

  procedure ReadRom (File_Name : String) is
    F : File_Type;
    I : Address := 0;
    Val : UInt8;
  begin
    Open (F, In_File, File_Name);

    while not End_Of_File (F) loop

      -- Read/write the next byte from the rom file --
      Read (F, Val);
      WriteByte (I, Val);

      case I is
        ---------------------
        -- Print the Title --
        ---------------------
        when 16#0134# => Ada.Text_IO.Put ("Game Title: " & Character'Val (Val));
        when 16#0135#..16#0141# => Ada.Text_IO.Put (Character'Val (Val));
        when 16#0142# => Ada.Text_IO.Put_Line (Character'Val (Val) & "");

        when others => null;
      end case;
      
      I := I + 1;
    end loop;

    Close (F);
  end;

end Gbada.CartridgeReader;
