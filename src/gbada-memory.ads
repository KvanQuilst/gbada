with Gbada.Types; use Gbada.Types;

package Gbada.Memory is

  -- Types --
  subtype Address is UInt16 range 0 .. 16#FFFF#;
  type MemorySpace is array (Address) of UInt8;

  type MemoryLocation is (
      RST0, RST8, RST10, RST18, RST20, RST28, RST30, RST38,
      Interrupt_EN
    );

  for MemoryLocation use (
      RST0  => 16#0000#,
      RST8  => 16#0008#,
      RST10 => 16#0010#,
      RST18 => 16#0018#,
      RST20 => 16#0020#,
      RST28 => 16#0028#,
      RST30 => 16#0030#,
      RST38 => 16#0038#,
      Interrupt_EN => 16#FFFF#
    );

  Interrupt_VBLank   : constant Address := 16#0040#;
  Interrupt_LCD_STAT : constant Address := 16#0048#;
  Interrupt_Timer    : constant Address := 16#0050#;
  Interrupt_Serial   : constant Address := 16#0058#;
  Interrupt_Joypad   : constant Address := 16#0060#;
  Interrupt_Flag     : constant Address := 16#FF0F#;

  -- Subprograms --

  -- Read a byte from the specified location
  function ReadByte (Addr : Address) return UInt8;
  function ReadByte (Loc : MemoryLocation) return UInt8;

  -- Read two bytes from the specified location
  function ReadDouble (Addr : Address) return UInt16;
  function ReadDouble (Loc : MemoryLocation) return UInt16;

  -- Write a byte to the specified location
  procedure WriteByte (Addr : Address; 
                       Val : UInt8);
  procedure WriteByte (Loc : MemoryLocation;
                       Val : UInt8);

  -- Write two bytes to the specified location
  procedure WriteDouble (Addr : Address; 
                         Val : UInt16);
  procedure WriteDouble (Loc : MemoryLocation;
                         Val : UInt16);

private

  MemMap : MemorySpace;
  Invalid_Memory_Reference_Exception : exception;

end Gbada.Memory;
