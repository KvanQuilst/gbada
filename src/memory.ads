with GBADA.Types; use GBADA.Types;

package Memory is

  -- Types --
  subtype Address is UInt16 range 0 .. 16#FFFF#;
  type MemorySpace is array (Address) of UInt8;

  type MemoryLocation is (
      RST0, RST8, RST10, RST18, RST20, RST28, RST30, RST38,
      INT_VBlank, INT_Stat, INT_Timer, INT_Serial, INT_JoyPad
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
      INT_VBlank  => 16#0040#, 
      INT_Stat    => 16#0048#,
      INT_Timer   => 16#0050#,
      INT_Serial  => 16#0058#,
      INT_JoyPad  => 16#0060#
    );

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

  -- Declarations --
  MemMap : MemorySpace;

private

  function ValidateAddress (Addr : Address) return Address;

  Invalid_Memory_Reference_Exception : exception;

end Memory;
