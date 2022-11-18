package body Gbada.Memory is

  -- Validate Address
  -- Validate a provded address and convert it to an equivalent or invalid address
  -- 
  -- Addr : The address to validate
  -- Return : A validated address
  function ValidateAddress (Addr : Address) return Address is
  begin
    case Addr is
      when 16#E000# .. 16#FDFF# =>
        return Addr - 16#2000#;
      when 16#FEA0# .. 16#FEFF# =>
        raise Invalid_Memory_Reference_Exception 
          with "Memory address referenced is invalid! (" &
               Addr'Image &
               ")";
      when others =>
        return Addr;
    end case;
  end ValidateAddress;

  -- ReadByte
  -- Read a byte from the provided address
  --
  -- Addr : The address to read from
  -- Return : The data at the provided Addr
  function ReadByte (Addr : Address) return UInt8 is
    Val_Addr : Address := ValidateAddress (Addr);
  begin 
    return MemMap (Val_Addr);
  end ReadByte;

  -- ReadByte
  -- Read a byte from the provided pre-defined location
  --
  -- Loc : The pre-defined location
  -- Return : The data at the provided Loc
  function ReadByte (Loc : MemoryLocation) return UInt8 is
  begin
    return MemMap (MemoryLocation'Enum_Rep (Loc));
  end ReadByte;

  -- ReadDouble
  -- Read two bytes from the provided address
  --
  -- Addr : The address to read from
  -- Return : The data at the provided Addr
  function ReadDouble (Addr : Address) return UInt16 is
    Val_Addr : Address := ValidateAddress (Addr);
    Data : UInt16_Split := (False,
                            (MemMap (Val_Addr + 1)),
                            (MemMap (Val_Addr)));
  begin
    return Data.Val;
  end ReadDouble;

  -- ReadDouble
  -- Read two bytes from the provided location
  --
  -- Loc : The pre-defined lcoation to read from
  -- Return : The data at the provided location
  function ReadDouble (Loc : MemoryLocation) return UInt16 is
    Data : UInt16_Split := (False,
                            (MemMap (MemoryLocation'Enum_Rep (Loc) + 1)),
                            (MemMap (MemoryLocation'Enum_Rep (Loc))));
  begin
    return Data.Val;
  end ReadDouble;

  -- WriteByte
  -- Write a byte to the specified address
  --
  -- Addr : The address to write to
  -- Val : The value to write
  procedure WriteByte (Addr : Address; Val : UInt8) is
    Val_Addr : Address := ValidateAddress (Addr);
  begin
    MemMap (Val_Addr) := Val;
  end WriteByte;

  -- WriteByte
  -- Write a byte to the specified address
  --
  -- Loc : The pre-defined location to write to
  -- Val : The value to write
  procedure WriteByte (Loc : MemoryLocation; Val : UInt8) is
  begin
    MemMap (MemoryLocation'Enum_Rep (Loc)) := Val;
  end WriteByte;

  -- WriteDouble
  -- Write two bytes to the specified address
  --
  -- Addr : The address to write to
  -- Val : The value to write
  procedure WriteDouble (Addr : Address; Val : UInt16) is
    Val_Addr : Address := ValidateAddress (Addr);
    Data : UInt16_Split := (True, Val);
  begin
    MemMap (Val_Addr + 1) := Data.Upper;
    MemMap (Val_Addr) := Data.Lower;
  end WriteDouble;

  -- WriteDouble
  -- Write two bytes to the specified address
  --
  -- Loc : The pre-defined location to write to
  -- Val : The value to write
  procedure WriteDouble (Loc : MemoryLocation; Val : UInt16) is
    Data : UInt16_Split := (True, Val); 
  begin
    MemMap (MemoryLocation'Enum_Rep (Loc) + 1) := Data.Upper;
    MemMap (MemoryLocation'Enum_Rep (Loc)) := Data.Lower;
  end WriteDouble;

end Gbada.Memory;
