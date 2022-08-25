unit FFTypes;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  // Must be at the top...
  {$IF CompilerVersion >= 24.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

type
  PPPByte = ^PPByte;
  PPByte = ^PByte;
  ByteArray  = array[0..$effffff] of Byte;
  PByteArray = ^ByteArray;

{$IFNDEF FPC}
{$IF CompilerVersion <= 18.5} // Delphi 2007 and olders
  // Delphi 6 undefined
  // Delphi 7 to Delphi 2007 defined incorrectly, SizeOf(NativeInt) = 8, thus re-define it
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$IFEND}
{$IF CompilerVersion <= 21.0} // Delphi 2010 and olders
  PNativeInt = ^NativeInt;
{$IFEND}
{$ENDIF}

  PSize_t = ^Size_t;
{$IF Defined(WIN64) Or Defined(MACOS64)}
  Size_t = NativeUInt;
{$ELSE}
  Size_t = Cardinal;
{$IFEND}
{$EXTERNALSYM SIZE_T}

{$IFDEF BCB}
  PCardinal = ^Cardinal;
  PInt64 = ^Int64;
  PSingle = ^Single;
  PPWideChar = ^PWideChar;
  PWord = ^Word;
{$IFDEF VER140} // C++Builder 6
  PPointer = ^Pointer;
{$ENDIF}
{$ENDIF}

const
  CharUp_A   = 'A';
  CharUp_P   = 'P';
  CharUp_I   = 'I';
  CharUp_C   = 'C';
  CharUp_Y   = 'Y';
  CharUp_U   = 'U';
  CharUp_V   = 'V';
  Char_g     = 'g';
  Char_s     = 's';
  Char_c     = 'c';
  Char_p     = 'p';
  Char_a     = 'a';
  Char_v     = 'v';
  Char_0     = #0;
  Char_LF    = #10;
  Char_colon = ':';
  Char_comma = ',';
  Char_equal = '=';
  Char_minus = '-';
  Char_question = '?';
  Char_slash = '/';
  Char_back_slash = '\';

implementation

end.
