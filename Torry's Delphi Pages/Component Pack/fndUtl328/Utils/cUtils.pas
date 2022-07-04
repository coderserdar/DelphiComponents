{$INCLUDE ..\cDefines.inc}
unit cUtils;

{                                                                              }
{                    Miscellaneous utility functions v3.36                     }
{                                                                              }
{             This unit is copyright © 2000-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                     Its original file name is cUtils.pas                     }
{                      It was generated 1 Aug 2004 23:29.                      }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   2000/02/02  0.01  Initial version                                          }
{   2000/03/08  1.02  Added RealArray / IntegerArray functions.                }
{   2000/04/10  1.03  Added Append, Renamed Delete to Remove and added         }
{                     StringArrays.                                            }
{   2000/05/03  1.04  Added Path functions.                                    }
{   2000/05/08  1.05  Revision.                                                }
{                     188 lines interface. 1171 lines implementation.          }
{   2000/06/01  1.06  Added Range and Dup constructors for dynamic arrays.     }
{   2000/06/03  1.07  Added ArrayInsert functions.                             }
{   2000/06/06  1.08  Moved bit functions from cMaths.                         }
{   2000/06/08  1.09  Removed TInteger, TReal, TRealArray, TIntegerArray.      }
{                     299 lines interface. 2019 lines implementations.         }
{   2000/06/10  1.10  Added linked lists for Integer, Int64, Extended and      }
{                     String.                                                  }
{                     518 lines interface. 3396 lines implementation.          }
{   2000/06/14  1.11  cUtils now generated from a template using a source      }
{                     pre-processor that uses cUtils.                          }
{                     560 lines interface. 1328 lines implementation.          }
{                     Produced source: 644 lines interface, 4716 lines         }
{                     implementation.                                          }
{   2000/07/04  1.12  Revision for Fundamentals release.                       }
{   2000/07/24  1.13  Added TrimArray functions.                               }
{   2000/07/26  1.14  Added Difference functions.                              }
{   2000/09/02  1.15  Added RemoveDuplicates functions.                        }
{                     Added Count functions.                                   }
{                     Fixed bug in Sort.                                       }
{   2000/09/27  1.16  Fixed bug in ArrayInsert.                                }
{   2000/11/29  1.17  Moved SetFPUPrecision to cSysUtils.                      }
{   2001/05/03  1.18  Improved bit functions. Added Pascal versions of         }
{                     assembly routines.                                       }
{                     Templ: 867 lines interface, 2886 lines implementation.   }
{                     Source: 939 lines interface, 9796 lines implementation.  }
{   2001/05/13  1.19  Added CharCount.                                         }
{   2001/05/15  1.20  Added PosNext (ClassType, ObjectArray).                  }
{   2001/05/18  1.21  Added hashing functions from cMaths.                     }
{   2001/07/07  1.22  Added TBinaryTreeNode.                                   }
{   2001/11/11  2.23  Revision.                                                }
{   2002/01/03  2.24  Moved EncodeBase64, DecodeBase64 from cMaths and         }
{                     optimized. Added LongWordToHex, HexToLongWord.           }
{   2002/03/30  2.25  Fixed bug in DecodeBase64.                               }
{   2002/04/02  2.26  Removed dependencies on all other units (incl. Delphi    )
{                     units) to remove initialization code associated with     }
{                     SysUtils. This allows usage of cUtils in projects        }
{                     and still have very small binaries.                      }
{                     Fixed bug in LongWordToHex.                              }
{   2002/05/31  3.27  Refactored for Fundamentals 3.                           }
{                     Moved linked lists to cLinkedLists.                      }
{   2002/08/09  3.28  Added HashInteger.                                       }
{   2002/10/06  3.29  Renamed Cond to iif.                                     }
{   2002/12/12  3.30  Small revisions.                                         }
{   2003/03/14  3.31  Removed ApproxZero. Added FloatZero, FloatsEqual and     }
{                     FloatsCompare. Added documentation and test cases for    }
{                     comparison functions.                                    }
{                     Added support for Currency type.                         }
{   2003/07/27  3.32  Added fast ZeroMem and FillMem routines.                 }
{   2003/09/11  3.33  Added InterfaceArray functions.                          }
{   2004/01/18  3.34  Added WideStringArray functions.                         }
{   2004/07/24  3.35  Optimizations of Sort functions.                         }
{   2005/08/01  3.36  Improved validation in base conversion routines.         }
{                                                                              }
interface

const
  UnitName      = 'cUtils';
  UnitVersion   = '3.36';
  UnitDesc      = 'Miscelleanous utility functions';
  UnitCopyright = 'Copyright (c) 2000-2004 David J Butler';

  FundamentalsMajorVersion = 3;
  FundamentalsMinorVersion = 28;



{$WRITEABLECONST OFF}

{                                                                              }
{ Integer types                                                                }
{   Byte      unsigned 8 bits                                                  }
{   Word      unsigned 16 bits                                                 }
{   LongWord  unsigned 32 bits                                                 }
{   ShortInt  signed 8 bits                                                    }
{   SmallInt  signed 16 bits                                                   }
{   LongInt   signed 32 bits                                                   }
{   Int64     signed 64 bits                                                   }
{   Integer   signed system word                                               }
{   Cardinal  unsigned system word                                             }
{                                                                              }
type
  Int8      = ShortInt;
  Int16     = SmallInt;
  Int32     = LongInt;
  LargeInt  = Int64;
  PLargeInt = ^LargeInt;
  Word8     = Byte;
  Word16    = Word;
  Word32    = LongWord;

  {$IFDEF DELPHI5_DOWN}
  PBoolean  = ^Boolean;
  PByte     = ^Byte;
  PWord     = ^Word;
  PLongWord = ^LongWord;
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PLongInt  = ^LongInt;
  PInteger  = ^Integer;
  PInt64    = ^Int64;
  {$ENDIF}

  LongIntRec = packed record
    case Integer of
      0 : (Lo, Hi : Word);
      1 : (Words  : Array[0..1] of Word);
      2 : (Bytes  : Array[0..3] of Byte);
  end;

const
  MinByte     = Low(Byte);
  MaxByte     = High(Byte);
  MinWord     = Low(Word);
  MaxWord     = High(Word);
  MinShortInt = Low(ShortInt);
  MaxShortInt = High(ShortInt);
  MinSmallInt = Low(SmallInt);
  MaxSmallInt = High(SmallInt);
  MinLongWord = LongWord(Low(LongWord));
  MaxLongWord = LongWord(High(LongWord));
  MinLongInt  = LongInt(Low(LongInt));
  MaxLongInt  = LongInt(High(LongInt));
  MaxInt64    = Int64(High(Int64));
  MinInt64    = Int64(Low(Int64));
  MinInteger  = Integer(Low(Integer));
  MaxInteger  = Integer(High(Integer));
  MinCardinal = Cardinal(Low(Cardinal));
  MaxCardinal = Cardinal(High(Cardinal));

const
  BitsPerByte      = 8;
  BitsPerWord      = 16;
  BitsPerLongWord  = 32;
  BytesPerCardinal = Sizeof(Cardinal);
  BitsPerCardinal  = BytesPerCardinal * 8;

{ Min returns smallest of A and B                                              }
{ Max returns greatest of A and B                                              }
function  MinI(const A, B: Integer): Integer;
function  MaxI(const A, B: Integer): Integer;
function  MinC(const A, B: Cardinal): Cardinal;
function  MaxC(const A, B: Cardinal): Cardinal;

{ Clip returns Value if in Low..High range, otherwise Low or High              }
function  Clip(const Value: Integer; const Low, High: Integer): Integer;
function  ClipByte(const Value: Integer): Integer;
function  ClipWord(const Value: Integer): Integer;
function  ClipLongWord(const Value: Int64): LongWord;
function  SumClipI(const A, I: Integer): Integer;
function  SumClipC(const A: Cardinal; const I: Integer): Cardinal;

{ InXXXRange returns True if A in range of type XXX                            }
function  InByteRange(const A: Int64): Boolean;
function  InWordRange(const A: Int64): Boolean;
function  InLongWordRange(const A: Int64): Boolean;
function  InShortIntRange(const A: Int64): Boolean;
function  InSmallIntRange(const A: Int64): Boolean;
function  InLongIntRange(const A: Int64): Boolean;



{                                                                              }
{ Real types                                                                   }
{                                                                              }
{   Floating point:                                                            }
{     Single    32 bits  7-8 significant digits                                }
{     Double    64 bits  15-16 significant digits                              }
{     Extended  80 bits  19-20 significant digits                              }
{                                                                              }
{   Fixed point:                                                               }
{     Currency  64 bits  19-20 significant digits, 4 after the decimal point.  }
{                                                                              }
type
  Float  = Extended;
  PFloat = ^Float;

const
  MinSingle   : Single   = 1.5E-45;
  MaxSingle   : Single   = 3.4E+38;
  MinDouble   : Double   = 5.0E-324;
  MaxDouble   : Double   = 1.7E+308;
  MinExtended : Extended = 3.4E-4932;
  MaxExtended : Extended = 1.1E+4932;
  {$IFDEF FREEPASCAL}
  MinCurrency = -922337203685477.5807;
  MaxCurrency = 922337203685477.5807;
  {$ELSE}
  MinCurrency : Currency = -922337203685477.5807;
  MaxCurrency : Currency = 922337203685477.5807;
  {$ENDIF}

{$IFDEF DELPHI5_DOWN}
type
  PSingle   = ^Single;
  PDouble   = ^Double;
  PExtended = ^Extended;
  PCurrency = ^Currency;
{$ENDIF}

type
  TExtended = packed record
    Case Boolean of
      True: (
        Mantissa : packed Array[0..1] of LongWord; { MSB of [1] is the normalized 1 bit }
        Exponent : Word; { MSB is the sign bit }
      );
      False: (Value: Extended);
  end;

const
  ExtendedNan      : TExtended = (Mantissa:($FFFFFFFF, $FFFFFFFF); Exponent:$7FFF);
  ExtendedInfinity : TExtended = (Mantissa:($00000000, $80000000); Exponent:$7FFF);

{ Min returns smallest of A and B                                              }
{ Max returns greatest of A and B                                              }
{ Clip returns Value if in Low..High range, otherwise Low or High              }
function  MinF(const A, B: Extended): Extended;
function  MaxF(const A, B: Extended): Extended;
function  ClipF(const Value: Extended; const Low, High: Extended): Extended;

{ InXXXRange returns True if A in range of type XXX                            }
function  InSingleRange(const A: Extended): Boolean;
function  InDoubleRange(const A: Extended): Boolean;
function  InCurrencyRange(const A: Extended): Boolean; overload;
function  InCurrencyRange(const A: Int64): Boolean; overload;



{                                                                              }
{ Bit functions                                                                }
{   All bit functions operate on 32-bit values (LongWord).                     }
{                                                                              }
function  ClearBit(const Value, BitIndex: LongWord): LongWord;
function  SetBit(const Value, BitIndex: LongWord): LongWord;
function  IsBitSet(const Value, BitIndex: LongWord): Boolean;
function  ToggleBit(const Value, BitIndex: LongWord): LongWord;
function  IsHighBitSet(const Value: LongWord): Boolean;

function  SetBitScanForward(const Value: LongWord): Integer; overload;
function  SetBitScanForward(const Value, BitIndex: LongWord): Integer; overload;
function  SetBitScanReverse(const Value: LongWord): Integer; overload;
function  SetBitScanReverse(const Value, BitIndex: LongWord): Integer; overload;
function  ClearBitScanForward(const Value: LongWord): Integer; overload;
function  ClearBitScanForward(const Value, BitIndex: LongWord): Integer; overload;
function  ClearBitScanReverse(const Value: LongWord): Integer; overload;
function  ClearBitScanReverse(const Value, BitIndex: LongWord): Integer; overload;

function  ReverseBits(const Value: LongWord): LongWord; overload;
function  ReverseBits(const Value: LongWord; const BitCount: Integer): LongWord; overload;
function  SwapEndian(const Value: LongWord): LongWord;
Procedure SwapEndianBuf(var Buf; const Count: Integer);
function  TwosComplement(const Value: LongWord): LongWord;

function  RotateLeftBits(const Value: LongWord; const Bits: Byte): LongWord;
function  RotateRightBits(const Value: LongWord; const Bits: Byte): LongWord;

function  BitCount(const Value: LongWord): LongWord;
function  IsPowerOfTwo(const Value: LongWord): Boolean;

function  LowBitMask(const HighBitIndex: LongWord): LongWord;
function  HighBitMask(const LowBitIndex: LongWord): LongWord;
function  RangeBitMask(const LowBitIndex, HighBitIndex: LongWord): LongWord;

function  SetBitRange(const Value: LongWord;
          const LowBitIndex, HighBitIndex: LongWord): LongWord;
function  ClearBitRange(const Value: LongWord;
          const LowBitIndex, HighBitIndex: LongWord): LongWord;
function  ToggleBitRange(const Value: LongWord;
          const LowBitIndex, HighBitIndex: LongWord): LongWord;
function  IsBitRangeSet(const Value: LongWord;
          const LowBitIndex, HighBitIndex: LongWord): Boolean;
function  IsBitRangeClear(const Value: LongWord;
          const LowBitIndex, HighBitIndex: LongWord): Boolean;

const
  BitMaskTable: Array[0..31] of LongWord =
    ($00000001, $00000002, $00000004, $00000008,
     $00000010, $00000020, $00000040, $00000080,
     $00000100, $00000200, $00000400, $00000800,
     $00001000, $00002000, $00004000, $00008000,
     $00010000, $00020000, $00040000, $00080000,
     $00100000, $00200000, $00400000, $00800000,
     $01000000, $02000000, $04000000, $08000000,
     $10000000, $20000000, $40000000, $80000000);



{                                                                              }
{ Sets                                                                         }
{                                                                              }
type
  CharSet = Set of Char;
  ByteSet = Set of Byte;
  PCharSet = ^CharSet;
  PByteSet = ^ByteSet;

const
  CompleteCharSet = [#0..#255];
  CompleteByteSet = [0..255];

function  AsCharSet(const C: Array of Char): CharSet;
function  AsByteSet(const C: Array of Byte): ByteSet;
procedure ComplementChar(var C: CharSet; const Ch: Char);
procedure ClearCharSet(var C: CharSet);
procedure FillCharSet(var C: CharSet);
procedure ComplementCharSet(var C: CharSet);
procedure AssignCharSet(var DestSet: CharSet; const SourceSet: CharSet); overload;
procedure Union(var DestSet: CharSet; const SourceSet: CharSet); overload;
procedure Difference(var DestSet: CharSet; const SourceSet: CharSet); overload;
procedure Intersection(var DestSet: CharSet; const SourceSet: CharSet); overload;
procedure XORCharSet(var DestSet: CharSet; const SourceSet: CharSet);
function  IsSubSet(const A, B: CharSet): Boolean;
function  IsEqual(const A, B: CharSet): Boolean; overload;
function  IsEmpty(const C: CharSet): Boolean;
function  IsComplete(const C: CharSet): Boolean;
function  CharCount(const C: CharSet): Integer; overload;
procedure ConvertCaseInsensitive(var C: CharSet);
function  CaseInsensitiveCharSet(const C: CharSet): CharSet;



{                                                                              }
{ Range functions                                                              }
{                                                                              }
function  IntRangeLength(const Low, High: Integer): Int64;
function  IntRangeAdjacent(const Low1, High1, Low2, High2: Integer): Boolean;
function  IntRangeOverlap(const Low1, High1, Low2, High2: Integer): Boolean;
function  IntRangeHasElement(const Low, High, Element: Integer): Boolean;

function  IntRangeIncludeElement(var Low, High: Integer;
          const Element: Integer): Boolean;
function  IntRangeIncludeElementRange(var Low, High: Integer;
          const LowElement, HighElement: Integer): Boolean;

function  CardinalRangeLength(const Low, High: Cardinal): Int64;
function  CardinalRangeAdjacent(const Low1, High1, Low2, High2: Cardinal): Boolean;
function  CardinalRangeOverlap(const Low1, High1, Low2, High2: Cardinal): Boolean;
function  CardinalRangeHasElement(const Low, High, Element: Cardinal): Boolean;

function  CardinalRangeIncludeElement(var Low, High: Cardinal;
          const Element: Cardinal): Boolean;
function  CardinalRangeIncludeElementRange(var Low, High: Cardinal;
          const LowElement, HighElement: Cardinal): Boolean;



{                                                                              }
{ Swap                                                                         }
{                                                                              }
procedure Swap(var X, Y: Boolean); overload;
procedure Swap(var X, Y: Byte); overload;
procedure Swap(var X, Y: Word); overload;
procedure Swap(var X, Y: LongWord); overload;
procedure Swap(var X, Y: ShortInt); overload;
procedure Swap(var X, Y: SmallInt); overload;
procedure Swap(var X, Y: LongInt); overload;
procedure Swap(var X, Y: Int64); overload;
procedure Swap(var X, Y: Single); overload;
procedure Swap(var X, Y: Double); overload;
procedure Swap(var X, Y: Extended); overload;
procedure Swap(var X, Y: Currency); overload;
procedure Swap(var X, Y: String); overload;
procedure Swap(var X, Y: WideString); overload;
procedure Swap(var X, Y: Pointer); overload;
procedure Swap(var X, Y: TObject); overload;
procedure SwapObjects(var X, Y);



{                                                                              }
{ Inline if                                                                    }
{                                                                              }
{   iif returns TrueValue if Expr is True, otherwise it returns FalseValue.    }
{                                                                              }
function  iif(const Expr: Boolean; const TrueValue: Integer;
          const FalseValue: Integer = 0): Integer; overload;
function  iif(const Expr: Boolean; const TrueValue: Int64;
          const FalseValue: Int64 = 0): Int64; overload;
function  iif(const Expr: Boolean; const TrueValue: Extended;
          const FalseValue: Extended = 0.0): Extended; overload;
function  iif(const Expr: Boolean; const TrueValue: String;
          const FalseValue: String = ''): String; overload;
function  iif(const Expr: Boolean; const TrueValue: TObject;
          const FalseValue: TObject = nil): TObject; overload;



{                                                                              }
{ Comparison                                                                   }
{                                                                              }
type
  TCompareResult = (
      crLess,
      crEqual,
      crGreater,
      crUndefined);
  TCompareResultSet = Set of TCompareResult;

function  ReverseCompareResult(const C: TCompareResult): TCompareResult;



{                                                                              }
{ Direct comparison                                                            }
{                                                                              }
{   Compare(I1, I2) returns crLess if I1 < I2, crEqual if I1 = I2 or           }
{   crGreater if I1 > I2.                                                      }
{                                                                              }
function  Compare(const I1, I2: Boolean): TCompareResult; overload;
function  Compare(const I1, I2: Integer): TCompareResult; overload;
function  Compare(const I1, I2: Int64): TCompareResult; overload;
function  Compare(const I1, I2: Extended): TCompareResult; overload;
function  Compare(const I1, I2: String): TCompareResult; overload;
function  WideCompare(const I1, I2: WideString): TCompareResult;



{                                                                              }
{ Approximate comparison of floating point values                              }
{                                                                              }
{   FloatZero, FloatOne, FloatsEqual and FloatsCompare are functions for       }
{   comparing floating point numbers based on a fixed CompareDelta difference  }
{   between the values. This means that values are considered equal if the     }
{   unsigned difference between the values are less than CompareDelta.         }
{                                                                              }
const
  // Minimum CompareDelta values for the different floating point types:
  // The values were chosen to be slightly higher than the minimum value that
  // the floating-point type can store.
  SingleCompareDelta   = 1.0E-34;
  DoubleCompareDelta   = 1.0E-280;
  ExtendedCompareDelta = 1.0E-4400;

  // Default CompareDelta is set to SingleCompareDelta. This allows any type
  // of floating-point value to be compared with any other.
  DefaultCompareDelta = SingleCompareDelta;

function  FloatZero(const A: Extended;
          const CompareDelta: Extended = DefaultCompareDelta): Boolean;
function  FloatOne(const A: Extended;
          const CompareDelta: Extended = DefaultCompareDelta): Boolean;

function  FloatsEqual(const A, B: Extended;
          const CompareDelta: Extended = DefaultCompareDelta): Boolean;
function  FloatsCompare(const A, B: Extended;
          const CompareDelta: Extended = DefaultCompareDelta): TCompareResult;



{                                                                              }
{ Scaled approximate comparison of floating point values                       }
{                                                                              }
{   ApproxEqual and ApproxCompare are functions for comparing floating point   }
{   numbers based on a scaled order of magnitude difference between the        }
{   values. CompareEpsilon is the ratio applied to the largest of the two      }
{   exponents to give the maximum difference (CompareDelta) for comparison.    }
{                                                                              }
{   For example:                                                               }
{                                                                              }
{   When the CompareEpsilon is 1.0E-9, the result of                           }
{                                                                              }
{   ApproxEqual(1.0E+20, 1.000000001E+20) = False, but the result of           }
{   ApproxEqual(1.0E+20, 1.0000000001E+20) = True, ie the first 9 digits of    }
{   the mantissas of the values must be the same.                              }
{                                                                              }
{   Note that for A <> 0.0, the value of ApproxEqual(A, 0.0) will always be    }
{   False. Rather use the unscaled FloatZero, FloatsEqual and FloatsCompare    }
{   functions when specifically testing for zero.                              }
{                                                                              }
const
  // Smallest (most sensitive) CompareEpsilon values allowed for the different
  // floating point types:
  SingleCompareEpsilon   = 1.0E-5;
  DoubleCompareEpsilon   = 1.0E-13;
  ExtendedCompareEpsilon = 1.0E-17;

  // Default CompareEpsilon is set for half the significant digits of the
  // Extended type.
  DefaultCompareEpsilon  = 1.0E-10;

function  ApproxEqual(const A, B: Extended;
          const CompareEpsilon: Double = DefaultCompareEpsilon): Boolean;
function  ApproxCompare(const A, B: Extended;
          const CompareEpsilon: Double = DefaultCompareEpsilon): TCompareResult;



{                                                                              }
{ Special floating-point values                                                }
{                                                                              }
{   FloatIsInfinity is True if A is a positive or negative infinity.           }
{   FloatIsNaN is True if A is Not-a-Number.                                   }
{                                                                              }
function  FloatIsInfinity(const A: Extended): Boolean;
function  FloatIsNaN(const A: Extended): Boolean;



{                                                                              }
{ Base Conversion                                                              }
{                                                                              }
{   EncodeBase64 converts a binary string (S) to a base 64 string using        }
{   Alphabet. if Pad is True, the result will be padded with PadChar to be a   }
{   multiple of PadMultiple.                                                   }
{                                                                              }
{   DecodeBase64 converts a base 64 string using Alphabet (64 characters for   }
{   values 0-63) to a binary string.                                           }
{                                                                              }
const
  s_HexDigitsUpper: String[16] = '0123456789ABCDEF';
  s_HexDigitsLower: String[16] = '0123456789abcdef';

function  IsHexChar(const Ch: Char): Boolean;
function  IsHexWideChar(const Ch: WideChar): Boolean;
function  HexCharValue(const Ch: Char): Byte;
function  HexWideCharValue(const Ch: WideChar): Byte;

function  LongWordToBin(const I: LongWord; const Digits: Byte = 0): String;
function  LongWordToOct(const I: LongWord; const Digits: Byte = 0): String;
function  LongWordToHex(const I: LongWord; const Digits: Byte = 0;
          const UpperCase: Boolean = True): String;
function  LongWordToStr(const I: LongWord; const Digits: Byte = 0): String;

function  IsValidBinStr(const S: String): Boolean;
function  IsValidOctStr(const S: String): Boolean;
function  IsValidDecStr(const S: String): Boolean;
function  IsValidHexStr(const S: String): Boolean;

{ xxxStrToLongWord converts a number in a specific base to a LongWord value.   }
{ Valid is False on return if the string could not be converted.               }
function  BinStrToLongWord(const S: String; var Valid: Boolean): LongWord;
function  OctStrToLongWord(const S: String; var Valid: Boolean): LongWord;
function  DecStrToLongWord(const S: String; var Valid: Boolean): LongWord;
function  HexStrToLongWord(const S: String; var Valid: Boolean): LongWord;

function  EncodeBase64(const S, Alphabet: String; const Pad: Boolean = False;
          const PadMultiple: Integer = 4; const PadChar: Char = '='): String;
function  DecodeBase64(const S, Alphabet: String; const PadSet: CharSet = []): String;

const
  b64_MIMEBase64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64_UUEncode   = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  b64_XXEncode   = '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

function  MIMEBase64Decode(const S: String): String;
function  MIMEBase64Encode(const S: String): String;
function  UUDecode(const S: String): String;
function  XXDecode(const S: String): String;

function  BytesToHex(const P: Pointer; const Count: Integer;
          const UpperCase: Boolean = True): String;



{                                                                              }
{ Type conversion                                                              }
{                                                                              }
function  PointerToStr(const P: Pointer): String;
function  StrToPointer(const S: String): Pointer;
function  ObjectClassName(const O: TObject): String;
function  ClassClassName(const C: TClass): String;
function  ObjectToStr(const O: TObject): String;
function  ClassToStr(const C: TClass): String;
function  CharSetToStr(const C: CharSet): String;
function  StrToCharSet(const S: String): CharSet;



{                                                                              }
{ Hashing functions                                                            }
{                                                                              }
{   HashBuf uses a every byte in the buffer to calculate a hash.               }
{                                                                              }
{   HashStrBuf/HashStr is a general purpose string hashing function.           }
{   For large strings, HashStr will sample up to 48 bytes from the string.     }
{                                                                              }
{   If Slots = 0 the hash value is in the LongWord range (0-$FFFFFFFF),        }
{   otherwise the value is in the range from 0 to Slots-1. Note that the       }
{   'mod' operation, which is used when Slots <> 0, is comparitively slow.     }
{                                                                              }
function  HashBuf(const Buf; const BufSize: Integer;
          const Slots: LongWord = 0): LongWord;
function  HashStrBuf(const StrBuf: Pointer; const StrLength: Integer;
          const Slots: LongWord = 0): LongWord;
function  HashStrBufNoCase(const StrBuf: Pointer; const StrLength: Integer;
          const Slots: LongWord = 0): LongWord;
function  HashStr(const S: String; const Slots: LongWord = 0;
          const CaseSensitive: Boolean = True): LongWord;
function  HashInteger(const I: Integer; const Slots: LongWord = 0): LongWord;
function  HashLongWord(const I: LongWord; const Slots: LongWord = 0): LongWord;



{                                                                              }
{ Memory operations                                                            }
{                                                                              }
{$IFDEF DELPHI5_DOWN}
type
  PPointer = ^Pointer;
{$ENDIF}

const
  Bytes1KB  = 1024;
  Bytes1MB  = 1024 * Bytes1KB;
  Bytes1GB  = 1024 * Bytes1MB;
  Bytes64KB = 64 * Bytes1KB;
  Bytes64MB = 64 * Bytes1MB;
  Bytes2GB  = 2 * LongWord(Bytes1GB);

procedure FillMem(var Buf; const Count: Integer; const Value: Byte);
procedure ZeroMem(var Buf; const Count: Integer);
procedure MoveMem(const Source; var Dest; const Count: Integer);
function  CompareMem(const Buf1; const Buf2; const Count: Integer): Boolean;
function  CompareMemNoCase(const Buf1; const Buf2; const Count: Integer): TCompareResult;
procedure ReverseMem(var Buf; const Size: Integer);



{                                                                              }
{ IInterface                                                                   }
{                                                                              }
{$IFDEF DELPHI5_DOWN}
type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;
{$ENDIF}



{                                                                              }
{ Array pointers                                                               }
{                                                                              }

{ Maximum array elements                                                       }
const
  MaxArraySize = $7FFFFFFF; // 2 Gigabytes
  MaxByteArrayElements = MaxArraySize div Sizeof(Byte);
  MaxWordArrayElements = MaxArraySize div Sizeof(Word);
  MaxLongWordArrayElements = MaxArraySize div Sizeof(LongWord);
  MaxCardinalArrayElements = MaxArraySize div Sizeof(Cardinal);
  MaxShortIntArrayElements = MaxArraySize div Sizeof(ShortInt);
  MaxSmallIntArrayElements = MaxArraySize div Sizeof(SmallInt);
  MaxLongIntArrayElements = MaxArraySize div Sizeof(LongInt);
  MaxIntegerArrayElements = MaxArraySize div Sizeof(Integer);
  MaxInt64ArrayElements = MaxArraySize div Sizeof(Int64);
  MaxSingleArrayElements = MaxArraySize div Sizeof(Single);
  MaxDoubleArrayElements = MaxArraySize div Sizeof(Double);
  MaxExtendedArrayElements = MaxArraySize div Sizeof(Extended);
  MaxCurrencyArrayElements = MaxArraySize div Sizeof(Currency);
  MaxStringArrayElements = MaxArraySize div Sizeof(String);
  MaxWideStringArrayElements = MaxArraySize div Sizeof(WideString);
  MaxPointerArrayElements = MaxArraySize div Sizeof(Pointer);
  MaxObjectArrayElements = MaxArraySize div Sizeof(TObject);
  MaxInterfaceArrayElements = MaxArraySize div Sizeof(IInterface);
  MaxBooleanArrayElements = MaxArraySize div Sizeof(Boolean);
  MaxCharSetArrayElements = MaxArraySize div Sizeof(CharSet);
  MaxByteSetArrayElements = MaxArraySize div Sizeof(ByteSet);

{ Static array types                                                           }
type
  TStaticByteArray = Array[0..MaxByteArrayElements - 1] of Byte;
  TStaticWordArray = Array[0..MaxWordArrayElements - 1] of Word;
  TStaticLongWordArray = Array[0..MaxLongWordArrayElements - 1] of LongWord;
  TStaticShortIntArray = Array[0..MaxShortIntArrayElements - 1] of ShortInt;
  TStaticSmallIntArray = Array[0..MaxSmallIntArrayElements - 1] of SmallInt;
  TStaticLongIntArray = Array[0..MaxLongIntArrayElements - 1] of LongInt;
  TStaticInt64Array = Array[0..MaxInt64ArrayElements - 1] of Int64;
  TStaticSingleArray = Array[0..MaxSingleArrayElements - 1] of Single;
  TStaticDoubleArray = Array[0..MaxDoubleArrayElements - 1] of Double;
  TStaticExtendedArray = Array[0..MaxExtendedArrayElements - 1] of Extended;
  TStaticCurrencyArray = Array[0..MaxCurrencyArrayElements - 1] of Currency;
  TStaticStringArray = Array[0..MaxStringArrayElements - 1] of String;
  TStaticWideStringArray = Array[0..MaxWideStringArrayElements - 1] of WideString;
  TStaticPointerArray = Array[0..MaxPointerArrayElements - 1] of Pointer;
  TStaticObjectArray = Array[0..MaxObjectArrayElements - 1] of TObject;
  TStaticInterfaceArray = Array[0..MaxInterfaceArrayElements - 1] of IInterface;
  TStaticBooleanArray = Array[0..MaxBooleanArrayElements - 1] of Boolean;
  TStaticCharSetArray = Array[0..MaxCharSetArrayElements - 1] of CharSet;
  TStaticByteSetArray = Array[0..MaxByteSetArrayElements - 1] of ByteSet;
  TStaticCardinalArray = TStaticLongWordArray;
  TStaticIntegerArray = TStaticLongIntArray;

{ Array pointers                                                               }
type
  PByteArray = ^TStaticByteArray;
  PWordArray = ^TStaticWordArray;
  PLongWordArray = ^TStaticLongWordArray;
  PCardinalArray = ^TStaticCardinalArray;
  PShortIntArray = ^TStaticShortIntArray;
  PSmallIntArray = ^TStaticSmallIntArray;
  PLongIntArray = ^TStaticLongIntArray;
  PIntegerArray = ^TStaticIntegerArray;
  PInt64Array = ^TStaticInt64Array;
  PSingleArray = ^TStaticSingleArray;
  PDoubleArray = ^TStaticDoubleArray;
  PExtendedArray = ^TStaticExtendedArray;
  PCurrencyArray = ^TStaticCurrencyArray;
  PStringArray = ^TStaticStringArray;
  PWideStringArray = ^TStaticWideStringArray;
  PPointerArray = ^TStaticPointerArray;
  PObjectArray = ^TStaticObjectArray;
  PInterfaceArray = ^TStaticInterfaceArray;
  PBooleanArray = ^TStaticBooleanArray;
  PCharSetArray = ^TStaticCharSetArray;
  PByteSetArray = ^TStaticByteSetArray;



{                                                                              }
{ Dynamic arrays                                                               }
{                                                                              }
type
  ByteArray = Array of Byte;
  WordArray = Array of Word;
  LongWordArray = Array of LongWord;
  ShortIntArray = Array of ShortInt;
  SmallIntArray = Array of SmallInt;
  LongIntArray = Array of LongInt;
  Int64Array = Array of Int64;
  SingleArray = Array of Single;
  DoubleArray = Array of Double;
  ExtendedArray = Array of Extended;
  CurrencyArray = Array of Currency;
  StringArray = Array of String;
  WideStringArray = Array of WideString;
  PointerArray = Array of Pointer;
  ObjectArray = Array of TObject;
  InterfaceArray = Array of IInterface;
  BooleanArray = Array of Boolean;
  CharSetArray = Array of CharSet;
  ByteSetArray = Array of ByteSet;
  IntegerArray = LongIntArray;
  CardinalArray = LongWordArray;


function  Append(var V: ByteArray; const R: Byte): Integer; overload;
function  Append(var V: WordArray; const R: Word): Integer; overload;
function  Append(var V: LongWordArray; const R: LongWord): Integer; overload;
function  Append(var V: ShortIntArray; const R: ShortInt): Integer; overload;
function  Append(var V: SmallIntArray; const R: SmallInt): Integer; overload;
function  Append(var V: LongIntArray; const R: LongInt): Integer; overload;
function  Append(var V: Int64Array; const R: Int64): Integer; overload;
function  Append(var V: SingleArray; const R: Single): Integer; overload;
function  Append(var V: DoubleArray; const R: Double): Integer; overload;
function  Append(var V: ExtendedArray; const R: Extended): Integer; overload;
function  Append(var V: CurrencyArray; const R: Currency): Integer; overload;
function  Append(var V: StringArray; const R: String): Integer; overload;
function  Append(var V: WideStringArray; const R: WideString): Integer; overload;
function  Append(var V: BooleanArray; const R: Boolean): Integer; overload;
function  Append(var V: PointerArray; const R: Pointer): Integer; overload;
function  Append(var V: ObjectArray; const R: TObject): Integer; overload;
function  Append(var V: InterfaceArray; const R: IInterface): Integer; overload;
function  Append(var V: ByteSetArray; const R: ByteSet): Integer; overload;
function  Append(var V: CharSetArray; const R: CharSet): Integer; overload;
function  AppendByteArray(var V: ByteArray; const R: Array of Byte): Integer; overload;
function  AppendWordArray(var V: WordArray; const R: Array of Word): Integer; overload;
function  AppendCardinalArray(var V: CardinalArray; const R: Array of LongWord): Integer; overload;
function  AppendShortIntArray(var V: ShortIntArray; const R: Array of ShortInt): Integer; overload;
function  AppendSmallIntArray(var V: SmallIntArray; const R: Array of SmallInt): Integer; overload;
function  AppendIntegerArray(var V: IntegerArray; const R: Array of LongInt): Integer; overload;
function  AppendInt64Array(var V: Int64Array; const R: Array of Int64): Integer; overload;
function  AppendSingleArray(var V: SingleArray; const R: Array of Single): Integer; overload;
function  AppendDoubleArray(var V: DoubleArray; const R: Array of Double): Integer; overload;
function  AppendExtendedArray(var V: ExtendedArray; const R: Array of Extended): Integer; overload;
function  AppendCurrencyArray(var V: CurrencyArray; const R: Array of Currency): Integer; overload;
function  AppendStringArray(var V: StringArray; const R: Array of String): Integer; overload;
function  AppendPointerArray(var V: PointerArray; const R: Array of Pointer): Integer; overload;
function  AppendObjectArray(var V: ObjectArray; const R: ObjectArray): Integer; overload;
function  AppendCharSetArray(var V: CharSetArray; const R: Array of CharSet): Integer; overload;
function  AppendByteSetArray(var V: ByteSetArray; const R: Array of ByteSet): Integer; overload;


function  Remove(var V: ByteArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: WordArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: LongWordArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: ShortIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: SmallIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: LongIntArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: Int64Array; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: SingleArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: DoubleArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: ExtendedArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: CurrencyArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: StringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: WideStringArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: PointerArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;
function  Remove(var V: ObjectArray; const Idx: Integer; const Count: Integer = 1;
          const FreeObjects: Boolean = False): Integer; overload;
function  Remove(var V: InterfaceArray; const Idx: Integer; const Count: Integer = 1): Integer; overload;

procedure RemoveDuplicates(var V: ByteArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: WordArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: LongWordArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: ShortIntArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: SmallIntArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: LongIntArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: Int64Array; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: SingleArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: DoubleArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: ExtendedArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: StringArray; const IsSorted: Boolean); overload;
procedure RemoveDuplicates(var V: PointerArray; const IsSorted: Boolean); overload;

procedure TrimArrayLeft(var S: ByteArray; const TrimList: Array of Byte); overload;
procedure TrimArrayLeft(var S: WordArray; const TrimList: Array of Word); overload;
procedure TrimArrayLeft(var S: LongWordArray; const TrimList: Array of LongWord); overload;
procedure TrimArrayLeft(var S: ShortIntArray; const TrimList: Array of ShortInt); overload;
procedure TrimArrayLeft(var S: SmallIntArray; const TrimList: Array of SmallInt); overload;
procedure TrimArrayLeft(var S: LongIntArray; const TrimList: Array of LongInt); overload;
procedure TrimArrayLeft(var S: Int64Array; const TrimList: Array of Int64); overload;
procedure TrimArrayLeft(var S: SingleArray; const TrimList: Array of Single); overload;
procedure TrimArrayLeft(var S: DoubleArray; const TrimList: Array of Double); overload;
procedure TrimArrayLeft(var S: ExtendedArray; const TrimList: Array of Extended); overload;
procedure TrimArrayLeft(var S: StringArray; const TrimList: Array of String); overload;
procedure TrimArrayLeft(var S: PointerArray; const TrimList: Array of Pointer); overload;

procedure TrimArrayRight(var S: ByteArray; const TrimList: Array of Byte); overload;
procedure TrimArrayRight(var S: WordArray; const TrimList: Array of Word); overload;
procedure TrimArrayRight(var S: LongWordArray; const TrimList: Array of LongWord); overload;
procedure TrimArrayRight(var S: ShortIntArray; const TrimList: Array of ShortInt); overload;
procedure TrimArrayRight(var S: SmallIntArray; const TrimList: Array of SmallInt); overload;
procedure TrimArrayRight(var S: LongIntArray; const TrimList: Array of LongInt); overload;
procedure TrimArrayRight(var S: Int64Array; const TrimList: Array of Int64); overload;
procedure TrimArrayRight(var S: SingleArray; const TrimList: Array of Single); overload;
procedure TrimArrayRight(var S: DoubleArray; const TrimList: Array of Double); overload;
procedure TrimArrayRight(var S: ExtendedArray; const TrimList: Array of Extended); overload;
procedure TrimArrayRight(var S: StringArray; const TrimList: Array of String); overload;
procedure TrimArrayRight(var S: PointerArray; const TrimList: Array of Pointer); overload;

function  ArrayInsert(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: WordArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: StringArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: WideStringArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: ObjectArray; const Idx: Integer; const Count: Integer): Integer; overload;
function  ArrayInsert(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer; overload;

procedure FreeObjectArray(var V); overload;
procedure FreeObjectArray(var V; const LoIdx, HiIdx: Integer); overload;
procedure FreeAndNilObjectArray(var V: ObjectArray);

function  PosNext(const Find: Byte; const V: ByteArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Word; const V: WordArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: LongWord; const V: LongWordArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: ShortInt; const V: ShortIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: SmallInt; const V: SmallIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: LongInt; const V: LongIntArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Int64; const V: Int64Array; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Single; const V: SingleArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Double; const V: DoubleArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Extended; const V: ExtendedArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Boolean; const V: BooleanArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: String; const V: StringArray; const PrevPos: Integer = -1;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  PosNext(const Find: Pointer; const V: PointerArray;
          const PrevPos: Integer = -1): Integer; overload;
function  PosNext(const Find: TObject; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;
function  PosNext(const ClassType: TClass; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;
function  PosNext(const ClassName: String; const V: ObjectArray;
          const PrevPos: Integer = -1): Integer; overload;

function  Count(const Find: Byte; const V: ByteArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Word; const V: WordArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: LongWord; const V: LongWordArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: ShortInt; const V: ShortIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: SmallInt; const V: SmallIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: LongInt; const V: LongIntArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Int64; const V: Int64Array;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Single; const V: SingleArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Double; const V: DoubleArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Extended; const V: ExtendedArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: String; const V: StringArray;
          const IsSortedAscending: Boolean = False): Integer; overload;
function  Count(const Find: Boolean; const V: BooleanArray;
          const IsSortedAscending: Boolean = False): Integer; overload;

procedure RemoveAll(const Find: Byte; var V: ByteArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: Word; var V: WordArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: LongWord; var V: LongWordArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: ShortInt; var V: ShortIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: SmallInt; var V: SmallIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: LongInt; var V: LongIntArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: Int64; var V: Int64Array;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: Single; var V: SingleArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: Double; var V: DoubleArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: Extended; var V: ExtendedArray;
          const IsSortedAscending: Boolean = False); overload; 
procedure RemoveAll(const Find: String; var V: StringArray;
          const IsSortedAscending: Boolean = False); overload; 

function  Intersection(const V1, V2: ByteArray;
          const IsSortedAscending: Boolean = False): ByteArray; overload;
function  Intersection(const V1, V2: WordArray;
          const IsSortedAscending: Boolean = False): WordArray; overload;
function  Intersection(const V1, V2: LongWordArray;
          const IsSortedAscending: Boolean = False): LongWordArray; overload;
function  Intersection(const V1, V2: ShortIntArray;
          const IsSortedAscending: Boolean = False): ShortIntArray; overload;
function  Intersection(const V1, V2: SmallIntArray;
          const IsSortedAscending: Boolean = False): SmallIntArray; overload;
function  Intersection(const V1, V2: LongIntArray;
          const IsSortedAscending: Boolean = False): LongIntArray; overload;
function  Intersection(const V1, V2: Int64Array;
          const IsSortedAscending: Boolean = False): Int64Array; overload;
function  Intersection(const V1, V2: SingleArray;
          const IsSortedAscending: Boolean = False): SingleArray; overload;
function  Intersection(const V1, V2: DoubleArray;
          const IsSortedAscending: Boolean = False): DoubleArray; overload;
function  Intersection(const V1, V2: ExtendedArray;
          const IsSortedAscending: Boolean = False): ExtendedArray; overload;
function  Intersection(const V1, V2: StringArray;
          const IsSortedAscending: Boolean = False): StringArray; overload;

function  Difference(const V1, V2: ByteArray;
          const IsSortedAscending: Boolean = False): ByteArray; overload;
function  Difference(const V1, V2: WordArray;
          const IsSortedAscending: Boolean = False): WordArray; overload;
function  Difference(const V1, V2: LongWordArray;
          const IsSortedAscending: Boolean = False): LongWordArray; overload;
function  Difference(const V1, V2: ShortIntArray;
          const IsSortedAscending: Boolean = False): ShortIntArray; overload;
function  Difference(const V1, V2: SmallIntArray;
          const IsSortedAscending: Boolean = False): SmallIntArray; overload;
function  Difference(const V1, V2: LongIntArray;
          const IsSortedAscending: Boolean = False): LongIntArray; overload;
function  Difference(const V1, V2: Int64Array;
          const IsSortedAscending: Boolean = False): Int64Array; overload;
function  Difference(const V1, V2: SingleArray;
          const IsSortedAscending: Boolean = False): SingleArray; overload;
function  Difference(const V1, V2: DoubleArray;
          const IsSortedAscending: Boolean = False): DoubleArray; overload;
function  Difference(const V1, V2: ExtendedArray;
          const IsSortedAscending: Boolean = False): ExtendedArray; overload;
function  Difference(const V1, V2: StringArray;
          const IsSortedAscending: Boolean = False): StringArray; overload;

procedure Reverse(var V: ByteArray); overload;
procedure Reverse(var V: WordArray); overload;
procedure Reverse(var V: LongWordArray); overload;
procedure Reverse(var V: ShortIntArray); overload;
procedure Reverse(var V: SmallIntArray); overload;
procedure Reverse(var V: LongIntArray); overload;
procedure Reverse(var V: Int64Array); overload;
procedure Reverse(var V: SingleArray); overload;
procedure Reverse(var V: DoubleArray); overload;
procedure Reverse(var V: ExtendedArray); overload;
procedure Reverse(var V: StringArray); overload;
procedure Reverse(var V: PointerArray); overload;
procedure Reverse(var V: ObjectArray); overload;

function  AsBooleanArray(const V: Array of Boolean): BooleanArray; overload;
function  AsByteArray(const V: Array of Byte): ByteArray; overload;
function  AsWordArray(const V: Array of Word): WordArray; overload;
function  AsLongWordArray(const V: Array of LongWord): LongWordArray; overload;
function  AsCardinalArray(const V: Array of Cardinal): CardinalArray; overload;
function  AsShortIntArray(const V: Array of ShortInt): ShortIntArray; overload;
function  AsSmallIntArray(const V: Array of SmallInt): SmallIntArray; overload;
function  AsLongIntArray(const V: Array of LongInt): LongIntArray; overload;
function  AsIntegerArray(const V: Array of Integer): IntegerArray; overload;
function  AsInt64Array(const V: Array of Int64): Int64Array; overload;
function  AsSingleArray(const V: Array of Single): SingleArray; overload;
function  AsDoubleArray(const V: Array of Double): DoubleArray; overload;
function  AsExtendedArray(const V: Array of Extended): ExtendedArray; overload;
function  AsCurrencyArray(const V: Array of Currency): CurrencyArray; overload;
function  AsStringArray(const V: Array of String): StringArray; overload;
function  AsWideStringArray(const V: Array of WideString): WideStringArray; overload;
function  AsPointerArray(const V: Array of Pointer): PointerArray; overload;
function  AsCharSetArray(const V: Array of CharSet): CharSetArray; overload;
function  AsObjectArray(const V: Array of TObject): ObjectArray; overload;
function  AsInterfaceArray(const V: Array of IInterface): InterfaceArray; overload;

function  RangeByte(const First: Byte; const Count: Integer;
          const Increment: Byte = 1): ByteArray;
function  RangeWord(const First: Word; const Count: Integer;
          const Increment: Word = 1): WordArray;
function  RangeLongWord(const First: LongWord; const Count: Integer;
          const Increment: LongWord = 1): LongWordArray;
function  RangeCardinal(const First: Cardinal; const Count: Integer;
          const Increment: Cardinal = 1): CardinalArray;
function  RangeShortInt(const First: ShortInt; const Count: Integer;
          const Increment: ShortInt = 1): ShortIntArray;
function  RangeSmallInt(const First: SmallInt; const Count: Integer;
          const Increment: SmallInt = 1): SmallIntArray;
function  RangeLongInt(const First: LongInt; const Count: Integer;
          const Increment: LongInt = 1): LongIntArray;
function  RangeInteger(const First: Integer; const Count: Integer;
          const Increment: Integer = 1): IntegerArray;
function  RangeInt64(const First: Int64; const Count: Integer;
          const Increment: Int64 = 1): Int64Array;
function  RangeSingle(const First: Single; const Count: Integer;
          const Increment: Single = 1): SingleArray;
function  RangeDouble(const First: Double; const Count: Integer;
          const Increment: Double = 1): DoubleArray;
function  RangeExtended(const First: Extended; const Count: Integer;
          const Increment: Extended = 1): ExtendedArray;

function  DupByte(const V: Byte; const Count: Integer): ByteArray;
function  DupWord(const V: Word; const Count: Integer): WordArray;
function  DupLongWord(const V: LongWord; const Count: Integer): LongWordArray;
function  DupCardinal(const V: Cardinal; const Count: Integer): CardinalArray;
function  DupShortInt(const V: ShortInt; const Count: Integer): ShortIntArray;
function  DupSmallInt(const V: SmallInt; const Count: Integer): SmallIntArray;
function  DupLongInt(const V: LongInt; const Count: Integer): LongIntArray;
function  DupInteger(const V: Integer; const Count: Integer): IntegerArray;
function  DupInt64(const V: Int64; const Count: Integer): Int64Array;
function  DupSingle(const V: Single; const Count: Integer): SingleArray;
function  DupDouble(const V: Double; const Count: Integer): DoubleArray;
function  DupExtended(const V: Extended; const Count: Integer): ExtendedArray;
function  DupCurrency(const V: Currency; const Count: Integer): CurrencyArray;
function  DupString(const V: String; const Count: Integer): StringArray;
function  DupCharSet(const V: CharSet; const Count: Integer): CharSetArray;
function  DupObject(const V: TObject; const Count: Integer): ObjectArray;

procedure SetLengthAndZero(var V: ByteArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: WordArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: LongWordArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ShortIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: SmallIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: LongIntArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: Int64Array; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: SingleArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: DoubleArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ExtendedArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: CurrencyArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: CharSetArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: BooleanArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: PointerArray; const NewLength: Integer); overload;
procedure SetLengthAndZero(var V: ObjectArray; const NewLength: Integer;
          const FreeObjects: Boolean = False); overload;

function  IsEqual(const V1, V2: ByteArray): Boolean; overload;
function  IsEqual(const V1, V2: WordArray): Boolean; overload;
function  IsEqual(const V1, V2: LongWordArray): Boolean; overload;
function  IsEqual(const V1, V2: ShortIntArray): Boolean; overload;
function  IsEqual(const V1, V2: SmallIntArray): Boolean; overload;
function  IsEqual(const V1, V2: LongIntArray): Boolean; overload;
function  IsEqual(const V1, V2: Int64Array): Boolean; overload;
function  IsEqual(const V1, V2: SingleArray): Boolean; overload;
function  IsEqual(const V1, V2: DoubleArray): Boolean; overload;
function  IsEqual(const V1, V2: ExtendedArray): Boolean; overload;
function  IsEqual(const V1, V2: CurrencyArray): Boolean; overload;
function  IsEqual(const V1, V2: StringArray): Boolean; overload;
function  IsEqual(const V1, V2: CharSetArray): Boolean; overload;

function  ByteArrayToLongIntArray(const V: ByteArray): LongIntArray;
function  WordArrayToLongIntArray(const V: WordArray): LongIntArray;
function  ShortIntArrayToLongIntArray(const V: ShortIntArray): LongIntArray;
function  SmallIntArrayToLongIntArray(const V: SmallIntArray): LongIntArray;
function  LongIntArrayToInt64Array(const V: LongIntArray): Int64Array;
function  LongIntArrayToSingleArray(const V: LongIntArray): SingleArray;
function  LongIntArrayToDoubleArray(const V: LongIntArray): DoubleArray;
function  LongIntArrayToExtendedArray(const V: LongIntArray): ExtendedArray;
function  SingleArrayToDoubleArray(const V: SingleArray): DoubleArray;
function  SingleArrayToExtendedArray(const V: SingleArray): ExtendedArray;
function  SingleArrayToCurrencyArray(const V: SingleArray): CurrencyArray;
function  SingleArrayToLongIntArray(const V: SingleArray): LongIntArray;
function  SingleArrayToInt64Array(const V: SingleArray): Int64Array;
function  DoubleArrayToExtendedArray(const V: DoubleArray): ExtendedArray;
function  DoubleArrayToCurrencyArray(const V: DoubleArray): CurrencyArray;
function  DoubleArrayToLongIntArray(const V: DoubleArray): LongIntArray;
function  DoubleArrayToInt64Array(const V: DoubleArray): Int64Array;
function  ExtendedArrayToCurrencyArray(const V: ExtendedArray): CurrencyArray;
function  ExtendedArrayToLongIntArray(const V: ExtendedArray): LongIntArray;
function  ExtendedArrayToInt64Array(const V: ExtendedArray): Int64Array;

function  ByteArrayFromIndexes(const V: ByteArray;
          const Indexes: IntegerArray): ByteArray;
function  WordArrayFromIndexes(const V: WordArray;
          const Indexes: IntegerArray): WordArray;
function  LongWordArrayFromIndexes(const V: LongWordArray;
          const Indexes: IntegerArray): LongWordArray;
function  CardinalArrayFromIndexes(const V: CardinalArray;
          const Indexes: IntegerArray): CardinalArray;
function  ShortIntArrayFromIndexes(const V: ShortIntArray;
          const Indexes: IntegerArray): ShortIntArray;
function  SmallIntArrayFromIndexes(const V: SmallIntArray;
          const Indexes: IntegerArray): SmallIntArray;
function  LongIntArrayFromIndexes(const V: LongIntArray;
          const Indexes: IntegerArray): LongIntArray;
function  IntegerArrayFromIndexes(const V: IntegerArray;
          const Indexes: IntegerArray): IntegerArray;
function  Int64ArrayFromIndexes(const V: Int64Array;
          const Indexes: IntegerArray): Int64Array;
function  SingleArrayFromIndexes(const V: SingleArray;
          const Indexes: IntegerArray): SingleArray;
function  DoubleArrayFromIndexes(const V: DoubleArray;
          const Indexes: IntegerArray): DoubleArray;
function  ExtendedArrayFromIndexes(const V: ExtendedArray;
          const Indexes: IntegerArray): ExtendedArray;
function  StringArrayFromIndexes(const V: StringArray;
          const Indexes: IntegerArray): StringArray;

procedure Sort(const V: ByteArray); overload;
procedure Sort(const V: WordArray); overload;
procedure Sort(const V: LongWordArray); overload;
procedure Sort(const V: ShortIntArray); overload;
procedure Sort(const V: SmallIntArray); overload;
procedure Sort(const V: LongIntArray); overload;
procedure Sort(const V: Int64Array); overload;
procedure Sort(const V: SingleArray); overload;
procedure Sort(const V: DoubleArray); overload;
procedure Sort(const V: ExtendedArray); overload;
procedure Sort(const V: StringArray); overload;

procedure Sort(const Key: IntegerArray; const Data: IntegerArray); overload;
procedure Sort(const Key: IntegerArray; const Data: Int64Array); overload;
procedure Sort(const Key: IntegerArray; const Data: StringArray); overload;
procedure Sort(const Key: IntegerArray; const Data: ExtendedArray); overload;
procedure Sort(const Key: IntegerArray; const Data: PointerArray); overload;
procedure Sort(const Key: StringArray; const Data: IntegerArray); overload;
procedure Sort(const Key: StringArray; const Data: Int64Array); overload;
procedure Sort(const Key: StringArray; const Data: StringArray); overload;
procedure Sort(const Key: StringArray; const Data: ExtendedArray); overload;
procedure Sort(const Key: StringArray; const Data: PointerArray); overload;
procedure Sort(const Key: ExtendedArray; const Data: IntegerArray); overload;
procedure Sort(const Key: ExtendedArray; const Data: Int64Array); overload;
procedure Sort(const Key: ExtendedArray; const Data: StringArray); overload;
procedure Sort(const Key: ExtendedArray; const Data: ExtendedArray); overload;
procedure Sort(const Key: ExtendedArray; const Data: PointerArray); overload;



{                                                                              }
{ Self testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation



{$IFDEF DELPHI}{$IFDEF OS_WIN32}{$IFDEF CPU_INTEL386}
  {$DEFINE USE_ASM386}
{$ENDIF}{$ENDIF}{$ENDIF}

{                                                                              }
{ Integer                                                                      }
{                                                                              }
function MinI(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A else
    Result := B;
end;

function MaxI(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A else
    Result := B;
end;

function MinC(const A, B: Cardinal): Cardinal;
begin
  if A < B then
    Result := A else
    Result := B;
end;

function MaxC(const A, B: Cardinal): Cardinal;
begin
  if A > B then
    Result := A else
    Result := B;
end;

function Clip(const Value: Integer; const Low, High: Integer): Integer;
begin
  if Value < Low then
    Result := Low else
  if Value > High then
    Result := High else
    Result := Value;
end;

function ClipByte(const Value: Integer): Integer;
begin
  if Value < MinByte then
    Result := MinByte else
  if Value > MaxByte then
    Result := MaxByte else
    Result := Value;
end;

function ClipWord(const Value: Integer): Integer;
begin
  if Value < MinWord then
    Result := MinWord else
  if Value > MaxWord then
    Result := MaxWord else
    Result := Value;
end;

function ClipLongWord(const Value: Int64): LongWord;
begin
  if Value < MinLongWord then
    Result := MinLongWord else
  if Value > MaxLongWord then
    Result := MaxLongWord else
    Result := LongWord(Value);
end;

function SumClipI(const A, I: Integer): Integer;
begin
  if I >= 0 then
    if A >= MaxInteger - I then
      Result := MaxInteger else
      Result := A + I
  else
    if A <= MinInteger - I then
      Result := MinInteger else
      Result := A + I;
end;

function SumClipC(const A: Cardinal; const I: Integer): Cardinal;
var B : Cardinal;
begin
  if I >= 0 then
    if A >= MaxCardinal - Cardinal(I) then
      Result := MaxCardinal else
      Result := A + Cardinal(I)
  else
    begin
      B := Cardinal(-I);
      if A <= B then
        Result := 0 else
        Result := A - B;
    end;
end;

function InByteRange(const A: Int64): Boolean;
begin
  Result := (A >= MinByte) and (A <= MaxByte);
end;

function InWordRange(const A: Int64): Boolean;
begin
  Result := (A >= MinWord) and (A <= MaxWord);
end;

function InLongWordRange(const A: Int64): Boolean;
begin
  Result := (A >= MinLongWord) and (A <= MaxLongWord);
end;

function InShortIntRange(const A: Int64): Boolean;
begin
  Result := (A >= MinShortInt) and (A <= MaxShortInt);
end;

function InSmallIntRange(const A: Int64): Boolean;
begin
  Result := (A >= MinSmallInt) and (A <= MaxSmallInt);
end;

function InLongIntRange(const A: Int64): Boolean;
begin
  Result := (A >= MinLongInt) and (A <= MaxLongInt);
end;



{                                                                              }
{ Real                                                                         }
{                                                                              }
function MinF(const A, B: Extended): Extended;
begin
  if A < B then
    Result := A else
    Result := B;
end;

function MaxF(const A, B: Extended): Extended;
begin
  if A > B then
    Result := A else
    Result := B;
end;

function ClipF(const Value: Extended; const Low, High: Extended): Extended;
begin
  if Value < Low then
    Result := Low else
  if Value > High then
    Result := High else
    Result := Value;
end;

function InSingleRange(const A: Extended): Boolean;
var B : Extended;
begin
  B := Abs(A);
  Result := (B >= MinSingle) and (B <= MaxSingle);
end;

function InDoubleRange(const A: Extended): Boolean;
var B : Extended;
begin
  B := Abs(A);
  Result := (B >= MinDouble) and (B <= MaxDouble);
end;

function InCurrencyRange(const A: Extended): Boolean;
begin
  Result := (A >= MinCurrency) and (A <= MaxCurrency);
end;

function InCurrencyRange(const A: Int64): Boolean;
begin
  Result := (A >= MinCurrency) and (A <= MaxCurrency);
end;



{                                                                              }
{ Bit functions                                                                }
{                                                                              }

{ Assembly versions of ReverseBits and SwapEndian taken from the               }
{ Delphi Encryption Compendium by Hagen Reddmann (HaReddmann@aol.com)          }
{$IFDEF USE_ASM386}
function ReverseBits(const Value: LongWord): LongWord;
asm
      BSWAP   EAX
      MOV     EDX, EAX
      AND     EAX, 0AAAAAAAAh
      SHR     EAX, 1
      AND     EDX, 055555555h
      SHL     EDX, 1
      OR      EAX, EDX
      MOV     EDX, EAX
      AND     EAX, 0CCCCCCCCh
      SHR     EAX, 2
      AND     EDX, 033333333h
      SHL     EDX, 2
      OR      EAX, EDX
      MOV     EDX, EAX
      AND     EAX, 0F0F0F0F0h
      SHR     EAX, 4
      AND     EDX, 00F0F0F0Fh
      SHL     EDX, 4
      OR      EAX, EDX
end;
{$ELSE}
function ReverseBits(const Value: LongWord): LongWord;
var I : Byte;
begin
  Result := 0;
  For I := 0 to 31 do
    if Value and BitMaskTable[I] <> 0 then
      Result := Result or BitMaskTable[31 - I];
end;
{$ENDIF}

function ReverseBits(const Value: LongWord; const BitCount: Integer): LongWord;
var I : Integer;
  V : LongWord;
begin
  V := Value;
  Result := 0;
  For I := 0 to MinI(BitCount, BitsPerLongWord) - 1 do
    begin
      Result := (Result shl 1) or (V and 1);
      V := V shr 1;
    end;
end;

{$IFDEF USE_ASM386}
function SwapEndian(const Value: LongWord): LongWord;
asm
      XCHG    AH, AL
      ROL     EAX, 16
      XCHG    AH, AL
end;
{$ELSE}
function SwapEndian(const Value: LongWord): LongWord;
type Bytes4 = packed record
       B1, B2, B3, B4 : Byte;
     end;
var Val : Bytes4 absolute Value;
    Res : Bytes4 absolute Result;
begin
  Res.B4 := Val.B1;
  Res.B3 := Val.B2;
  Res.B2 := Val.B3;
  Res.B1 := Val.B4;
end;
{$ENDIF}

procedure SwapEndianBuf(var Buf; const Count: Integer);
var P : PLongWord;
    I : Integer;
begin
  P := @Buf;
  For I := 1 to Count do
    begin
      P^ := SwapEndian(P^);
      Inc(P);
    end;
end;

{$IFDEF USE_ASM386}
function TwosComplement(const Value: LongWord): LongWord;
asm
      NEG EAX
end;
{$ELSE}
function TwosComplement(const Value: LongWord): LongWord;
begin
  Result := not Value + 1;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function RotateLeftBits(const Value: LongWord; const Bits: Byte): LongWord;
asm
      MOV   CL, DL
      ROL   EAX, CL
end;
{$ELSE}
function RotateLeftBits(const Value: LongWord; const Bits: Byte): LongWord;
var I : Integer;
begin
  Result := Value;
  For I := 1 to Bits do
    if Value and $80000000 = 0 then
      Result := Value shl 1 else
      Result := (Value shl 1) or 1;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function RotateRightBits(const Value: LongWord; const Bits: Byte): LongWord;
asm
      MOV   CL, DL
      ROL   EAX, CL
end;
{$ELSE}
function RotateRightBits(const Value: LongWord; const Bits: Byte): LongWord;
var I : Integer;
begin
  Result := Value;
  For I := 1 to Bits do
    if Value and 1 = 0 then
      Result := Value shr 1 else
      Result := (Value shr 1) or $80000000;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function SetBit(const Value, BitIndex: LongWord): LongWord;
asm
      {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @Fin
      {$ENDIF}
      OR      EAX, DWORD PTR [BitIndex * 4 + BitMaskTable]
    @Fin:
end;
{$ELSE}
function SetBit(const Value, BitIndex: LongWord): LongWord;
begin
  Result := Value or BitMaskTable[BitIndex];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function ClearBit(const Value, BitIndex: LongWord): LongWord;
asm
      {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @Fin
      {$ENDIF}
      MOV     ECX, DWORD PTR [BitIndex * 4 + BitMaskTable]
      NOT     ECX
      AND     EAX, ECX
    @Fin:
end;
{$ELSE}
function ClearBit(const Value, BitIndex: LongWord): LongWord;
begin
  Result := Value and not BitMaskTable[BitIndex];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function ToggleBit(const Value, BitIndex: LongWord): LongWord;
asm
      {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @Fin
      {$ENDIF}
      XOR     EAX, DWORD PTR [BitIndex * 4 + BitMaskTable]
    @Fin:
end;
{$ELSE}
function ToggleBit(const Value, BitIndex: LongWord): LongWord;
begin
  Result := Value xor BitMaskTable[BitIndex];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsHighBitSet(const Value: LongWord): Boolean;
asm
      TEST    Value, $80000000
      SETNZ   AL
end;
{$ELSE}
function IsHighBitSet(const Value: LongWord): Boolean;
begin
  Result := Value and $80000000 <> 0;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsBitSet(const Value, BitIndex: LongWord): Boolean;
asm
      {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @Fin
      {$ENDIF}
      MOV     ECX, DWORD PTR BitMaskTable [BitIndex * 4]
      TEST    Value, ECX
      SETNZ   AL
    @Fin:
end;
{$ELSE}
function IsBitSet(const Value, BitIndex: LongWord): Boolean;
begin
  Result := Value and BitMaskTable[BitIndex] <> 0;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function SetBitScanForward(const Value: LongWord): Integer;
asm
      OR      EAX, EAX
      JZ      @NoBits
      BSF     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function SetBitScanForward(const Value, BitIndex: LongWord): Integer;
asm
      {$IFOPT R+}
      CMP     BitIndex, BitsPerLongWord
      JAE     @@zq
      {$ENDIF}
      MOV     ECX, BitIndex
      MOV     EDX, $FFFFFFFF
      SHL     EDX, CL
      AND     EDX, EAX
      JE      @@zq
      BSF     EAX, EDX
      RET
@@zq: MOV     EAX, -1
end;
{$ELSE}
function SetBitScanForward(const Value, BitIndex: LongWord): Integer;
var I : Byte;
begin
  {$IFOPT R+}
  if BitIndex < BitsPerLongWord then
  {$ENDIF}
  For I := Byte(BitIndex) to 31 do
    if Value and BitMaskTable[I] <> 0 then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function SetBitScanForward(const Value: LongWord): Integer;
begin
  Result := SetBitScanForward(Value, 0);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function SetBitScanReverse(const Value: LongWord): Integer;
asm
      OR      EAX, EAX
      JZ      @NoBits
      BSR     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function SetBitScanReverse(const Value, BitIndex: LongWord): Integer;
asm
      {$IFOPT R+}
      CMP     EDX, BitsPerLongWord
      JAE     @@zq
      {$ENDIF}
      LEA     ECX, [EDX-31]
      MOV     EDX, $FFFFFFFF
      NEG     ECX
      SHR     EDX, CL
      AND     EDX, EAX
      JE      @@zq
      BSR     EAX, EDX
      RET
@@zq: MOV     EAX, -1
end;
{$ELSE}
function SetBitScanReverse(const Value, BitIndex: LongWord): Integer;
var I : Byte;
begin
  {$IFOPT R+}
  if BitIndex < BitsPerLongWord then
  {$ENDIF}
  For I := Byte(BitIndex) downto 0 do
    if Value and BitMaskTable[I] <> 0 then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function SetBitScanReverse(const Value: LongWord): Integer;
begin
  Result := SetBitScanReverse(Value, 31);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function ClearBitScanForward(const Value: LongWord): Integer;
asm
      NOT     EAX
      OR      EAX, EAX
      JZ      @NoBits
      BSF     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function ClearBitScanForward(const Value, BitIndex: LongWord): Integer;
asm
      {$IFOPT R+}
      CMP     EDX, BitsPerLongWord
      JAE     @@zq
      {$ENDIF}
      MOV     ECX, EDX
      MOV     EDX, $FFFFFFFF
      NOT     EAX
      SHL     EDX, CL
      AND     EDX, EAX
      JE      @@zq
      BSF     EAX, EDX
      RET
@@zq: MOV     EAX, -1
end;
{$ELSE}
function ClearBitScanForward(const Value, BitIndex: LongWord): Integer;
var I : Byte;
begin
  {$IFOPT R+}
  if BitIndex < BitsPerLongWord then
  {$ENDIF}
  For I := Byte(BitIndex) to 31 do
    if Value and BitMaskTable[I] = 0 then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function ClearBitScanForward(const Value: LongWord): Integer;
begin
  Result := ClearBitScanForward(Value, 0);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function ClearBitScanReverse(const Value: LongWord): Integer;
asm
      NOT     EAX
      OR      EAX, EAX
      JZ      @NoBits
      BSR     EAX, EAX
      RET
  @NoBits:
      MOV     EAX, -1
end;

function ClearBitScanReverse(const Value, BitIndex: LongWord): Integer;
asm
      {$IFOPT R+}
      CMP     EDX, BitsPerLongWord
      JAE     @@zq
      {$ENDIF}
      LEA     ECX, [EDX-31]
      MOV     EDX, $FFFFFFFF
      NEG     ECX
      NOT     EAX
      SHR     EDX, CL
      AND     EDX, EAX
      JE      @@zq
      BSR     EAX, EDX
      RET
@@zq: MOV     EAX, -1
end;
{$ELSE}
function ClearBitScanReverse(const Value, BitIndex: LongWord): Integer;
var I : Byte;
begin
  {$IFOPT R+}
  if BitIndex < BitsPerLongWord then
  {$ENDIF}
  For I := Byte(BitIndex) downto 0 do
    if Value and BitMaskTable[I] = 0 then
      begin
        Result := I;
        exit;
      end;
  Result := -1;
end;

function ClearBitScanReverse(const Value: LongWord): Integer;
begin
  Result := ClearBitScanReverse(Value, 31);
end;
{$ENDIF}

const
  BitCountTable : Array[0..255] of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
     4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

{$IFDEF USE_ASM386}
function BitCount(const Value: LongWord): LongWord;
asm
      MOVZX   EDX, AL
      MOVZX   EDX, BYTE PTR [EDX + BitCountTable]
      MOVZX   ECX, AH
      ADD     DL, BYTE PTR [ECX + BitCountTable]
      SHR     EAX, 16
      MOVZX   ECX, AH
      ADD     DL, BYTE PTR [ECX + BitCountTable]
      AND     EAX, $FF
      ADD     DL, BYTE PTR [EAX + BitCountTable]
      MOV     AL, DL
end;
{$ELSE}
function BitCount(const Value: LongWord): LongWord;
var V : Array[0..3] of Byte absolute Value;
begin
  Result := BitCountTable[V[0]] + BitCountTable[V[1]] +
            BitCountTable[V[2]] + BitCountTable[V[3]];
end;
{$ENDIF}

function IsPowerOfTwo(const Value: LongWord): Boolean;
begin
  Result := BitCount(Value) = 1;
end;

function LowBitMask(const HighBitIndex: LongWord): LongWord;
begin
  {$IFOPT R+}
  if HighBitIndex >= BitsPerLongWord then
    Result := 0 else
  {$ENDIF}
  Result := BitMaskTable[HighBitIndex] - 1;
end;

function HighBitMask(const LowBitIndex: LongWord): LongWord;
begin
  {$IFOPT R+}
  if LowBitIndex >= BitsPerLongWord then
    Result := 0 else
  {$ENDIF}
  Result := not BitMaskTable[LowBitIndex] + 1;
end;

function RangeBitMask(const LowBitIndex, HighBitIndex: LongWord): LongWord;
begin
  {$IFOPT R+}
  if (LowBitIndex >= BitsPerLongWord) and (HighBitIndex >= BitsPerLongWord) then
    begin
      Result := 0;
      exit;
    end;
  {$ENDIF}
  Result := $FFFFFFFF;
  if LowBitIndex > 0 then
    Result := Result xor (BitMaskTable[LowBitIndex] - 1);
  if HighBitIndex < 31 then
    Result := Result xor (not BitMaskTable[HighBitIndex + 1] + 1);
end;

function SetBitRange(const Value: LongWord; const LowBitIndex, HighBitIndex: LongWord): LongWord;
begin
  Result := Value or RangeBitMask(LowBitIndex, HighBitIndex);
end;

function ClearBitRange(const Value: LongWord; const LowBitIndex, HighBitIndex: LongWord): LongWord;
begin
  Result := Value and not RangeBitMask(LowBitIndex, HighBitIndex);
end;

function ToggleBitRange(const Value: LongWord; const LowBitIndex, HighBitIndex: LongWord): LongWord;
begin
  Result := Value xor RangeBitMask(LowBitIndex, HighBitIndex);
end;

function IsBitRangeSet(const Value: LongWord; const LowBitIndex, HighBitIndex: LongWord): Boolean;
var M: LongWord;
begin
  M := RangeBitMask(LowBitIndex, HighBitIndex);
  Result := Value and M = M;
end;

function IsBitRangeClear(const Value: LongWord; const LowBitIndex, HighBitIndex: LongWord): Boolean;
begin
  Result := Value and RangeBitMask(LowBitIndex, HighBitIndex) = 0;
end;



{                                                                              }
{ Sets                                                                         }
{                                                                              }
function AsCharSet(const C: Array of Char): CharSet;
var I: Integer;
begin
  Result := [];
  For I := 0 to High(C) do
    Include(Result, C[I]);
end;

function AsByteSet(const C: Array of Byte): ByteSet;
var I: Integer;
begin
  Result := [];
  For I := 0 to High(C) do
    Include(Result, C[I]);
end;

{$IFDEF USE_ASM386}
procedure ComplementChar(var C: CharSet; const Ch: Char);
asm
      MOVZX   ECX, DL
      BTC     [EAX], ECX
end;
{$ELSE}
procedure ComplementChar(var C: CharSet; const Ch: Char);
begin
  if Ch in C then
    Exclude(C, Ch) else
    Include(C, Ch);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure ClearCharSet(var C: CharSet);
asm
      XOR     EDX, EDX
      MOV     [EAX], EDX
      MOV     [EAX + 4], EDX
      MOV     [EAX + 8], EDX
      MOV     [EAX + 12], EDX
      MOV     [EAX + 16], EDX
      MOV     [EAX + 20], EDX
      MOV     [EAX + 24], EDX
      MOV     [EAX + 28], EDX
end;
{$ELSE}
procedure ClearCharSet(var C: CharSet);
begin
  C := [];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure FillCharSet(var C: CharSet);
asm
      MOV     EDX, $FFFFFFFF
      MOV     [EAX], EDX
      MOV     [EAX + 4], EDX
      MOV     [EAX + 8], EDX
      MOV     [EAX + 12], EDX
      MOV     [EAX + 16], EDX
      MOV     [EAX + 20], EDX
      MOV     [EAX + 24], EDX
      MOV     [EAX + 28], EDX
end;
{$ELSE}
procedure FillCharSet(var C: CharSet);
begin
  C := [#0..#255];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure ComplementCharSet(var C: CharSet);
asm
      NOT     DWORD PTR [EAX]
      NOT     DWORD PTR [EAX + 4]
      NOT     DWORD PTR [EAX + 8]
      NOT     DWORD PTR [EAX + 12]
      NOT     DWORD PTR [EAX + 16]
      NOT     DWORD PTR [EAX + 20]
      NOT     DWORD PTR [EAX + 24]
      NOT     DWORD PTR [EAX + 28]
end;
{$ELSE}
procedure ComplementCharSet(var C: CharSet);
begin
  C := [#0..#255] - C;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure AssignCharSet(var DestSet: CharSet; const SourceSet: CharSet);
asm
      MOV     ECX, [EDX]
      MOV     [EAX], ECX
      MOV     ECX, [EDX + 4]
      MOV     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      MOV     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      MOV     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      MOV     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      MOV     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      MOV     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      MOV     [EAX + 28], ECX
end;
{$ELSE}
procedure AssignCharSet(var DestSet: CharSet; const SourceSet: CharSet);
begin
  DestSet := SourceSet;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Union(var DestSet: CharSet; const SourceSet: CharSet);
asm
      MOV     ECX, [EDX]
      OR      [EAX], ECX
      MOV     ECX, [EDX + 4]
      OR      [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      OR      [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      OR      [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      OR      [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      OR      [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      OR      [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      OR      [EAX + 28], ECX
end;
{$ELSE}
procedure Union(var DestSet: CharSet; const SourceSet: CharSet);
begin
  DestSet := DestSet + SourceSet;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Difference(var DestSet: CharSet; const SourceSet: CharSet);
asm
      MOV     ECX, [EDX]
      NOT     ECX
      AND     [EAX], ECX
      MOV     ECX, [EDX + 4]
      NOT     ECX
      AND     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      NOT     ECX
      AND     [EAX + 8],ECX
      MOV     ECX, [EDX + 12]
      NOT     ECX
      AND     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      NOT     ECX
      AND     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      NOT     ECX
      AND     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      NOT     ECX
      AND     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      NOT     ECX
      AND     [EAX + 28], ECX
end;
{$ELSE}
procedure Difference(var DestSet: CharSet; const SourceSet: CharSet);
begin
  DestSet := DestSet - SourceSet;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Intersection(var DestSet: CharSet; const SourceSet: CharSet);
asm
      MOV     ECX, [EDX]
      AND     [EAX], ECX
      MOV     ECX, [EDX + 4]
      AND     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      AND     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      AND     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      AND     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      AND     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      AND     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      AND     [EAX + 28], ECX
end;
{$ELSE}
procedure Intersection(var DestSet: CharSet; const SourceSet: CharSet);
begin
  DestSet := DestSet * SourceSet;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure XORCharSet(var DestSet: CharSet; const SourceSet: CharSet);
asm
      MOV     ECX, [EDX]
      XOR     [EAX], ECX
      MOV     ECX, [EDX + 4]
      XOR     [EAX + 4], ECX
      MOV     ECX, [EDX + 8]
      XOR     [EAX + 8], ECX
      MOV     ECX, [EDX + 12]
      XOR     [EAX + 12], ECX
      MOV     ECX, [EDX + 16]
      XOR     [EAX + 16], ECX
      MOV     ECX, [EDX + 20]
      XOR     [EAX + 20], ECX
      MOV     ECX, [EDX + 24]
      XOR     [EAX + 24], ECX
      MOV     ECX, [EDX + 28]
      XOR     [EAX + 28], ECX
end;
{$ELSE}
procedure XORCharSet(var DestSet: CharSet; const SourceSet: CharSet);
var Ch: Char;
begin
  For Ch := #0 to #255 do
    if Ch in DestSet then
      begin
        if Ch in SourceSet then
          Exclude(DestSet, Ch);
      end else
      if Ch in SourceSet then
        Include(DestSet, Ch);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsSubSet(const A, B: CharSet): Boolean;
asm
      MOV     ECX, [EDX]
      NOT     ECX
      AND     ECX, [EAX]
      JNE     @Fin0
      MOV     ECX, [EDX + 4]
      NOT     ECX
      AND     ECX, [EAX + 4]
      JNE     @Fin0
      MOV     ECX, [EDX + 8]
      NOT     ECX
      AND     ECX, [EAX + 8]
      JNE     @Fin0
      MOV     ECX, [EDX + 12]
      NOT     ECX
      AND     ECX, [EAX + 12]
      JNE     @Fin0
      MOV     ECX, [EDX + 16]
      NOT     ECX
      AND     ECX, [EAX + 16]
      JNE     @Fin0
      MOV     ECX, [EDX + 20]
      NOT     ECX
      AND     ECX, [EAX + 20]
      JNE     @Fin0
      MOV     ECX, [EDX + 24]
      NOT     ECX
      AND     ECX, [EAX + 24]
      JNE     @Fin0
      MOV     ECX, [EDX + 28]
      NOT     ECX
      AND     ECX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsSubSet(const A, B: CharSet): Boolean;
begin
  Result := A <= B;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsEqual(const A, B: CharSet): Boolean;
asm
      MOV     ECX, [EDX]
      XOR     ECX, [EAX]
      JNE     @Fin0
      MOV     ECX, [EDX + 4]
      XOR     ECX, [EAX + 4]
      JNE     @Fin0
      MOV     ECX, [EDX + 8]
      XOR     ECX, [EAX + 8]
      JNE     @Fin0
      MOV     ECX, [EDX + 12]
      XOR     ECX, [EAX + 12]
      JNE     @Fin0
      MOV     ECX, [EDX + 16]
      XOR     ECX, [EAX + 16]
      JNE     @Fin0
      MOV     ECX, [EDX + 20]
      XOR     ECX, [EAX + 20]
      JNE     @Fin0
      MOV     ECX, [EDX + 24]
      XOR     ECX, [EAX + 24]
      JNE     @Fin0
      MOV     ECX, [EDX + 28]
      XOR     ECX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsEqual(const A, B: CharSet): Boolean;
begin
  Result := A = B;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsEmpty(const C: CharSet): Boolean;
asm
      MOV     EDX, [EAX]
      OR      EDX, [EAX + 4]
      OR      EDX, [EAX + 8]
      OR      EDX, [EAX + 12]
      OR      EDX, [EAX + 16]
      OR      EDX, [EAX + 20]
      OR      EDX, [EAX + 24]
      OR      EDX, [EAX + 28]
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX,EAX
end;
{$ELSE}
function IsEmpty(const C: CharSet): Boolean;
begin
  Result := C = [];
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function IsComplete(const C: CharSet): Boolean;
asm
      MOV     EDX, [EAX]
      AND     EDX, [EAX + 4]
      AND     EDX, [EAX + 8]
      AND     EDX, [EAX + 12]
      AND     EDX, [EAX + 16]
      AND     EDX, [EAX + 20]
      AND     EDX, [EAX + 24]
      AND     EDX, [EAX + 28]
      CMP     EDX, $FFFFFFFF
      JNE     @Fin0
      MOV     EAX, 1
      RET
@Fin0:
      XOR     EAX, EAX
end;
{$ELSE}
function IsComplete(const C: CharSet): Boolean;
begin
  Result := C = CompleteCharSet;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function CharCount(const C: CharSet): Integer;
asm
      PUSH    EBX
      PUSH    ESI
      MOV     EBX, EAX
      XOR     ESI, ESI
      MOV     EAX, [EBX]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 4]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 8]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 12]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 16]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 20]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 24]
      CALL    BitCount
      ADD     ESI, EAX
      MOV     EAX, [EBX + 28]
      CALL    BitCount
      ADD     EAX, ESI
      POP     ESI
      POP     EBX
end;
{$ELSE}
function CharCount(const C: CharSet): Integer;
var I: Char;
begin
  Result := 0;
  For I := #0 to #255 do
    if I in C then
      Inc(Result);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure ConvertCaseInsensitive(var C: CharSet);
asm
      MOV     ECX, [EAX + 12]
      AND     ECX, $3FFFFFF
      OR      [EAX + 8], ECX
      MOV     ECX, [EAX + 8]
      AND     ECX, $3FFFFFF
      OR      [EAX + 12], ECX
end;
{$ELSE}
procedure ConvertCaseInsensitive(var C: CharSet);
var Ch: Char;
begin
  For Ch := 'A' to 'Z' do
    if Ch in C then
      Include(C, Char(Ord(Ch) + 32));
  For Ch := 'a' to 'z' do
    if Ch in C then
      Include(C, Char(Ord(Ch) - 32));
end;
{$ENDIF}

function CaseInsensitiveCharSet(const C: CharSet): CharSet;
begin
  AssignCharSet(Result, C);
  ConvertCaseInsensitive(Result);
end;



{                                                                              }
{ Range functions                                                              }
{                                                                              }
function IntRangeLength(const Low, High: Integer): Int64;
begin
  if Low > High then
    Result := 0
  else
    Result := Int64(High - Low) + 1;
end;

function IntRangeAdjacent(const Low1, High1, Low2, High2: Integer): Boolean;
begin
  Result := ((Low2 > MinInteger) and (High1 = Low2 - 1)) or
            ((High2 < MaxInteger) and (Low1 = High2 + 1));
end;

function IntRangeOverlap(const Low1, High1, Low2, High2: Integer): Boolean;
begin
  Result := ((Low1 >= Low2) and (Low1 <= High2)) or
            ((Low2 >= Low1) and (Low2 <= High1));
end;

function IntRangeHasElement(const Low, High, Element: Integer): Boolean;
begin
  Result := (Element >= Low) and (Element <= High);
end;

function IntRangeIncludeElement(var Low, High: Integer;
    const Element: Integer): Boolean;
begin
  Result := (Element >= Low) and (Element <= High);
  if Result then
    exit;
  if (Element < Low) and (Element + 1 = Low) then
    begin
      Low := Element;
      Result := True;
    end else
  if (Element > High) and (Element - 1 = High) then
    begin
      High := Element;
      Result := True;
    end;
end;

function IntRangeIncludeElementRange(var Low, High: Integer;
    const LowElement, HighElement: Integer): Boolean;
begin
  Result := (LowElement >= Low) and (HighElement <= High);
  if Result then
    exit;
  if ((Low >= LowElement) and (Low <= HighElement)) or
     ((Low > MinInteger) and (Low - 1 = HighElement)) then
    begin
      Low := LowElement;
      Result := True;
    end;
  if ((High >= LowElement) and (High <= HighElement)) or
     ((High < MaxInteger) and (High + 1 = LowElement)) then
    begin
      High := HighElement;
      Result := True;
    end;
end;

function CardinalRangeLength(const Low, High: Cardinal): Int64;
begin
  if Low > High then
    Result := 0
  else
    Result := Int64(High - Low) + 1;
end;

function CardinalRangeAdjacent(const Low1, High1, Low2, High2: Cardinal): Boolean;
begin
  Result := ((Low2 > MinCardinal) and (High1 = Low2 - 1)) or
            ((High2 < MaxCardinal) and (Low1 = High2 + 1));
end;

function CardinalRangeOverlap(const Low1, High1, Low2, High2: Cardinal): Boolean;
begin
  Result := ((Low1 >= Low2) and (Low1 <= High2)) or
            ((Low2 >= Low1) and (Low2 <= High1));
end;

function CardinalRangeHasElement(const Low, High, Element: Cardinal): Boolean;
begin
  Result := (Element >= Low) and (Element <= High);
end;

function CardinalRangeIncludeElement(var Low, High: Cardinal;
    const Element: Cardinal): Boolean;
begin
  Result := (Element >= Low) and (Element <= High);
  if Result then
    exit;
  if (Element < Low) and (Element + 1 = Low) then
    begin
      Low := Element;
      Result := True;
    end else
  if (Element > High) and (Element - 1 = High) then
    begin
      High := Element;
      Result := True;
    end;
end;

function CardinalRangeIncludeElementRange(var Low, High: Cardinal;
    const LowElement, HighElement: Cardinal): Boolean;
begin
  Result := (LowElement >= Low) and (HighElement <= High);
  if Result then
    exit;
  if ((Low >= LowElement) and (Low <= HighElement)) or
     ((Low > MinCardinal) and (Low - 1 = HighElement)) then
    begin
      Low := LowElement;
      Result := True;
    end;
  if ((High >= LowElement) and (High <= HighElement)) or
     ((High < MaxCardinal) and (High + 1 = LowElement)) then
    begin
      High := HighElement;
      Result := True;
    end;
end;



{                                                                              }
{ Swap                                                                         }
{                                                                              }
{$IFDEF USE_ASM386}
procedure Swap(var X, Y: Boolean);
asm
    mov cl, [edx]
    xchg byte ptr [eax], cl
    mov [edx], cl
end;
{$ELSE}
procedure Swap(var X, Y: Boolean);
var F: Boolean;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: Byte);
asm
    mov cl, [edx]
    xchg byte ptr [eax], cl
    mov [edx], cl
end;
{$ELSE}
procedure Swap(var X, Y: Byte);
var F: Byte;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: ShortInt);
asm
    mov cl, [edx]
    xchg byte ptr [eax], cl
    mov [edx], cl
end;
{$ELSE}
procedure Swap(var X, Y: ShortInt);
var F: ShortInt;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: Word);
asm
    mov cx, [edx]
    xchg word ptr [eax], cx
    mov [edx], cx
end;
{$ELSE}
procedure Swap(var X, Y: Word);
var F: Word;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: SmallInt);
asm
    mov cx, [edx]
    xchg word ptr [eax], cx
    mov [edx], cx
end;
{$ELSE}
procedure Swap(var X, Y: SmallInt);
var F: SmallInt;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: LongInt);
asm
    mov ecx, [edx]
    xchg [eax], ecx
    mov [edx], ecx
end;
{$ELSE}
procedure Swap(var X, Y: LongInt);
var F: LongInt;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: LongWord);
asm
    mov ecx, [edx]
    xchg [eax], ecx
    mov [edx], ecx
end;
{$ELSE}
procedure Swap(var X, Y: LongWord);
var F: LongWord;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: Pointer);
asm
    mov ecx, [edx]
    xchg [eax], ecx
    mov [edx], ecx
end;
{$ELSE}
procedure Swap(var X, Y: Pointer);
var F: Pointer;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure Swap(var X, Y: TObject);
asm
    mov ecx, [edx]
    xchg [eax], ecx
    mov [edx], ecx
end;
{$ELSE}
procedure Swap(var X, Y: TObject);
var F: TObject;
begin
  F := X;
  X := Y;
  Y := F;
end;
{$ENDIF}

procedure Swap(var X, Y: Int64);
var F: Int64;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: Single);
var F: Single;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: Double);
var F: Double;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: Extended);
var F: Extended;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: Currency);
var F: Currency;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: String);
var F: String;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: WideString);
var F: WideString;
begin
  F := X;
  X := Y;
  Y := F;
end;

{$IFDEF USE_ASM386}
procedure SwapObjects(var X, Y);
asm
    mov ecx, [edx]
    xchg [eax], ecx
    mov [edx], ecx
end;
{$ELSE}
procedure SwapObjects(var X, Y);
var F: TObject;
begin
  F := TObject(X);
  TObject(X) := TObject(Y);
  TObject(Y) := F;
end;
{$ENDIF}



{                                                                              }
{ iif                                                                          }
{                                                                              }
function iif(const Expr: Boolean; const TrueValue, FalseValue: Integer): Integer;
begin
  if Expr then
    Result := TrueValue else
    Result := FalseValue;
end;

function iif(const Expr: Boolean; const TrueValue, FalseValue: Int64): Int64;
begin
  if Expr then
    Result := TrueValue else
    Result := FalseValue;
end;

function iif(const Expr: Boolean; const TrueValue, FalseValue: Extended): Extended;
begin
  if Expr then
    Result := TrueValue else
    Result := FalseValue;
end;

function iif(const Expr: Boolean; const TrueValue, FalseValue: String): String;
begin
  if Expr then
    Result := TrueValue else
    Result := FalseValue;
end;

function iif(const Expr: Boolean; const TrueValue, FalseValue: TObject): TObject;
begin
  if Expr then
    Result := TrueValue else
    Result := FalseValue;
end;




{                                                                              }
{ Compare                                                                      }
{                                                                              }
function ReverseCompareResult(const C: TCompareResult): TCompareResult;
begin
  if C = crLess then
    Result := crGreater else
  if C = crGreater then
    Result := crLess else
    Result := C;
end;

function Compare(const I1, I2: Integer): TCompareResult;
begin
  if I1 < I2 then
    Result := crLess else
  if I1 > I2 then
    Result := crGreater else
    Result := crEqual;
end;

function Compare(const I1, I2: Int64): TCompareResult;
begin
  if I1 < I2 then
    Result := crLess else
  if I1 > I2 then
    Result := crGreater else
    Result := crEqual;
end;

function Compare(const I1, I2: Extended): TCompareResult;
begin
  if I1 < I2 then
    Result := crLess else
  if I1 > I2 then
    Result := crGreater else
    Result := crEqual;
end;

function Compare(const I1, I2: Boolean): TCompareResult;
begin
  if I1 = I2 then
    Result := crEqual else
  if I1 then
    Result := crGreater else
    Result := crLess;
end;

function Compare(const I1, I2: String): TCompareResult;
begin
  if I1 = I2 then
    Result := crEqual else
  if I1 > I2 then
    Result := crGreater else
    Result := crLess;
end;

function WideCompare(const I1, I2: WideString): TCompareResult;
begin
  if I1 = I2 then
    Result := crEqual else
  if I1 > I2 then
    Result := crGreater else
    Result := crLess;
end;



{                                                                              }
{ Approximate comparison                                                       }
{                                                                              }
function FloatZero(const A: Extended; const CompareDelta: Extended): Boolean;
begin
  Assert(CompareDelta >= 0.0, 'CompareDelta >= 0.0');
  Result := Abs(A) <= CompareDelta;
end;

function FloatOne(const A: Extended; const CompareDelta: Extended): Boolean;
begin
  Assert(CompareDelta >= 0.0, 'CompareDelta >= 0.0');
  Result := Abs(A - 1.0) <= CompareDelta;
end;

function FloatsEqual(const A, B: Extended; const CompareDelta: Extended): Boolean;
begin
  Assert(CompareDelta >= 0.0, 'CompareDelta >= 0.0');
  Result := Abs(A - B) <= CompareDelta;
end;

function FloatsCompare(const A, B: Extended; const CompareDelta: Extended): TCompareResult;
var D: Extended;
begin
  Assert(CompareDelta >= 0.0, 'CompareDelta >= 0.0');
  D := A - B;
  if Abs(D) <= CompareDelta then
    Result := crEqual else
  if D >= CompareDelta then
    Result := crGreater else
    Result := crLess;
end;



{                                                                              }
{ Scaled approximate comparison                                                }
{                                                                              }
{   The ApproxEqual and ApproxCompare functions were taken from the freeware   }
{   FltMath unit by Tempest Software, as taken from Knuth, Seminumerical       }
{   Algorithms, 2nd ed., Addison-Wesley, 1981, pp. 217-220.                    }
{                                                                              }
function ApproxEqual(const A, B: Extended; const CompareEpsilon: Double): Boolean;
var ExtA : TExtended absolute A;
    ExtB : TExtended absolute B;
    ExpA : Word;
    ExpB : Word;
    Exp  : TExtended;
begin
  ExpA := ExtA.Exponent and $7FFF;
  ExpB := ExtB.Exponent and $7FFF;
  if (ExpA = $7FFF) and
     ((ExtA.Mantissa[1] <> $80000000) or (ExtA.Mantissa[0] <> 0)) then
    { A is NaN }
    Result := False else
  if (ExpB = $7FFF) and
     ((ExtB.Mantissa[1] <> $80000000) or (ExtB.Mantissa[0] <> 0)) then
    { B is NaN }
    Result := False else
  if (ExpA = $7FFF) or (ExpB = $7FFF) then
    { A or B is infinity. Use the builtin comparison, which will       }
    { properly account for signed infinities, comparing infinity with  }
    { infinity, or comparing infinity with a finite value.             }
    Result := A = B else
  begin
    { We are comparing two finite values, so take the difference and   }
    { compare that against the scaled Epsilon.                         }
    Exp.Value := 1.0;
    if ExpA < ExpB then
      Exp.Exponent := ExpB else
      Exp.Exponent := ExpA;
    Result := Abs(A - B) <= (CompareEpsilon * Exp.Value);
  end;
end;

function ApproxCompare(const A, B: Extended; const CompareEpsilon: Double): TCompareResult;
var ExtA : TExtended absolute A;
    ExtB : TExtended absolute B;
    ExpA : Word;
    ExpB : Word;
    Exp  : TExtended;
    D, E : Extended;
begin
  ExpA := ExtA.Exponent and $7FFF;
  ExpB := ExtB.Exponent and $7FFF;
  if (ExpA = $7FFF) and
     ((ExtA.Mantissa[1] <> $80000000) or (ExtA.Mantissa[0] <> 0)) then
    { A is NaN }
    Result := crUndefined else
  if (ExpB = $7FFF) and
     ((ExtB.Mantissa[1] <> $80000000) or (ExtB.Mantissa[0] <> 0)) then
    { B is NaN }
    Result := crUndefined else
  if (ExpA = $7FFF) or (ExpB = $7FFF) then
    { A or B is infinity. Use the builtin comparison, which will       }
    { properly account for signed infinities, comparing infinity with  }
    { infinity, or comparing infinity with a finite value.             }
    Result := Compare(A, B) else
  begin
    { We are comparing two finite values, so take the difference and   }
    { compare that against the scaled Epsilon.                         }
    Exp.Value := 1.0;
    if ExpA < ExpB then
      Exp.Exponent := ExpB else
      Exp.Exponent := ExpA;
    E := CompareEpsilon * Exp.Value;
    D := A - B;
    if Abs(D) <= E then
      Result := crEqual else
    if D >= E then
      Result := crGreater else
      Result := crLess;
  end;
end;



{                                                                              }
{ Special floating-point values                                                }
{                                                                              }
function FloatIsInfinity(const A: Extended): Boolean;
var Ext : TExtended absolute A;
begin
  if Ext.Exponent and $7FFF <> $7FFF then
    Result := False
  else
    Result := (Ext.Mantissa[1] = $80000000) and (Ext.Mantissa[0] = 0);
end;

function FloatIsNaN(const A: Extended): Boolean;
var Ext : TExtended absolute A;
begin
  if Ext.Exponent and $7FFF <> $7FFF then
    Result := False
  else
    Result := (Ext.Mantissa[1] <> $80000000) or (Ext.Mantissa[0] <> 0)
end;



{                                                                              }
{ Base Conversion                                                              }
{                                                                              }
function LongWordToBase(const I: LongWord; const Digits, Base: Byte;
         const UpperCase: Boolean = True): String;
var D : LongWord;
    L : Byte;
    P : PChar;
    V : Byte;
begin
  Assert(Base <= 16, 'Maximum base is 16');
  if I = 0 then
    begin
      if Digits = 0 then
        L := 1
      else
        L := Digits;
      SetLength(Result, L);
      FillChar(Pointer(Result)^, L, '0');
      exit;
    end;
  L := 0;
  D := I;
  While D > 0 do
    begin
      Inc(L);
      D := D div Base;
    end;
  if L < Digits then
    L := Digits;
  SetLength(Result, L);
  P := Pointer(Result);
  Inc(P, L - 1);
  D := I;
  While D > 0 do
    begin
      V := D mod Base + 1;
      if UpperCase then
        P^ := s_HexDigitsUpper[V] else
        P^ := s_HexDigitsLower[V];
      Dec(P);
      Dec(L);
      D := D div Base;
    end;
  While L > 0 do
    begin
      P^ := '0';
      Dec(P);
      Dec(L);
    end;
end;

function LongWordToBin(const I: LongWord; const Digits: Byte): String;
begin
  Result := LongWordToBase(I, Digits, 2);
end;

function LongWordToOct(const I: LongWord; const Digits: Byte): String;
begin
  Result := LongWordToBase(I, Digits, 8);
end;

function LongWordToHex(const I: LongWord; const Digits: Byte;
         const UpperCase: Boolean): String;
begin
  Result := LongWordToBase(I, Digits, 16, UpperCase);
end;

function LongWordToStr(const I: LongWord; const Digits: Byte): String;
begin
  Result := LongWordToBase(I, Digits, 10);
end;

const
  HexLookup: Array[Char] of Byte = (
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, 10,  11,  12,  13,  14,  15,  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, 10,  11,  12,  13,  14,  15,  $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
      $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

function IsHexChar(const Ch: Char): Boolean;
begin
  Result := HexLookup[Ch] <= 15;
end;

function IsHexWideChar(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) <= $FF then
    Result := HexLookup[Char(Ch)] <= 15
  else
    Result := False;
end;

function HexCharValue(const Ch: Char): Byte;
begin
  Result := HexLookup[Ch];
end;

function HexWideCharValue(const Ch: WideChar): Byte;
begin
  if Ord(Ch) <= $FF then
    Result := HexLookup[Char(Ch)]
  else
    Result := $FF;
end;

function IsValidBaseStr(const S: String; const V: CharSet): Boolean;
var I : Integer;
    P : PChar;
begin
  I := Length(S);
  if I = 0 then
    begin
      Result := False;
      exit;
    end;
  P := Pointer(S);
  While I > 0 do
    if not (P^ in V) then
      begin
        Result := False;
        exit;
      end else
      begin
        Dec(I);
        Inc(P);
      end;
  Result := True;
end;

function IsValidBinStr(const S: String): Boolean;
begin
  Result := IsValidBaseStr(S, ['0'..'1']);
end;

function IsValidOctStr(const S: String): Boolean;
begin
  Result := IsValidBaseStr(S, ['0'..'7']);
end;

function IsValidDecStr(const S: String): Boolean;
begin
  Result := IsValidBaseStr(S, ['0'..'9']);
end;

function IsValidHexStr(const S: String): Boolean;
begin
  Result := IsValidBaseStr(S, ['0'..'9', 'A'..'F', 'a'..'f']);
end;

function BaseStrToLongWord(const S: String; const BaseLog2: Byte;
    var Valid: Boolean): LongWord;
var M : Byte;
    L : LongWord;
    P : Byte;
    C : Byte;
    Q : PChar;
begin
  Assert(BaseLog2 <= 4, 'BaseLog2 <= 4');
  P := Length(S);
  if P = 0 then // empty string is invalid
    begin
      Valid := False;
      Result := 0;
      exit;
    end;
  M := (1 shl BaseLog2) - 1; // maximum digit value
  L := 0;
  Result := 0;
  Q := Pointer(S);
  Inc(Q, P - 1);
  Repeat
    C := HexLookup[Q^];
    if C > M then // invalid digit
      begin
        Valid := False;
        Result := 0;
        exit;
      end;
    Inc(Result, LongWord(C) shl L);
    Inc(L, BaseLog2);
    if L > 32 then // overflow
      begin
        Valid := False;
        Result := 0;
        exit;
      end;
    Dec(P);
    Dec(Q);
  Until P = 0;
  Valid := True;
end;

function BinStrToLongWord(const S: String; var Valid: Boolean): LongWord;
begin
  Result := BaseStrToLongWord(S, 1, Valid);
end;

function OctStrToLongWord(const S: String; var Valid: Boolean): LongWord;
begin
  Result := BaseStrToLongWord(S, 3, Valid);
end;

function HexStrToLongWord(const S: String; var Valid: Boolean): LongWord;
begin
  Result := BaseStrToLongWord(S, 4, Valid);
end;

function DecStrToLongWord(const S: String; var Valid: Boolean): LongWord;
var L : Integer;
    P : PChar;
    C : Char;
    F : LongWord;
    R : Int64;
begin
  L := Length(S);
  if L = 0 then // empty string
    begin
      Result := 0;
      Valid := False;
      exit;
    end;
  R := 0;
  F := 1;
  P := Pointer(S);
  Inc(P, L - 1);
  Repeat
    C := P^;
    if not (C in ['0'..'9']) then // invalid character
      begin
        Valid := False;
        Result := 0;
        exit;
      end;
    Inc(R, Int64(Ord(C) - Ord('0')) * F);
    if R > MaxLongWord then // overflow, value too large
      begin
        Valid := False;
        Result := 0;
        exit;
      end;
    Dec(P);
    Dec(L);
    if L > 0 then
      begin
        if F = 1000000000 then // overflow, too many digits
          begin
            Valid := False;
            Result := 0;
            exit;
          end;
        F := F * 10;
      end;
  Until L = 0;
  Valid := True;
  Result := LongWord(R);
end;

function EncodeBase64(const S, Alphabet: String; const Pad: Boolean; const PadMultiple: Integer; const PadChar: Char): String;
var R, C : Byte;
    F, L, M, N, U : Integer;
    P : PChar;
    T : Boolean;
begin
  Assert(Length(Alphabet) = 64, 'Alphabet must contain 64 characters');
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  M := L mod 3;
  N := (L div 3) * 4 + M;
  if M > 0 then
    Inc(N);
  T := Pad and (PadMultiple > 1);
  if T then
    begin
      U := N mod PadMultiple;
      if U > 0 then
        begin
          U := PadMultiple - U;
          Inc(N, U);
        end;
    end else
    U := 0;
  SetLength(Result, N);
  P := Pointer(Result);
  R := 0;
  For F := 0 to L - 1 do
    begin
      C := Byte(S [F + 1]);
      Case F mod 3 of
        0 : begin
              P^ := Alphabet[C shr 2 + 1];
              Inc(P);
              R := (C and 3) shl 4;
            end;
        1 : begin
              P^ := Alphabet[C shr 4 + R + 1];
              Inc(P);
              R := (C and $0F) shl 2;
            end;
        2 : begin
              P^ := Alphabet[C shr 6 + R + 1];
              Inc(P);
              P^ := Alphabet[C and $3F + 1];
              Inc(P);
            end;
      end;
    end;
  if M > 0 then
    begin
      P^ := Alphabet[R + 1];
      Inc(P);
    end;
  For F := 1 to U do
    begin
      P^ := PadChar;
      Inc(P);
    end;
end;

function DecodeBase64(const S, Alphabet: String; const PadSet: CharSet): String;
var F, L, M, P : Integer;
    B, OutPos  : Byte;
    OutB       : Array[1..3] of Byte;
    Lookup     : Array[Char] of Byte;
    R          : PChar;
begin
  Assert(Length(Alphabet) = 64, 'Alphabet must contain 64 characters');
  L := Length(S);
  P := 0;
  if PadSet <> [] then
    While (L - P > 0) and (S[L - P] in PadSet) do
      Inc(P);
  M := L - P;
  if M = 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, (M * 3) div 4);
  FillChar(Lookup, Sizeof(Lookup), #0);
  For F := 0 to 63 do
    Lookup[Alphabet[F + 1]] := Byte(F);
  R := Pointer(Result);
  OutPos := 0;
  For F := 1 to L - P do
    begin
      B := Lookup[S[F]];
      Case OutPos of
          0 : OutB[1] := B shl 2;
          1 : begin
                OutB[1] := OutB[1] or (B shr 4);
                R^ := Char(OutB[1]);
                Inc(R);
                OutB[2] := (B shl 4) and $FF;
              end;
          2 : begin
                OutB[2] := OutB[2] or (B shr 2);
                R^ := Char(OutB[2]);
                Inc(R);
                OutB[3] := (B shl 6) and $FF;
              end;
          3 : begin
                OutB[3] := OutB[3] or B;
                R^ := Char(OutB[3]);
                Inc(R);
              end;
        end;
      OutPos := (OutPos + 1) mod 4;
    end;
  if (OutPos > 0) and (P = 0) then // incomplete encoding, add the partial byte if not 0
    if OutB[OutPos] <> 0 then
      Result := Result + Char(OutB[OutPos]);
end;

function MIMEBase64Encode(const S: String): String;
begin
  Result := EncodeBase64(S, b64_MIMEBase64, True, 4, '=');
end;

function UUDecode(const S: String): String;
begin
  // Line without size indicator (first byte = length + 32)
  Result := DecodeBase64(S, b64_UUEncode, ['`']);
end;

function MIMEBase64Decode(const S: String): String;
begin
  Result := DecodeBase64(S, b64_MIMEBase64, ['=']);
end;

function XXDecode(const S: String): String;
begin
  Result := DecodeBase64(S, b64_XXEncode, []);
end;

function BytesToHex(const P: Pointer; const Count: Integer;
         const UpperCase: Boolean): String;
var Q : PByte;
    D : PChar;
    L : Integer;
    V : Byte;
begin
  Q := P;
  L := Count;
  if (L <= 0) or not Assigned(Q) then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count * 2);
  D := Pointer(Result);
  While L > 0 do
    begin
      V := Q^ shr 4 + 1;
      if UpperCase then
        D^ := s_HexDigitsUpper[V] else
        D^ := s_HexDigitsLower[V];
      Inc(D);
      V := Q^ and $F + 1;
      if UpperCase then
        D^ := s_HexDigitsUpper[V] else
        D^ := s_HexDigitsLower[V];
      Inc(D);
      Inc(Q);
      Dec(L);
    end;
end;



{                                                                              }
{ Type conversion                                                              }
{                                                                              }
function PointerToStr(const P: Pointer): String;
begin
  Result := '$' + LongWordToHex(LongWord(P), 8);
end;

function StrToPointer(const S: String): Pointer;
var V : Boolean;
begin
  Result := Pointer(HexStrToLongWord(S, V));
end;

function ObjectClassName(const O: TObject): String;
begin
  if not Assigned(O) then
    Result := 'nil' else
    Result := O.ClassName;
end;

function ClassClassName(const C: TClass): String;
begin
  if not Assigned(C) then
    Result := 'nil' else
    Result := C.ClassName;
end;

function ObjectToStr(const O: TObject): String;
begin
  if not Assigned(O) then
    Result := 'nil' else
    Result := O.ClassName + '@' + LongWordToHex(LongWord(O), 8);
end;

function ClassToStr(const C: TClass): String;
begin
  if not Assigned(C) then
    Result := 'nil' else
    Result := C.ClassName + '@' + LongWordToHex(LongWord(C), 8);
end;

{$IFDEF USE_ASM386}
function CharSetToStr(const C: CharSet): String; // Andrew N. Driazgov
asm
      PUSH    EBX
      MOV     ECX, $100
      MOV     EBX, EAX
      PUSH    ESI
      MOV     EAX, EDX
      SUB     ESP, ECX
      XOR     ESI, ESI
      XOR     EDX, EDX
@@lp: BT      [EBX], EDX
      JC      @@mm
@@nx: INC     EDX
      DEC     ECX
      JNE     @@lp
      MOV     ECX, ESI
      MOV     EDX, ESP
      CALL    System.@LStrFromPCharLen
      ADD     ESP, $100
      POP     ESI
      POP     EBX
      RET
@@mm: MOV     [ESP + ESI], DL
      INC     ESI
      JMP     @@nx
end;
{$ELSE}
function CharSetToStr(const C: CharSet): String;
// Implemented recursively to avoid multiple memory allocations
  procedure CharMatch(const Start: Char; const Count: Integer);
  var Ch : Char;
  begin
    For Ch := Start to #255 do
      if Ch in C then
        begin
          if Ch = #255 then
            SetLength(Result, Count + 1) else
            CharMatch(Char(Byte(Ch) + 1), Count + 1);
          Result[Count + 1] := Ch;
          exit;
        end;
    SetLength(Result, Count);
  end;
begin
  CharMatch(#0, 0);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function StrToCharSet(const S: String): CharSet; // Andrew N. Driazgov
asm
      XOR     ECX, ECX
      MOV     [EDX], ECX
      MOV     [EDX + 4], ECX
      MOV     [EDX + 8], ECX
      MOV     [EDX + 12], ECX
      MOV     [EDX + 16], ECX
      MOV     [EDX + 20], ECX
      MOV     [EDX + 24], ECX
      MOV     [EDX + 28], ECX
      TEST    EAX, EAX
      JE      @@qt
      MOV     ECX, [EAX - 4]
      PUSH    EBX
      SUB     ECX, 8
      JS      @@nx
@@lp: MOVZX   EBX, BYTE PTR [EAX]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 1]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 2]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 3]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 4]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 5]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 6]
      BTS     [EDX], EBX
      MOVZX   EBX, BYTE PTR [EAX + 7]
      BTS     [EDX], EBX
      ADD     EAX, 8
      SUB     ECX, 8
      JNS     @@lp
@@nx: JMP     DWORD PTR @@tV[ECX * 4 + 32]
@@tV: DD      @@ex, @@t1, @@t2, @@t3
      DD      @@t4, @@t5, @@t6, @@t7
@@t7: MOVZX   EBX, BYTE PTR [EAX + 6]
      BTS     [EDX], EBX
@@t6: MOVZX   EBX, BYTE PTR [EAX + 5]
      BTS     [EDX], EBX
@@t5: MOVZX   EBX, BYTE PTR [EAX + 4]
      BTS     [EDX], EBX
@@t4: MOVZX   EBX, BYTE PTR [EAX + 3]
      BTS     [EDX], EBX
@@t3: MOVZX   EBX, BYTE PTR [EAX + 2]
      BTS     [EDX], EBX
@@t2: MOVZX   EBX, BYTE PTR [EAX + 1]
      BTS     [EDX], EBX
@@t1: MOVZX   EBX, BYTE PTR [EAX]
      BTS     [EDX], EBX
@@ex: POP     EBX
@@qt:
end;
{$ELSE}
function StrToCharSet(const S: String): CharSet;
var I: Integer;
begin
  ClearCharSet(Result);
  For I := 1 to Length(S) do
    Include(Result, S [I]);
end;
{$ENDIF}



{                                                                              }
{ Hash functions                                                               }
{   Derived from a CRC32 algorithm.                                            }
{                                                                              }
var
  HashTableInit   : Boolean = False;
  HashTable       : Array[Byte] of LongWord;
  HashTableNoCase : Array[Byte] of LongWord;
  HashPoly        : LongWord = $EDB88320;

procedure InitHashTable;
var I, J : Byte;
    R    : LongWord;
begin
  For I := $00 to $FF do
    begin
      R := I;
      For J := 8 downto 1 do
        if R and 1 <> 0 then
          R := (R shr 1) xor HashPoly
        else
          R := R shr 1;
      HashTable[I] := R;
    end;
  Move(HashTable, HashTableNoCase, Sizeof(HashTable));
  For I := Ord('A') to Ord('Z') do
    HashTableNoCase[I] := HashTableNoCase[I or 32];
  HashTableInit := True;
end;

function Hash(const Hash: LongWord; const Buf; const BufSize: Integer): LongWord;
var P : PByte;
    I : Integer;
begin
  P := @Buf;
  Result := Hash;
  For I := 1 to BufSize do
    begin
      Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
      Inc(P);
    end;
end;

function HashNoCase(const Hash: LongWord; const Buf; const BufSize: Integer): LongWord;
var P : PByte;
    I : Integer;
begin
  P := @Buf;
  Result := Hash;
  For I := 1 to BufSize do
    begin
      Result := HashTableNoCase[P^] xor (Result shr 8);
      Inc(P);
    end;
end;

function HashBuf(const Buf; const BufSize: Integer; const Slots: LongWord): LongWord;
begin
  if not HashTableInit then
    InitHashTable;
  Result := not Hash($FFFFFFFF, Buf, BufSize);
  // Mod into slots
  if Slots <> 0 then
    Result := Result mod Slots;
end;

function HashStrBuf(const StrBuf: Pointer; const StrLength: Integer;
    const Slots: LongWord): LongWord;
var P    : PChar;
    I, J : Integer;
begin
  if not HashTableInit then
    InitHashTable;
  P := StrBuf;
  if StrLength <= 48 then // Hash all characters for short strings
    Result := Hash($FFFFFFFF, P^, StrLength)
  else
    begin
      // Hash first 16 bytes
      Result := Hash($FFFFFFFF, P^, 16);
      // Hash last 16 bytes
      Inc(P, StrLength - 16);
      Result := Hash(Result, P^, 16);
      // Hash 16 bytes sampled from rest of string
      I := (StrLength - 48) div 16;
      P := StrBuf;
      Inc(P, 16);
      For J := 1 to 16 do
        begin
          Result := HashTable[Byte(Result) xor Byte(P^)] xor (Result shr 8);
          Inc(P, I + 1);
        end;
    end;
  // Mod into slots
  if Slots <> 0 then
    Result := Result mod Slots;
end;

function HashStrBufNoCase(const StrBuf: Pointer; const StrLength: Integer;
    const Slots: LongWord): LongWord;
var P    : PChar;
    I, J : Integer;
begin
  if not HashTableInit then
    InitHashTable;
  P := StrBuf;
  if StrLength <= 48 then // Hash all characters for short strings
    Result := HashNoCase($FFFFFFFF, P^, StrLength)
  else
    begin
      // Hash first 16 bytes
      Result := HashNoCase($FFFFFFFF, P^, 16);
      // Hash last 16 bytes
      Inc(P, StrLength - 16);
      Result := HashNoCase(Result, P^, 16);
      // Hash 16 bytes sampled from rest of string
      I := (StrLength - 48) div 16;
      P := StrBuf;
      Inc(P, 16);
      For J := 1 to 16 do
        begin
          Result := HashTableNoCase[Byte(P^)] xor (Result shr 8);
          Inc(P, I + 1);
        end;
    end;
  // Mod into slots
  if Slots <> 0 then
    Result := Result mod Slots;
end;

function HashStr(const S: String; const Slots: LongWord; const CaseSensitive: Boolean): LongWord;
begin
  if CaseSensitive then
    Result := HashStrBuf(Pointer(S), Length(S), Slots)
  else
    Result := HashStrBufNoCase(Pointer(S), Length(S), Slots);
end;

{ HashInteger based on the CRC32 algorithm. It is a very good all purpose hash }
{ with a highly uniform distribution of results.                               }
function HashInteger(const I: Integer; const Slots: LongWord): LongWord;
var P : PByte;
begin
  if not HashTableInit then
    InitHashTable;
  Result := $FFFFFFFF;
  P := @I;
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  if Slots <> 0 then
    Result := Result mod Slots;
end;

function HashLongWord(const I: LongWord; const Slots: LongWord): LongWord;
var P : PByte;
begin
  if not HashTableInit then
    InitHashTable;
  Result := $FFFFFFFF;
  P := @I;
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  Inc(P);
  Result := HashTable[Byte(Result) xor P^] xor (Result shr 8);
  if Slots <> 0 then
    Result := Result mod Slots;
end;



{                                                                              }
{ Memory                                                                       }
{                                                                              }
{$IFDEF USE_ASM386}
procedure FillMem(var Buf; const Count: Integer; const Value: Byte);
asm
    OR     EDX, EDX
    JLE    @Fin
    PUSH   EDI
    MOV    EDI, EAX
    MOV    CH, CL
    MOV    EAX, ECX
    SHL    EAX, 16
    MOV    AX, CX
    CMP    EDX, 12
    JBE    @SmallFillMem
  @GeneralFillMem:
    MOV    ECX, EDX
    SHR    ECX, 2
    REP    STOSD
    AND    EDX, 3
  @SmallFillMem:
    JMP    DWORD PTR @JumpTable[EDX * 4]
  @JumpTable:
    DD     @Fill0,  @Fill1,  @Fill2,  @Fill3
    DD     @Fill4,  @Fill5,  @Fill6,  @Fill7
    DD     @Fill8,  @Fill9,  @Fill10, @Fill11
    DD     @Fill12
  @Fill12:
    MOV    DWORD PTR [EDI], EAX
    MOV    DWORD PTR [EDI + 4], EAX
    MOV    DWORD PTR [EDI + 8], EAX
    POP    EDI
    RET
  @Fill11:
    MOV    BYTE PTR [EDI + 10], AL
  @Fill10:
    MOV    DWORD PTR [EDI], EAX
    MOV    DWORD PTR [EDI + 4], EAX
    MOV    WORD PTR [EDI + 8], AX
    POP    EDI
    RET
  @Fill9:
    MOV    BYTE PTR [EDI + 8], AL
  @Fill8:
    MOV    DWORD PTR [EDI], EAX
    MOV    DWORD PTR [EDI + 4], EAX
    POP    EDI
    RET
  @Fill7:
    MOV    BYTE PTR [EDI + 6], AL
  @Fill6:
    MOV    DWORD PTR [EDI], EAX
    MOV    WORD PTR [EDI + 4], AX
    POP    EDI
    RET
  @Fill5:
    MOV    BYTE PTR [EDI + 4], AL
  @Fill4:
    MOV    DWORD PTR [EDI], EAX
    POP    EDI
    RET
  @Fill3:
    MOV    BYTE PTR [EDI + 2], AL
  @Fill2:
    MOV    WORD PTR [EDI], AX
    POP    EDI
    RET
  @Fill1:
    MOV    BYTE PTR [EDI], AL
  @Fill0:
    POP    EDI
  @Fin:
    RET
end;
{$ELSE}
procedure FillMem(var Buf; const Count: Integer; const Value: Byte);
begin
  FillChar(Buf, Count, Value);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure ZeroMem(var Buf; const Count: Integer);
asm
    OR     EDX, EDX
    JLE    @Zero0
    CMP    EDX, 12
    JA     @GeneralZeroMem
    XOR    ECX, ECX
    JMP    DWORD PTR @JumpTable[EDX * 4]
  @JumpTable:
    DD     @Zero0,  @Zero1,  @Zero2,  @Zero3
    DD     @Zero4,  @Zero5,  @Zero6,  @Zero7
    DD     @Zero8,  @Zero9,  @Zero10, @Zero11
    DD     @Zero12
  @Zero12:
    MOV    DWORD PTR [EAX], ECX
    MOV    DWORD PTR [EAX + 4], ECX
    MOV    DWORD PTR [EAX + 8], ECX
    RET
  @Zero11:
    MOV    BYTE PTR [EAX + 10], CL
  @Zero10:
    MOV    DWORD PTR [EAX], ECX
    MOV    DWORD PTR [EAX + 4], ECX
    MOV    WORD PTR [EAX + 8], CX
    RET
  @Zero9:
    MOV    BYTE PTR [EAX + 8], CL
  @Zero8:
    MOV    DWORD PTR [EAX], ECX
    MOV    DWORD PTR [EAX + 4], ECX
    RET
  @Zero7:
    MOV    BYTE PTR [EAX + 6], CL
  @Zero6:
    MOV    DWORD PTR [EAX], ECX
    MOV    WORD PTR [EAX + 4], CX
    RET
  @Zero5:
    MOV    BYTE PTR [EAX + 4], CL
  @Zero4:
    MOV    DWORD PTR [EAX], ECX
    RET
  @Zero3:
    MOV    BYTE PTR [EAX + 2], CL
  @Zero2:
    MOV    WORD PTR [EAX], CX
    RET
  @Zero1:
    MOV    BYTE PTR [EAX], CL
  @Zero0:
    RET
  @GeneralZeroMem:
    PUSH   EDI
    MOV    EDI, EAX
    XOR    EAX, EAX
    MOV    ECX, EDX
    SHR    ECX, 2
    REP    STOSD
    AND    EDX, 3
    XOR    ECX, ECX
    MOV    EAX, EDI
    POP    EDI
    JMP    DWORD PTR @JumpTable[EDX * 4]
end;
{$ELSE}
procedure ZeroMem(var Buf; const Count: Integer);
begin
  FillChar(Buf, Count, #0);
end;
{$ENDIF}

{$IFDEF USE_ASM386}
procedure MoveMem(const Source; var Dest; const Count: Integer);
asm
    OR     ECX, ECX
    JLE    @Move0
    CMP    ECX, 10
    JA     @GeneralMove
    JMP    DWORD PTR @JumpTable[ECX * 4]
  @JumpTable:
    DD     @Move0,  @Move1,        @Move2,   @Move3
    DD     @Move4,  @Move5,        @Move6,   @GeneralMove
    DD     @Move8,  @GeneralMove,  @Move10,  @GeneralMove
    DD     @Move12
  @Move12:
    MOV    ECX, [EAX]
    MOV    [EDX], ECX
    MOV    ECX, [EAX + 4]
    MOV    EAX, [EAX + 8]
    MOV    [EDX + 4], ECX
    MOV    [EDX + 8], EAX
    RET
  @Move10:
    MOV    ECX, [EAX]
    MOV    [EDX], ECX
    MOV    ECX, [EAX + 4]
    MOV    AX, [EAX + 8]
    MOV    [EDX + 4], ECX
    MOV    [EDX + 8], AX
    RET
  @Move8:
    MOV    ECX, [EAX]
    MOV    EAX, [EAX + 4]
    MOV    [EDX], ECX
    MOV    [EDX + 4], EAX
    RET
  @Move6:
    MOV    ECX, [EAX]
    MOV    AX, [EAX + 4]
    MOV    [EDX], ECX
    MOV    [EDX + 4], AX
    RET
  @Move5:
    MOV    ECX, [EAX]
    MOV    AL, [EAX + 4]
    MOV    [EDX], ECX
    MOV    [EDX + 4], AL
    RET
  @Move4:
    MOV    ECX, [EAX]
    MOV    [EDX], ECX
    RET
  @Move3:
    MOV    CX, [EAX]
    MOV    AL, [EAX + 2]
    MOV    [EDX], CX
    MOV    [EDX + 2], AL
    RET
  @Move2:
    MOV    CX, [EAX]
    MOV    [EDX], CX
    RET
  @Move1:
    MOV    CL, [EAX]
    MOV    [EDX], CL
    RET
  @GeneralMove:
    CALL   Move
  @Move0:
    RET
end;
{$ELSE}
procedure MoveMem(const Source; var Dest; const Count: Integer);
begin
  if Count <= 0 then
    exit;
  if Count > 4 then
    Move(Source, Dest, Count)
  else
    Case Count of // optimization for small moves
      1 : PByte(@Dest)^ := PByte(@Source)^;
      2 : PWord(@Dest)^ := PWord(@Source)^;
      4 : PLongWord(@Dest)^ := PLongWord(@Source)^;
    else
      Move(Source, Dest, Count);
    end;
end;
{$ENDIF}

{$IFDEF USE_ASM386}
function CompareMem(const Buf1; const Buf2; const Count: Integer): Boolean; assembler;
asm
    PUSH    ESI
    PUSH    EDI
    MOV     ESI, Buf1
    MOV     EDI, Buf2
    MOV     EDX, ECX
    XOR     EAX, EAX
    AND     EDX, 3
    SHR     ECX, 1
    SHR     ECX, 1
    REPE    CMPSD
    JNE     @Fin
    MOV     ECX, EDX
    REPE    CMPSB
    JNE     @Fin
    INC     EAX
  @Fin:
    POP     EDI
    POP     ESI
end;
{$ELSE}
function CompareMem(const Buf1; const Buf2; const Count: Integer): Boolean;
var P, Q : Pointer;
    D, I : Integer;
begin
  if Count <= 0 then
    begin
      Result := True;
      exit;
    end;
  P := @Buf1;
  Q := @Buf2;
  D := LongWord(Count) div 4;
  For I := 1 to D do
    if PLongWord(P)^ = PLongWord(Q)^ then
      begin
        Inc(PLongWord(P));
        Inc(PLongWord(Q));
      end else
      begin
        Result := False;
        exit;
      end;
  D := LongWord(Count) and 3;
  For I := 1 to D do
    if PByte(P)^ = PByte(Q)^ then
      begin
        Inc(PByte(P));
        Inc(PByte(Q));
      end else
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;
{$ENDIF}

function CompareMemNoCase(const Buf1; const Buf2; const Count: Integer): TCompareResult;
var P, Q : Pointer;
    I    : Integer;
    C, D : Byte;
begin
  if Count <= 0 then
    begin
      Result := crEqual;
      exit;
    end;
  P := @Buf1;
  Q := @Buf2;
  For I := 1 to Count do
    begin
      C := PByte(P)^;
      D := PByte(Q)^;
      if C in [Ord('A')..Ord('Z')] then
        C := C or 32;
      if D in [Ord('A')..Ord('Z')] then
        D := D or 32;
      if C = D then
        begin
          Inc(PByte(P));
          Inc(PByte(Q));
        end
      else
        begin
          if C < D then
            Result := crLess else
            Result := crGreater;
          exit;
        end;
    end;
  Result := crEqual;
end;

procedure ReverseMem(var Buf; const Size: Integer);
var I : Integer;
    P : PByte;
    Q : PByte;
    T : Byte;
begin
  P := @Buf;
  Q := P;
  Inc(Q, Size - 1);
  For I := 1 to Size div 2 do
    begin
      T := P^;
      P^ := Q^;
      Q^ := T;
      Inc(P);
      Dec(Q);
    end;
end;



{                                                                              }
{ Append                                                                       }
{                                                                              }
function Append(var V: ByteArray; const R: Byte): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: WordArray; const R: Word): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: LongWordArray; const R: LongWord): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: ShortIntArray; const R: ShortInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: SmallIntArray; const R: SmallInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: LongIntArray; const R: LongInt): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: Int64Array; const R: Int64): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: SingleArray; const R: Single): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: DoubleArray; const R: Double): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: ExtendedArray; const R: Extended): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: CurrencyArray; const R: Currency): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: StringArray; const R: String): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: WideStringArray; const R: WideString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: BooleanArray; const R: Boolean): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: PointerArray; const R: Pointer): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: ObjectArray; const R: TObject): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: InterfaceArray; const R: IInterface): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: ByteSetArray; const R: ByteSet): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function Append(var V: CharSetArray; const R: CharSet): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;


function AppendByteArray(var V: ByteArray; const R: Array of Byte): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Byte) * L);
    end;
end;

function AppendWordArray(var V: WordArray; const R: Array of Word): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Word) * L);
    end;
end;

function AppendCardinalArray(var V: CardinalArray; const R: Array of LongWord): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(LongWord) * L);
    end;
end;

function AppendShortIntArray(var V: ShortIntArray; const R: Array of ShortInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(ShortInt) * L);
    end;
end;

function AppendSmallIntArray(var V: SmallIntArray; const R: Array of SmallInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(SmallInt) * L);
    end;
end;

function AppendIntegerArray(var V: IntegerArray; const R: Array of LongInt): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(LongInt) * L);
    end;
end;

function AppendInt64Array(var V: Int64Array; const R: Array of Int64): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Int64) * L);
    end;
end;

function AppendSingleArray(var V: SingleArray; const R: Array of Single): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Single) * L);
    end;
end;

function AppendDoubleArray(var V: DoubleArray; const R: Array of Double): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Double) * L);
    end;
end;

function AppendExtendedArray(var V: ExtendedArray; const R: Array of Extended): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Extended) * L);
    end;
end;

function AppendCurrencyArray(var V: CurrencyArray; const R: Array of Currency): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Currency) * L);
    end;
end;

function AppendPointerArray(var V: PointerArray; const R: Array of Pointer): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(Pointer) * L);
    end;
end;

function AppendCharSetArray(var V: CharSetArray; const R: Array of CharSet): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(CharSet) * L);
    end;
end;

function AppendByteSetArray(var V: ByteSetArray; const R: Array of ByteSet): Integer;
var L : Integer;
begin
  Result := Length(V);
  L := Length(R);
  if L > 0 then
    begin
      SetLength(V, Result + L);
      Move(R[0], V[Result], Sizeof(ByteSet) * L);
    end;
end;


function AppendObjectArray(var V: ObjectArray; const R: ObjectArray): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      For I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;

function AppendStringArray(var V: StringArray; const R: Array of String): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      For I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;


  
{                                                                              }
{ FreeAndNil                                                                   }
{                                                                              }
procedure FreeAndNil(var Obj);
var Temp : TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;



{                                                                              }
{ Remove                                                                       }
{                                                                              }
function Remove(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Byte));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: WordArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Word));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(LongWord));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(ShortInt));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(SmallInt));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(LongInt));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Int64));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Single));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Double));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Extended));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Currency));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Pointer));
  SetLength(V, L - J);
  Result := J;
end;


function Remove(var V: ObjectArray; const Idx: Integer; const Count: Integer; const FreeObjects: Boolean): Integer;
var I, J, K, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  if FreeObjects then
    For K := I to I + J - 1 do
      FreeAndNil(V[K]);
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(Pointer));
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: StringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  For K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: WideStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  For K := I to L - J - 1 do
    V[K] := V[K + J];
  SetLength(V, L - J);
  Result := J;
end;

function Remove(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer;
var I, J, K, L, M : Integer;
begin
  L := Length(V);
  if (Idx >= L) or (Idx + Count <= 0) or (L = 0) or (Count = 0) then
    begin
      Result := 0;
      exit;
    end;
  I := MaxI(Idx, 0);
  J := MinI(Count, L - I);
  For K := I to I + J - 1 do
    V[K] := nil;
  M := L - J - I;
  if M > 0 then
    Move(V[I + J], V[I], M * SizeOf(IInterface));
  FillChar(V[L - J], J * SizeOf(IInterface), #0);
  SetLength(V, L - J);
  Result := J;
end;

procedure FreeObjectArray(var V);
var I : Integer;
    A : ObjectArray absolute V;
begin
  For I := Length(A) - 1 downto 0 do
    FreeAndNil(A[I]);
end;

procedure FreeObjectArray(var V; const LoIdx, HiIdx: Integer);
var I : Integer;
    A : ObjectArray absolute V;
begin
  For I := HiIdx downto LoIdx do
    FreeAndNil(A[I]);
end;

// Note: The parameter can not be changed to be untyped and then typecasted
// using an absolute variable, as in FreeObjectArray. The reference counting
// will be done incorrectly.
procedure FreeAndNilObjectArray(var V: ObjectArray);
var W : ObjectArray;
begin
  W := V;
  V := nil;
  FreeObjectArray(W);
end;


{                                                                              }
{ RemoveDuplicates                                                             }
{                                                                              }
procedure RemoveDuplicates(var V: ByteArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Byte;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: WordArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Word;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: LongWordArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : LongWord;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: ShortIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : ShortInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: SmallIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : SmallInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: LongIntArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : LongInt;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: Int64Array; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Int64;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: SingleArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Single;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: DoubleArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Double;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: ExtendedArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Extended;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: StringArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : String;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;

procedure RemoveDuplicates(var V: PointerArray; const IsSorted: Boolean);
var I, C, J, L : Integer;
    F          : Pointer;
begin
  L := Length(V);
  if L = 0 then
    exit;

  if IsSorted then
    begin
      J := 0;
      Repeat
        F := V[J];
        I := J + 1;
        While (I < L) and (V[I] = F) do
          Inc(I);
        C := I - J;
        if C > 1 then
          begin
            Remove(V, J + 1, C - 1);
            Dec(L, C - 1);
            Inc(J);
          end
        else
          J := I;
      Until J >= L;
    end else
    begin
      J := 0;
      Repeat
        Repeat
          I := PosNext(V[J], V, J);
          if I >= 0 then
            Remove(V, I, 1);
        Until I < 0;
        Inc(J);
      Until J >= Length(V);
    end;
end;



procedure TrimArrayLeft(var S: ByteArray; const TrimList: Array of Byte); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: WordArray; const TrimList: Array of Word); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: LongWordArray; const TrimList: Array of LongWord); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: ShortIntArray; const TrimList: Array of ShortInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: SmallIntArray; const TrimList: Array of SmallInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: LongIntArray; const TrimList: Array of LongInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: Int64Array; const TrimList: Array of Int64); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: SingleArray; const TrimList: Array of Single); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: DoubleArray; const TrimList: Array of Double); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: ExtendedArray; const TrimList: Array of Extended); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: StringArray; const TrimList: Array of String); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;


procedure TrimArrayLeft(var S: PointerArray; const TrimList: Array of Pointer); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := 0;
  R := True;
  While R and (I < Length(S)) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Inc(I);
            break;
          end;
    end;
  if I > 0 then
    Remove(S, 0, I - 1);
end;



procedure TrimArrayRight(var S: ByteArray; const TrimList: Array of Byte); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: WordArray; const TrimList: Array of Word); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: LongWordArray; const TrimList: Array of LongWord); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: ShortIntArray; const TrimList: Array of ShortInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: SmallIntArray; const TrimList: Array of SmallInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: LongIntArray; const TrimList: Array of LongInt); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: Int64Array; const TrimList: Array of Int64); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: SingleArray; const TrimList: Array of Single); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: DoubleArray; const TrimList: Array of Double); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: ExtendedArray; const TrimList: Array of Extended); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: StringArray; const TrimList: Array of String); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;


procedure TrimArrayRight(var S: PointerArray; const TrimList: Array of Pointer); overload;
var I, J : Integer;
    R    : Boolean;
begin
  I := Length(S) - 1;
  R := True;
  While R and (I >= 0) do
    begin
      R := False;
      For J := 0 to High(TrimList) do
        if S[I] = TrimList[J] then
          begin
            R := True;
            Dec(I);
            break;
          end;
    end;
  if I < Length(S) - 1 then
    SetLength(S, I + 1);
end;



{                                                                              }
{ ArrayInsert                                                                  }
{                                                                              }
function ArrayInsert(var V: ByteArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Byte));
  FillChar(P^, Count * Sizeof(Byte), #0);
  Result := I;
end;

function ArrayInsert(var V: WordArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Word));
  FillChar(P^, Count * Sizeof(Word), #0);
  Result := I;
end;

function ArrayInsert(var V: LongWordArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(LongWord));
  FillChar(P^, Count * Sizeof(LongWord), #0);
  Result := I;
end;

function ArrayInsert(var V: ShortIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(ShortInt));
  FillChar(P^, Count * Sizeof(ShortInt), #0);
  Result := I;
end;

function ArrayInsert(var V: SmallIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(SmallInt));
  FillChar(P^, Count * Sizeof(SmallInt), #0);
  Result := I;
end;

function ArrayInsert(var V: LongIntArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(LongInt));
  FillChar(P^, Count * Sizeof(LongInt), #0);
  Result := I;
end;

function ArrayInsert(var V: Int64Array; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Int64));
  FillChar(P^, Count * Sizeof(Int64), #0);
  Result := I;
end;

function ArrayInsert(var V: SingleArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Single));
  FillChar(P^, Count * Sizeof(Single), #0);
  Result := I;
end;

function ArrayInsert(var V: DoubleArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Double));
  FillChar(P^, Count * Sizeof(Double), #0);
  Result := I;
end;

function ArrayInsert(var V: ExtendedArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Extended));
  FillChar(P^, Count * Sizeof(Extended), #0);
  Result := I;
end;

function ArrayInsert(var V: CurrencyArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Currency));
  FillChar(P^, Count * Sizeof(Currency), #0);
  Result := I;
end;

function ArrayInsert(var V: StringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(String));
  FillChar(P^, Count * Sizeof(String), #0);
  Result := I;
end;

function ArrayInsert(var V: WideStringArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(WideString));
  FillChar(P^, Count * Sizeof(WideString), #0);
  Result := I;
end;

function ArrayInsert(var V: PointerArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Pointer));
  FillChar(P^, Count * Sizeof(Pointer), #0);
  Result := I;
end;

function ArrayInsert(var V: ObjectArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(Pointer));
  FillChar(P^, Count * Sizeof(Pointer), #0);
  Result := I;
end;

function ArrayInsert(var V: InterfaceArray; const Idx: Integer; const Count: Integer): Integer;
var I, L : Integer;
    P    : Pointer;
begin
  L := Length(V);
  if (Idx > L) or (Idx + Count <= 0) or (Count <= 0) then
    begin
      Result := -1;
      exit;
    end;
  SetLength(V, L + Count);
  I := Idx;
  if I < 0 then
    I := 0;
  P := @V[I];
  if I < L then
    Move(P^, V[I + Count], (L - I) * Sizeof(IInterface));
  FillChar(P^, Count * Sizeof(IInterface), #0);
  Result := I;
end;



{                                                                              }
{ PosNext                                                                      }
{   PosNext finds the next occurance of Find in V, -1 if it was not found.     }
{     Searches from Item[PrevPos + 1], ie PrevPos = -1 to find first           }
{     occurance.                                                               }
{                                                                              }
function PosNext(const Find: Byte; const V: ByteArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Byte;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Word; const V: WordArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Word;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: LongWord; const V: LongWordArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongWord;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: ShortInt; const V: ShortIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : ShortInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: SmallInt; const V: SmallIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : SmallInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: LongInt; const V: LongIntArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : LongInt;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Int64; const V: Int64Array; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Int64;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Single; const V: SingleArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Single;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Double; const V: DoubleArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Double;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Extended; const V: ExtendedArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Extended;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: Boolean; const V: BooleanArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : Boolean;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: String; const V: StringArray; const PrevPos: Integer;
    const IsSortedAscending: Boolean): Integer;
var I, L, H : Integer;
    D       : String;
begin
  if IsSortedAscending then // binary search
    begin
      if MaxI(PrevPos + 1, 0) = 0 then // find first
        begin
          L := 0;
          H := Length(V) - 1;
          While L <= H do
            begin
              I := (L + H) div 2;
              D := V[I];
              if Find = D then
                begin
                  While (I > 0) and (V[I - 1] = Find) do
                    Dec(I);
                  Result := I;
                  exit;
                end else
              if D > Find then
                H := I - 1
              else
                L := I + 1;
            end;
          Result := -1;
        end
      else // find next
        if PrevPos >= Length(V) - 1 then
          Result := -1
        else
          if V[PrevPos + 1] = Find then
            Result := PrevPos + 1
          else
            Result := -1;
    end
  else
    begin // linear search
      For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
        if V[I] = Find then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function PosNext(const Find: TObject; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] = Find then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;

function PosNext(const ClassType: TClass; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] is ClassType then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;

function PosNext(const ClassName: String; const V: ObjectArray; const PrevPos: Integer): Integer;
var I : Integer;
    T : TObject;
begin
  For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
    begin
      T := V[I];
      if Assigned(T) and (T.ClassName = ClassName) then
        begin
          Result := I;
          exit;
         end;
    end;
  Result := -1;
end;

function PosNext(const Find: Pointer; const V: PointerArray; const PrevPos: Integer): Integer;
var I : Integer;
begin
  For I := MaxI(PrevPos + 1, 0) to Length(V) - 1 do
    if V[I] = Find then
      begin
        Result := I;
        exit;
       end;
  Result := -1;
end;



{                                                                              }
{ Count                                                                        }
{                                                                              }
function Count(const Find: Byte; const V: ByteArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Word; const V: WordArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: LongWord; const V: LongWordArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: ShortInt; const V: ShortIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: SmallInt; const V: SmallIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: LongInt; const V: LongIntArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Int64; const V: Int64Array; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Single; const V: SingleArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Double; const V: DoubleArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Extended; const V: ExtendedArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: String; const V: StringArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;

function Count(const Find: Boolean; const V: BooleanArray; const IsSortedAscending: Boolean = False): Integer;
var I, J : Integer;
begin
  if IsSortedAscending then
    begin
      I := PosNext(Find, V, -1, True);
      if I = -1 then
        Result := 0 else
        begin
          Result := 1;
          J := Length(V);
          While (I + Result < J) and (V[I + Result] = Find) do
            Inc(Result);
        end;
    end
  else
    begin
      J := -1;
      Result := 0;
      Repeat
        I := PosNext(Find, V, J, False);
        if I >= 0 then
          begin
            Inc(Result);
            J := I;
          end;
      Until I < 0;
    end;
end;



{                                                                              }
{ RemoveAll                                                                    }
{                                                                              }
procedure RemoveAll(const Find: Byte; var V: ByteArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: Word; var V: WordArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: LongWord; var V: LongWordArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: ShortInt; var V: ShortIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: SmallInt; var V: SmallIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: LongInt; var V: LongIntArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: Int64; var V: Int64Array; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: Single; var V: SingleArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: Double; var V: DoubleArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: Extended; var V: ExtendedArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;

procedure RemoveAll(const Find: String; var V: StringArray; const IsSortedAscending: Boolean = False);
var I, J : Integer;
begin
  I := PosNext(Find, V, -1, IsSortedAscending);
  While I >= 0 do
    begin
      J := 1;
      While (I + J < Length(V)) and (V[I + J] = Find) do
        Inc(J);
      Remove(V, I, J);
      I := PosNext(Find, V, I, IsSortedAscending);
    end;
end;



{                                                                              }
{ Intersection                                                                 }
{   If both arrays are sorted ascending then time is o(n) instead of o(n^2).   }
{                                                                              }
function Intersection(const V1, V2: SingleArray; const IsSortedAscending: Boolean): SingleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: DoubleArray; const IsSortedAscending: Boolean): DoubleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: ExtendedArray; const IsSortedAscending: Boolean): ExtendedArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: ByteArray; const IsSortedAscending: Boolean): ByteArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: WordArray; const IsSortedAscending: Boolean): WordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: LongWordArray; const IsSortedAscending: Boolean): LongWordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: ShortIntArray; const IsSortedAscending: Boolean): ShortIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: SmallIntArray; const IsSortedAscending: Boolean): SmallIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: LongIntArray; const IsSortedAscending: Boolean): LongIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: Int64Array; const IsSortedAscending: Boolean): Int64Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Intersection(const V1, V2: StringArray; const IsSortedAscending: Boolean): StringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] = V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) >= 0) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;



{                                                                              }
{ Difference                                                                   }
{   Returns elements in V1 but not in V2.                                      }
{   If both arrays are sorted ascending then time is o(n) instead of o(n^2).   }
{                                                                              }
function Difference(const V1, V2: SingleArray; const IsSortedAscending: Boolean): SingleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: DoubleArray; const IsSortedAscending: Boolean): DoubleArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: ExtendedArray; const IsSortedAscending: Boolean): ExtendedArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: ByteArray; const IsSortedAscending: Boolean): ByteArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: WordArray; const IsSortedAscending: Boolean): WordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: LongWordArray; const IsSortedAscending: Boolean): LongWordArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: ShortIntArray; const IsSortedAscending: Boolean): ShortIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: SmallIntArray; const IsSortedAscending: Boolean): SmallIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: LongIntArray; const IsSortedAscending: Boolean): LongIntArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: Int64Array; const IsSortedAscending: Boolean): Int64Array;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;

function Difference(const V1, V2: StringArray; const IsSortedAscending: Boolean): StringArray;
var I, J, L, LV : Integer;
begin
  SetLength(Result, 0);
  if IsSortedAscending then
    begin
      I := 0;
      J := 0;
      L := Length(V1);
      LV := Length(V2);
      While (I < L) and (J < LV) do
        begin
          While (I < L) and (V1[I] < V2[J]) do
            Inc(I);
          if I < L then
            begin
              if V1[I] <> V2[J] then
                Append(Result, V1[I]);
              While (J < LV) and (V2[J] <= V1[I]) do
                Inc(J);
            end;
        end;
    end
  else
    For I := 0 to Length(V1) - 1 do
      if (PosNext(V1[I], V2) = -1) and (PosNext(V1[I], Result) = -1) then
        Append(Result, V1[I]);
end;



{                                                                              }
{ Reverse                                                                      }
{                                                                              }
procedure Reverse(var V: ByteArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: WordArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: LongWordArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: ShortIntArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: SmallIntArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: LongIntArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: Int64Array);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: StringArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: PointerArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: ObjectArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: SingleArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: DoubleArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;

procedure Reverse(var V: ExtendedArray);
var I, L : Integer;
begin
  L := Length(V);
  For I := 1 to L div 2 do
    Swap(V[I - 1], V[L - I]);
end;



{                                                                              }
{ Returns an open array (V) as a dynamic array.                                }
{                                                                              }
function AsBooleanArray(const V: Array of Boolean): BooleanArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsByteArray(const V: Array of Byte): ByteArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsWordArray(const V: Array of Word): WordArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsLongWordArray(const V: Array of LongWord): LongWordArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsCardinalArray(const V: Array of Cardinal): CardinalArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsShortIntArray(const V: Array of ShortInt): ShortIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsSmallIntArray(const V: Array of SmallInt): SmallIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsLongIntArray(const V: Array of LongInt): LongIntArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsIntegerArray(const V: Array of Integer): IntegerArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsInt64Array(const V: Array of Int64): Int64Array;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsSingleArray(const V: Array of Single): SingleArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsDoubleArray(const V: Array of Double): DoubleArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsExtendedArray(const V: Array of Extended): ExtendedArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsCurrencyArray(const V: Array of Currency): CurrencyArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsStringArray(const V: Array of String): StringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsWideStringArray(const V: Array of WideString): WideStringArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsPointerArray(const V: Array of Pointer): PointerArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsCharSetArray(const V: Array of CharSet): CharSetArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsObjectArray(const V: Array of TObject): ObjectArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;

function AsInterfaceArray(const V: Array of IInterface): InterfaceArray;
var I : Integer;
begin
  SetLength(Result, High(V) + 1);
  For I := 0 to High(V) do
    Result[I] := V[I];
end;



function RangeByte(const First: Byte; const Count: Integer; const Increment: Byte): ByteArray;
var I : Integer;
    J : Byte;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeWord(const First: Word; const Count: Integer; const Increment: Word): WordArray;
var I : Integer;
    J : Word;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeLongWord(const First: LongWord; const Count: Integer; const Increment: LongWord): LongWordArray;
var I : Integer;
    J : LongWord;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeCardinal(const First: Cardinal; const Count: Integer; const Increment: Cardinal): CardinalArray;
var I : Integer;
    J : Cardinal;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeShortInt(const First: ShortInt; const Count: Integer; const Increment: ShortInt): ShortIntArray;
var I : Integer;
    J : ShortInt;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeSmallInt(const First: SmallInt; const Count: Integer; const Increment: SmallInt): SmallIntArray;
var I : Integer;
    J : SmallInt;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeLongInt(const First: LongInt; const Count: Integer; const Increment: LongInt): LongIntArray;
var I : Integer;
    J : LongInt;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeInteger(const First: Integer; const Count: Integer; const Increment: Integer): IntegerArray;
var I : Integer;
    J : Integer;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeInt64(const First: Int64; const Count: Integer; const Increment: Int64): Int64Array;
var I : Integer;
    J : Int64;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeSingle(const First: Single; const Count: Integer; const Increment: Single): SingleArray;
var I : Integer;
    J : Single;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeDouble(const First: Double; const Count: Integer; const Increment: Double): DoubleArray;
var I : Integer;
    J : Double;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;

function RangeExtended(const First: Extended; const Count: Integer; const Increment: Extended): ExtendedArray;
var I : Integer;
    J : Extended;
begin
  SetLength(Result, Count);
  J := First;
  For I := 0 to Count - 1 do
    begin
      Result[I] := J;
      J := J + Increment;
    end;
end;



{                                                                              }
{ Dup                                                                          }
{                                                                              }
function DupByte(const V: Byte; const Count: Integer): ByteArray;
begin
  SetLength(Result, Count);
  FillChar(Result[0], Count, V);
end;

function DupWord(const V: Word; const Count: Integer): WordArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupLongWord(const V: LongWord; const Count: Integer): LongWordArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupCardinal(const V: Cardinal; const Count: Integer): CardinalArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupShortInt(const V: ShortInt; const Count: Integer): ShortIntArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupSmallInt(const V: SmallInt; const Count: Integer): SmallIntArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupLongInt(const V: LongInt; const Count: Integer): LongIntArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupInteger(const V: Integer; const Count: Integer): IntegerArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupInt64(const V: Int64; const Count: Integer): Int64Array;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupSingle(const V: Single; const Count: Integer): SingleArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupDouble(const V: Double; const Count: Integer): DoubleArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupExtended(const V: Extended; const Count: Integer): ExtendedArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupCurrency(const V: Currency; const Count: Integer): CurrencyArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupString(const V: String; const Count: Integer): StringArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupCharSet(const V: CharSet; const Count: Integer): CharSetArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;

function DupObject(const V: TObject; const Count: Integer): ObjectArray;
var I : Integer;
begin
  SetLength(Result, Count);
  For I := 0 to Count - 1 do
    Result[I] := V;
end;



{                                                                              }
{ SetLengthAndZero                                                             }
{                                                                              }
procedure SetLengthAndZero(var V: ByteArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Byte) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: WordArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Word) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: LongWordArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(LongWord) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ShortIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(ShortInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: SmallIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(SmallInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: LongIntArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(LongInt) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: Int64Array; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Int64) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: SingleArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Single) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: DoubleArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Double) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ExtendedArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Extended) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: CurrencyArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Currency) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: CharSetArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(CharSet) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: BooleanArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Boolean) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: PointerArray; const NewLength: Integer);
var OldLen, NewLen : Integer;
begin
  NewLen := NewLength;
  if NewLen < 0 then
    NewLen := 0;
  OldLen := Length(V);
  if OldLen = NewLen then
    exit;
  SetLength(V, NewLen);
  if OldLen > NewLen then
    exit;
  FillChar(Pointer(@V[OldLen])^, Sizeof(Pointer) * (NewLen - OldLen), #0);
end;

procedure SetLengthAndZero(var V: ObjectArray; const NewLength: Integer;
    const FreeObjects: Boolean);
var I, L : Integer;
begin
  L := Length(V);
  if L = NewLength then
    exit;
  if (L > NewLength) and FreeObjects then
    For I := NewLength to L - 1 do
      FreeAndNil(V[I]);
  SetLength(V, NewLength);
  if L > NewLength then
    exit;
  FillChar(V[L], Sizeof(Pointer) * (NewLength - L), #0);
end;



{                                                                              }
{ IsEqual                                                                      }
{                                                                              }
function IsEqual(const V1, V2: ByteArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Byte) * L);
end;

function IsEqual(const V1, V2: WordArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Word) * L);
end;

function IsEqual(const V1, V2: LongWordArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(LongWord) * L);
end;

function IsEqual(const V1, V2: ShortIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(ShortInt) * L);
end;

function IsEqual(const V1, V2: SmallIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(SmallInt) * L);
end;

function IsEqual(const V1, V2: LongIntArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(LongInt) * L);
end;

function IsEqual(const V1, V2: Int64Array): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Int64) * L);
end;

function IsEqual(const V1, V2: SingleArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Single) * L);
end;

function IsEqual(const V1, V2: DoubleArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Double) * L);
end;

function IsEqual(const V1, V2: ExtendedArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Extended) * L);
end;

function IsEqual(const V1, V2: CurrencyArray): Boolean;
var L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  Result := CompareMem(Pointer(V1)^, Pointer(V2)^, Sizeof(Currency) * L);
end;

function IsEqual(const V1, V2: StringArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  For I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;

function IsEqual(const V1, V2: CharSetArray): Boolean;
var I, L : Integer;
begin
  L := Length(V1);
  if L <> Length(V2) then
    begin
      Result := False;
      exit;
    end;
  For I := 0 to L - 1 do
    if V1[I] <> V2[I] then
      begin
        Result := False;
        exit;
      end;
  Result := True;
end;



{                                                                              }
{ Dynamic array to Dynamic array                                               }
{                                                                              }
function ByteArrayToLongIntArray(const V: ByteArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function WordArrayToLongIntArray(const V: WordArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function ShortIntArrayToLongIntArray(const V: ShortIntArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SmallIntArrayToLongIntArray(const V: SmallIntArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToInt64Array(const V: LongIntArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToSingleArray(const V: LongIntArray): SingleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToDoubleArray(const V: LongIntArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function LongIntArrayToExtendedArray(const V: LongIntArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToDoubleArray(const V: SingleArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToExtendedArray(const V: SingleArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToCurrencyArray(const V: SingleArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function SingleArrayToLongIntArray(const V: SingleArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function SingleArrayToInt64Array(const V: SingleArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;

function DoubleArrayToExtendedArray(const V: DoubleArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function DoubleArrayToCurrencyArray(const V: DoubleArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function DoubleArrayToLongIntArray(const V: DoubleArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function DoubleArrayToInt64Array(const V: DoubleArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;

function ExtendedArrayToCurrencyArray(const V: ExtendedArray): CurrencyArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[I];
end;

function ExtendedArrayToLongIntArray(const V: ExtendedArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := LongInt(Trunc(V[I]));
end;

function ExtendedArrayToInt64Array(const V: ExtendedArray): Int64Array;
var I, L : Integer;
begin
  L := Length(V);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := Trunc(V[I]);
end;



{                                                                              }
{ Array from indexes                                                           }
{                                                                              }
function ByteArrayFromIndexes(const V: ByteArray; const Indexes: IntegerArray): ByteArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function WordArrayFromIndexes(const V: WordArray; const Indexes: IntegerArray): WordArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function LongWordArrayFromIndexes(const V: LongWordArray; const Indexes: IntegerArray): LongWordArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function CardinalArrayFromIndexes(const V: CardinalArray; const Indexes: IntegerArray): CardinalArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function ShortIntArrayFromIndexes(const V: ShortIntArray; const Indexes: IntegerArray): ShortIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function SmallIntArrayFromIndexes(const V: SmallIntArray; const Indexes: IntegerArray): SmallIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function LongIntArrayFromIndexes(const V: LongIntArray; const Indexes: IntegerArray): LongIntArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function IntegerArrayFromIndexes(const V: IntegerArray; const Indexes: IntegerArray): IntegerArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function Int64ArrayFromIndexes(const V: Int64Array; const Indexes: IntegerArray): Int64Array;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function SingleArrayFromIndexes(const V: SingleArray; const Indexes: IntegerArray): SingleArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function DoubleArrayFromIndexes(const V: DoubleArray; const Indexes: IntegerArray): DoubleArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function ExtendedArrayFromIndexes(const V: ExtendedArray; const Indexes: IntegerArray): ExtendedArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;

function StringArrayFromIndexes(const V: StringArray; const Indexes: IntegerArray): StringArray;
var I, L : Integer;
begin
  L := Length(Indexes);
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := V[Indexes[I]];
end;



{                                                                              }
{ Dynamic array sort                                                           }
{                                                                              }
procedure Sort(const V: ByteArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Byte;
      P, Q    : PByte;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: WordArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Word;
      P, Q    : PWord;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: LongWordArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : LongWord;
      P, Q    : PLongWord;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: ShortIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : ShortInt;
      P, Q    : PShortInt;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: SmallIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : SmallInt;
      P, Q    : PSmallInt;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: LongIntArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : LongInt;
      P, Q    : PLongInt;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Int64;
      P, Q    : PInt64;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: SingleArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Single;
      P, Q    : PSingle;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: DoubleArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Double;
      P, Q    : PDouble;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const V: StringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
  begin
    Repeat
      I := L;
      P := @V[I];
      J := R;
      Q := @V[J];
      M := (L + R) shr 1;
      W := V[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  I := Length(V);
  if I > 0 then
    QuickSort(0, I - 1);
end;



procedure Sort(const Key: IntegerArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Integer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: IntegerArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Int64;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: IntegerArray; const Data: StringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : String;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: IntegerArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Extended;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: IntegerArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Integer;
      P, Q    : PInteger;
      A       : Pointer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: StringArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
      A       : Integer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: StringArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
      A       : Int64;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: StringArray; const Data: StringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
      A       : String;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: StringArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
      A       : Extended;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: StringArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : String;
      P, Q    : PString;
      A       : Pointer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: ExtendedArray; const Data: IntegerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Integer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: ExtendedArray; const Data: Int64Array);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Int64;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: ExtendedArray; const Data: StringArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : String;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: ExtendedArray; const Data: ExtendedArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Extended;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;

procedure Sort(const Key: ExtendedArray; const Data: PointerArray);

  procedure QuickSort(L, R: Integer);
  var I, J, M : Integer;
      W, T    : Extended;
      P, Q    : PExtended;
      A       : Pointer;
  begin
    Repeat
      I := L;
      P := @Key[I];
      J := R;
      Q := @Key[J];
      M := (L + R) shr 1;
      W := Key[M];
      Repeat
        While P^ < W do
          begin
            Inc(P);
            Inc(I);
          end;
        While Q^ > W do
          begin
            Dec(Q);
            Dec(J);
          end;
        if I <= J then
          begin
            T := P^;
            P^ := Q^;
            Q^ := T;
            A := Data[I];
            Data[I] := Data[J];
            Data[J] := A;
            if M = I then
              begin
                M := J;
                W := Q^;
              end else
              if M = J then
                begin
                  M := I;
                  W := P^;
                end;
            Inc(P);
            Inc(I);
            Dec(Q);
            Dec(J);
          end;
      Until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    Until I >= R;
  end;

var I : Integer;
begin
  Assert(Length(Key) = Length(Data), 'Sort pair must be of equal length');
  I := Length(Key);
  if I > 0 then
    QuickSort(0, I - 1);
end;




{                                                                              }
{ Test cases                                                                   }
{                                                                              }
{$ASSERTIONS ON}
procedure Test_Misc;
var A, B : String;
    L, H : Cardinal;
    I, J : Integer;
    V    : Boolean;
begin
  // Clip
  Assert(SumClipI(1, 2) = 3, 'SumClipI');
  Assert(SumClipI(1, -2) = -1, 'SumClipI');
  Assert(SumClipI(MaxInteger - 1, 0) = MaxInteger - 1, 'SumClipI');
  Assert(SumClipI(MaxInteger - 1, 1) = MaxInteger, 'SumClipI');
  Assert(SumClipI(MaxInteger - 1, 2) = MaxInteger, 'SumClipI');
  Assert(SumClipI(MinInteger + 1, 0) = MinInteger + 1, 'SumClipI');
  Assert(SumClipI(MinInteger + 1, -1) = MinInteger, 'SumClipI');
  Assert(SumClipI(MinInteger + 1, -2) = MinInteger, 'SumClipI');
  Assert(SumClipC(1, 2) = 3, 'SumClipC');
  Assert(SumClipC(3, -2) = 1, 'SumClipC');
  Assert(SumClipC(MaxCardinal - 1, 0) = MaxCardinal - 1, 'SumClipC');
  Assert(SumClipC(MaxCardinal - 1, 1) = MaxCardinal, 'SumClipC');
  Assert(SumClipC(MaxCardinal - 1, 2) = MaxCardinal, 'SumClipC');
  Assert(SumClipC(1, 0) = 1, 'SumClipC');
  Assert(SumClipC(1, -1) = 0, 'SumClipC');
  Assert(SumClipC(1, -2) = 0, 'SumClipC');

  // Ranges
  L := 10;
  H := 20;
  Assert(CardinalRangeIncludeElementRange(L, H, 10, 20), 'RangeInclude');
  Assert((L = 10) and (H = 20), 'RangeInclude');
  Assert(CardinalRangeIncludeElementRange(L, H, 9, 21), 'RangeInclude');
  Assert((L = 9) and (H = 21), 'RangeInclude');
  Assert(CardinalRangeIncludeElementRange(L, H, 7, 10), 'RangeInclude');
  Assert((L = 7) and (H = 21), 'RangeInclude');
  Assert(CardinalRangeIncludeElementRange(L, H, 5, 6), 'RangeInclude');
  Assert((L = 5) and (H = 21), 'RangeInclude');
  Assert(not CardinalRangeIncludeElementRange(L, H, 1, 3), 'RangeInclude');
  Assert((L = 5) and (H = 21), 'RangeInclude');
  Assert(CardinalRangeIncludeElementRange(L, H, 20, 22), 'RangeInclude');
  Assert((L = 5) and (H = 22), 'RangeInclude');
  Assert(CardinalRangeIncludeElementRange(L, H, 23, 24), 'RangeInclude');
  Assert((L = 5) and (H = 24), 'RangeInclude');
  Assert(not CardinalRangeIncludeElementRange(L, H, 26, 27), 'RangeInclude');
  Assert((L = 5) and (H = 24), 'RangeInclude');

  // iif
  Assert(iif(True, 1, 2) = 1, 'iif');
  Assert(iif(False, 1, 2) = 2, 'iif');
  Assert(iif(True, -1, -2) = -1, 'iif');
  Assert(iif(False, -1, -2) = -2, 'iif');
  Assert(iif(True, '1', '2') = '1', 'iif');
  Assert(iif(False, '1', '2') = '2', 'iif');
  Assert(iif(True, 1.1, 2.2) = 1.1, 'iif');
  Assert(iif(False, 1.1, 2.2) = 2.2, 'iif');

  // CharSet
  Assert(CharCount([]) = 0, 'CharCount');
  Assert(CharCount(['a'..'z']) = 26, 'CharCount');
  Assert(CharCount([#0, #255]) = 2, 'CharCount');

  // Compare
  Assert(Compare(1, 1) = crEqual, 'Compare');
  Assert(Compare(1, 2) = crLess, 'Compare');
  Assert(Compare(1, 0) = crGreater, 'Compare');
  Assert(Compare(1.0, 1.0) = crEqual, 'Compare');
  Assert(Compare(1.0, 1.1) = crLess, 'Compare');
  Assert(Compare(1.0, 0.9) = crGreater, 'Compare');
  Assert(Compare(False, False) = crEqual, 'Compare');
  Assert(Compare(True, True) = crEqual, 'Compare');
  Assert(Compare(False, True) = crLess, 'Compare');
  Assert(Compare(True, False) = crGreater, 'Compare');
  Assert(Compare('', '') = crEqual, 'Compare');
  Assert(Compare('a', 'a') = crEqual, 'Compare');
  Assert(Compare('a', 'b') = crLess, 'Compare');
  Assert(Compare('b', 'a') = crGreater, 'Compare');
  Assert(Compare('', 'a') = crLess, 'Compare');
  Assert(Compare('a', '') = crGreater, 'Compare');
  Assert(Compare('aa', 'a') = crGreater, 'Compare');

  Assert(not FloatZero(1e-1, 1e-2), 'FloatZero');
  Assert(FloatZero(1e-2, 1e-2), 'FloatZero');
  Assert(not FloatZero(1e-1, 1e-9), 'FloatZero');
  Assert(not FloatZero(1e-8, 1e-9), 'FloatZero');
  Assert(FloatZero(1e-9, 1e-9), 'FloatZero');
  Assert(FloatZero(1e-10, 1e-9), 'FloatZero');
  Assert(not FloatZero(0.2, 1e-1), 'FloatZero');
  Assert(FloatZero(0.09, 1e-1), 'FloatZero');

  Assert(FloatOne(1.0, 1e-1), 'FloatOne');
  Assert(FloatOne(1.09999, 1e-1), 'FloatOne');
  Assert(FloatOne(0.90001, 1e-1), 'FloatOne');
  Assert(not FloatOne(1.10001, 1e-1), 'FloatOne');
  Assert(not FloatOne(1.2, 1e-1), 'FloatOne');
  Assert(not FloatOne(0.89999, 1e-1), 'FloatOne');

  Assert(not FloatsEqual(2.0, -2.0, 1e-1), 'FloatsEqual');
  Assert(not FloatsEqual(1.0, 0.0, 1e-1), 'FloatsEqual');
  Assert(FloatsEqual(2.0, 2.0, 1e-1), 'FloatsEqual');
  Assert(FloatsEqual(2.0, 2.09, 1e-1), 'FloatsEqual');
  Assert(FloatsEqual(2.0, 1.90000001, 1e-1), 'FloatsEqual');
  Assert(not FloatsEqual(2.0, 2.10001, 1e-1), 'FloatsEqual');
  Assert(not FloatsEqual(2.0, 2.2, 1e-1), 'FloatsEqual');
  Assert(not FloatsEqual(2.0, 1.8999999, 1e-1), 'FloatsEqual');
  Assert(FloatsEqual(2.00000000011, 2.0, 1e-2), 'FloatsEqual');
  Assert(FloatsEqual(2.00000000011, 2.0, 1e-9), 'FloatsEqual');
  Assert(not FloatsEqual(2.00000000011, 2.0, 1e-10), 'FloatsEqual');
  Assert(not FloatsEqual(2.00000000011, 2.0, 1e-11), 'FloatsEqual');

  Assert(FloatsCompare(0.0, 0.0, MinExtended) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.2, 1.2, MinExtended) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.23456789e-300, 1.23456789e-300, MinExtended) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.23456780e-300, 1.23456789e-300, MinExtended) = crLess, 'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-4) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-5) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-6) = crLess, 'FloatsCompare');
  Assert(FloatsCompare(1.4e-5, 1.5e-5, 1e-7) = crLess, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-1) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-2) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-3) = crEqual, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-4) = crGreater, 'FloatsCompare');
  Assert(FloatsCompare(0.5003, 0.5001, 1e-5) = crGreater, 'FloatsCompare');

  Assert(ApproxEqual(0.0, 0.0), 'ApproxEqual');
  Assert(not ApproxEqual(0.0, 1e-100, 1e-10), 'ApproxEqual');
  Assert(not ApproxEqual(1.0, 1e-100, 1e-10), 'ApproxEqual');
  Assert(ApproxEqual(1.0, 1.0), 'ApproxEqual');
  Assert(ApproxEqual(-1.0, -1.0), 'ApproxEqual');
  Assert(not ApproxEqual(1.0, -1.0), 'ApproxEqual');
  Assert(ApproxEqual(1e-100, 1e-100, 1e-10), 'ApproxEqual');
  Assert(not ApproxEqual(0.0, 1.0, 1e-9), 'ApproxEqual');
  Assert(not ApproxEqual(-1.0, 1.0, 1e-9), 'ApproxEqual');
  Assert(ApproxEqual(0.12345, 0.12349, 1e-3), 'ApproxEqual');
  Assert(not ApproxEqual(0.12345, 0.12349, 1e-4), 'ApproxEqual');
  Assert(not ApproxEqual(0.12345, 0.12349, 1e-5), 'ApproxEqual');
  Assert(ApproxEqual(1.2345e+100, 1.2349e+100, 1e-3), 'ApproxEqual');
  Assert(not ApproxEqual(1.2345e+100, 1.2349e+100, 1e-4), 'ApproxEqual');
  Assert(not ApproxEqual(1.2345e+100, 1.2349e+100, 1e-5), 'ApproxEqual');
  Assert(ApproxEqual(1.2345e-100, 1.2349e-100, 1e-3), 'ApproxEqual');
  Assert(not ApproxEqual(1.2345e-100, 1.2349e-100, 1e-4), 'ApproxEqual');
  Assert(not ApproxEqual(1.2345e-100, 1.2349e-100, 1e-5), 'ApproxEqual');
  Assert(not ApproxEqual(1.0e+20, 1.00000001E+20, 1e-8), 'ApproxEqual');
  Assert(ApproxEqual(1.0e+20, 1.000000001E+20, 1e-8), 'ApproxEqual');
  Assert(not ApproxEqual(1.0e+20, 1.000000001E+20, 1e-9), 'ApproxEqual');
  Assert(ApproxEqual(1.0e+20, 1.0000000001E+20, 1e-9), 'ApproxEqual');
  Assert(not ApproxEqual(1.0e+20, 1.0000000001E+20, 1e-10), 'ApproxEqual');

  Assert(ApproxCompare(0.0, 0.0) = crEqual, 'ApproxCompare');
  Assert(ApproxCompare(0.0, 1.0) = crLess, 'ApproxCompare');
  Assert(ApproxCompare(1.0, 0.0) = crGreater, 'ApproxCompare');
  Assert(ApproxCompare(-1.0, 1.0) = crLess, 'ApproxCompare');
  Assert(ApproxCompare(1.2345e+10, 1.2349e+10, 1e-3) = crEqual, 'ApproxCompare');
  Assert(ApproxCompare(1.2345e+10, 1.2349e+10, 1e-4) = crLess, 'ApproxCompare');
  Assert(ApproxCompare(-1.2345e-10, -1.2349e-10, 1e-3) = crEqual, 'ApproxCompare');
  Assert(ApproxCompare(-1.2345e-10, -1.2349e-10, 1e-4) = crGreater, 'ApproxCompare');

  Assert(ReverseCompareResult(crLess) = crGreater, 'ReverseCompareResult');
  Assert(ReverseCompareResult(crGreater) = crLess, 'ReverseCompareResult');

  // MoveMem
  For I := -8 to 16 do
    begin
      A := '0123456789ABCDEFGHIJ';
      B := '                    ';
      MoveMem(A[1], B[1], I);
      For J := 1 to MinI(I, 10) do
        Assert(B[J] = Char(48 + J - 1), 'MoveMem');
      For J := 11 to MinI(I, 16) do
        Assert(B[J] = Char(65 + J - 11), 'MoveMem');
      For J := MaxI(I + 1, 1) to 20 do
        Assert(B[J] = ' ');
    end;

  // ZeroMem
  For I := -8 to 16 do
    begin
      A := '0123456789ABCDEFGHIJ';
      ZeroMem(A[1], I);
      For J := 1 to I do
        Assert(A[J] = #0, 'ZeroMem');
      For J := MaxI(I + 1, 1) to 10 do
        Assert(A[J] = Char(48 + J - 1), 'ZeroMem');
      For J := MaxI(I + 1, 11) to 20 do
        Assert(A[J] = Char(65 + J - 11), 'ZeroMem');
    end;

  // FillMem
  For I := -8 to 16 do
    begin
      A := '0123456789ABCDEFGHIJ';
      FillMem(A[1], I, Ord('Z'));
      For J := 1 to I do
        Assert(A[J] = 'Z', 'FillMem');
      For J := MaxI(I + 1, 1) to 10 do
        Assert(A[J] = Char(48 + J - 1), 'FillMem');
      For J := MaxI(I + 1, 11) to 20 do
        Assert(A[J] = Char(65 + J - 11), 'FillMem');
    end;

  // Hash
  Assert(HashStr('Fundamentals') = $3FB7796E, 'HashStr');

  // Encodings
  Assert(HexCharValue('A') = 10, 'HexCharValue');
  Assert(HexCharValue('a') = 10, 'HexCharValue');
  Assert(HexCharValue('1') = 1, 'HexCharValue');
  Assert(HexCharValue('G') = $FF, 'HexCharValue');

  Assert(LongWordToStr(123) = '123', 'LongWordToStr');
  Assert(LongWordToStr(0) = '0', 'LongWordToStr');
  Assert(LongWordToStr($FFFFFFFF) = '4294967295', 'LongWordToStr');
  Assert(LongWordToStr(10000) = '10000', 'LongWordToStr');
  Assert(LongWordToStr(99999) = '99999', 'LongWordToStr');
  Assert(LongWordToStr(1, 1) = '1', 'LongWordToStr');
  Assert(LongWordToStr(1, 3) = '001', 'LongWordToStr');
  Assert(LongWordToStr(1234, 3) = '1234', 'LongWordToStr');

  Assert(DecStrToLongWord('', V) = 0, 'DecStrToLongWord');
  Assert(V = False, 'DecStrToLongWord');
  Assert(DecStrToLongWord('123', V) = 123, 'DecStrToLongWord');
  Assert(V = True, 'DecStrToLongWord');
  Assert(DecStrToLongWord('4294967295', V) = $FFFFFFFF, 'DecStrToLongWord');
  Assert(V = True, 'DecStrToLongWord');
  Assert(DecStrToLongWord('99999', V) = 99999, 'DecStrToLongWord');

  Assert(LongWordToHex(0) = '0', 'LongWordToHex');
  Assert(LongWordToHex($FFFFFFFF) = 'FFFFFFFF', 'LongWordToHex');
  Assert(LongWordToHex($10000) = '10000', 'LongWordToHex');
  Assert(LongWordToHex($12345678) = '12345678', 'LongWordToHex');
  Assert(LongWordToHex($AB, 4) = '00AB', 'LongWordToHex');
  Assert(LongWordToHex($ABCD, 8) = '0000ABCD', 'LongWordToHex');
  Assert(LongWordToHex(0, 8) = '00000000', 'LongWordToHex');
  Assert(LongWordToHex($CDEF, 2) = 'CDEF', 'LongWordToHex');

  Assert(HexStrToLongWord('FFFFFFFF', V) = $FFFFFFFF, 'HexStrToLongWord');
  Assert(V = True, 'HexStrToLongWord');
  Assert(HexStrToLongWord('0', V) = 0, 'HexStrToLongWord');
  Assert(V = True, 'HexStrToLongWord');
  Assert(HexStrToLongWord('123456', V) = $123456, 'HexStrToLongWord');
  Assert(HexStrToLongWord('ABC', V) = $ABC, 'HexStrToLongWord');
  Assert(HexStrToLongWord('', V) = 0, 'HexStrToLongWord');
  Assert(V = False, 'HexStrToLongWord');
  Assert(HexStrToLongWord('x', V) = 0, 'HexStrToLongWord');
  Assert(V = False, 'HexStrToLongWord');
  Assert(HexStrToLongWord('1000', V) = $1000, 'HexStrToLongWord');
end;

procedure Test_BitFunctions;
begin
  Assert(SetBit($100F, 5) = $102F, 'SetBit');
  Assert(ClearBit($102F, 5) = $100F, 'ClearBit');
  Assert(ToggleBit($102F, 5) = $100F, 'ToggleBit');
  Assert(ToggleBit($100F, 5) = $102F, 'ToggleBit');
  Assert(IsBitSet($102F, 5), 'IsBitSet');
  Assert(not IsBitSet($100F, 5), 'IsBitSet');

  Assert(SetBitScanForward(0) = -1, 'SetBitScanForward');
  Assert(SetBitScanForward($1020) = 5, 'SetBitScanForward');
  Assert(SetBitScanReverse($1020) = 12, 'SetBitScanForward');
  Assert(SetBitScanForward($1020, 6) = 12, 'SetBitScanForward');
  Assert(SetBitScanReverse($1020, 11) = 5, 'SetBitScanForward');
  Assert(ClearBitScanForward($FFFFFFFF) = -1, 'ClearBitScanForward');
  Assert(ClearBitScanForward($1020) = 0, 'ClearBitScanForward');
  Assert(ClearBitScanReverse($1020) = 31, 'ClearBitScanForward');
  Assert(ClearBitScanForward($1020, 5) = 6, 'ClearBitScanForward');
  Assert(ClearBitScanReverse($1020, 12) = 11, 'ClearBitScanForward');

  Assert(ReverseBits($12345678) = $1E6A2C48, 'ReverseBits');
  Assert(SwapEndian($12345678) = $78563412, 'SwapEndian');

  Assert(BitCount($12341234) = 10, 'BitCount');

  Assert(LowBitMask(10) = $3FF, 'LowBitMask');
  Assert(HighBitMask(28) = $F0000000, 'HighBitMask');
  Assert(RangeBitMask(2, 6) = $7C, 'RangeBitMask');

  Assert(SetBitRange($101, 2, 6) = $17D, 'SetBitRange');
  Assert(ClearBitRange($17D, 2, 6) = $101, 'ClearBitRange');
  Assert(ToggleBitRange($17D, 2, 6) = $101, 'ToggleBitRange');
  Assert(IsBitRangeSet($17D, 2, 6), 'IsBitRangeSet');
  Assert(not IsBitRangeSet($101, 2, 6), 'IsBitRangeSet');
  Assert(not IsBitRangeClear($17D, 2, 6), 'IsBitRangeClear');
  Assert(IsBitRangeClear($101, 2, 6), 'IsBitRangeClear');
end;

procedure Test_IntegerArray;
var S, T : IntegerArray;
    F    : Integer;
begin
  S := nil;
  For F := 1 to 100 do
    begin
      Append(S, F);
      Assert(Length(S) = F,                 'Append');
      Assert(S[F - 1] = F,                  'Append');
    end;

  T := Copy(S);
  AppendIntegerArray(S, T);
  For F := 1 to 100 do
    Assert(S[F + 99] = F,                   'Append');
  Assert(PosNext(60, S) = 59,               'PosNext');
  Assert(PosNext(60, T) = 59,               'PosNext');
  Assert(PosNext(60, S, 59) = 159,          'PosNext');
  Assert(PosNext(60, T, 59) = -1,           'PosNext');
  Assert(PosNext(60, T, -1, True) = 59,     'PosNext');
  Assert(PosNext(60, T, 59, True) = -1,     'PosNext');

  For F := 1 to 100 do
    begin
      Remove(S, PosNext(F, S), 1);
      Assert(Length(S) = 200 - F,           'Remove');
    end;
  For F := 99 downto 0 do
    begin
      Remove(S, PosNext(F xor 3 + 1, S), 1);
      Assert(Length(S) = F,                 'Remove');
    end;

  S := AsIntegerArray([3, 1, 2, 5, 4]);
  Sort(S);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 2, 'Sort');
  Assert(S[2] = 3, 'Sort');
  Assert(S[3] = 4, 'Sort');
  Assert(S[4] = 5, 'Sort');

  S := AsIntegerArray([3, 5, 5, 2, 5, 5, 1]);
  Sort(S);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 2, 'Sort');
  Assert(S[2] = 3, 'Sort');
  Assert(S[3] = 5, 'Sort');
  Assert(S[4] = 5, 'Sort');
  Assert(S[5] = 5, 'Sort');
  Assert(S[6] = 5, 'Sort');

  SetLength(S, 1000);
  For F := 0 to 999 do
    S[F] := F mod 5;
  Sort(S);
  For F := 0 to 999 do
    Assert(S[F] = F div 200, 'Sort');

  S := AsIntegerArray([6, 3, 5, 1]);
  T := AsIntegerArray([1, 2, 3, 4]);
  Sort(S, T);
  Assert(S[0] = 1, 'Sort');
  Assert(S[1] = 3, 'Sort');
  Assert(S[2] = 5, 'Sort');
  Assert(S[3] = 6, 'Sort');
  Assert(T[0] = 4, 'Sort');
  Assert(T[1] = 2, 'Sort');
  Assert(T[2] = 3, 'Sort');
  Assert(T[3] = 1, 'Sort');
end;

procedure SelfTest;
begin
  Test_Misc;
  Test_BitFunctions;
  Test_IntegerArray;
end;



end.

