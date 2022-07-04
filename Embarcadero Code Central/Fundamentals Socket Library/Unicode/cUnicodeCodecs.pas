{$INCLUDE ..\cDefines.inc}
unit cUnicodeCodecs;

{                                                                              }
{                           Unicode codecs v3.06                               }
{                                                                              }
{      This unit is copyright © 2000-2003 by David Butler (david@e.co.za)      }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                Its original file name is cUnicodeCodecs.pas                  }
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
{ Description:                                                                 }
{   Codecs (encoders/decoders) for Unicode text.                               }
{                                                                              }
{ Revision history:                                                            }
{   17/04/2002  0.01  Initial version.                                         }
{                     ISO8859-1 to ISO8859-15, Mac, Win1250-Win1252, UTF-8,    }
{                     UTF-16.                                                  }
{   20/04/2002  0.02  Added EBCDIC-US.                                         }
{                     424 lines interface. 2359 lines implementation.          }
{   28/10/2002  3.03  Refactored for Fundamentals 3.                           }
{   29/10/2002  3.04  Added UTF-8 string functions.                            }
{   04/11/2002  3.05  Added SelfTest procedure.                                }
{                     Fixed bug in UTF-8 encoding function.                    }
{   23/05/2003  3.06  Detection routines.                                      }
{                                                                              }

interface

const
  UnitName      = 'cUnicodeCodecs';
  UnitVersion   = '3.06';
  UnitDesc      = 'Unicode codecs';



{                                                                              }
{ WideChar character conversion functions                                      }
{                                                                              }
{$IFDEF DELPHI5}
type
  UCS4Char = LongWord;
{$ENDIF}

function  ASCIIToWideChar(const P: Char): WideChar;
function  ISO8859_1ToWideChar(const P: Char): WideChar;
function  ISO8859_2ToWideChar(const P: Char): WideChar;
function  ISO8859_3ToWideChar(const P: Char): WideChar;
function  ISO8859_4ToWideChar(const P: Char): WideChar;
function  ISO8859_5ToWideChar(const P: Char): WideChar;
function  ISO8859_6ToWideChar(const P: Char): WideChar;
function  ISO8859_7ToWideChar(const P: Char): WideChar;
function  ISO8859_8ToWideChar(const P: Char): WideChar;
function  ISO8859_9ToWideChar(const P: Char): WideChar;
function  ISO8859_10ToWideChar(const P: Char): WideChar;
function  ISO8859_13ToWideChar(const P: Char): WideChar;
function  ISO8859_14ToWideChar(const P: Char): WideChar;
function  ISO8859_15ToWideChar(const P: Char): WideChar;
function  KOI8_RToWideChar(const P: Char): WideChar;
function  MacLatin2ToWideChar(const P: Char): WideChar;
function  MacRomanToWideChar(const P: Char): WideChar;
function  MacCyrillicToWideChar(const P: Char): WideChar;
function  CP437ToWideChar(const P: Char): WideChar;
function  Win1250ToWideChar(const P: Char): WideChar;
function  Win1251ToWideChar(const P: Char): WideChar;
function  Win1252ToWideChar(const P: Char): WideChar;
function  EBCDIC_USToWideChar(const P: Char): WideChar;

type
  TUTF8Error = (
      UTF8ErrorNone,
      UTF8ErrorInvalidEncoding,
      UTF8ErrorIncompleteEncoding,
      UTF8ErrorInvalidBuffer,
      UTF8ErrorOutOfRange);

function  UTF8ToUCS4Char(const P: PChar; const Size: Integer;
          var SeqSize: Integer; var Ch: UCS4Char): TUTF8Error;
function  UTF8ToWideChar(const P: PChar; const Size: Integer;
          var SeqSize: Integer; var Ch: WideChar): TUTF8Error;

function  WideCharToASCII(const Ch: WideChar): Char;
function  WideCharToISO8859_1(const Ch: WideChar): Char;
function  WideCharToISO8859_2(const Ch: WideChar): Char;
function  WideCharToISO8859_3(const Ch: WideChar): Char;
function  WideCharToISO8859_4(const Ch: WideChar): Char;
function  WideCharToISO8859_5(const Ch: WideChar): Char;
function  WideCharToISO8859_6(const Ch: WideChar): Char;
function  WideCharToISO8859_7(const Ch: WideChar): Char;
function  WideCharToISO8859_8(const Ch: WideChar): Char;
function  WideCharToISO8859_9(const Ch: WideChar): Char;
function  WideCharToISO8859_10(const Ch: WideChar): Char;
function  WideCharToISO8859_13(const Ch: WideChar): Char;
function  WideCharToISO8859_14(const Ch: WideChar): Char;
function  WideCharToISO8859_15(const Ch: WideChar): Char;
function  WideCharToKOI8_R(const Ch: WideChar): Char;
function  WideCharToMacLatin2(const Ch: WideChar): Char;
function  WideCharToMacRoman(const Ch: WideChar): Char;
function  WideCharToMacCyrillic(const Ch: WideChar): Char;
function  WideCharToCP437(const Ch: WideChar): Char;
function  WideCharToWin1250(const Ch: WideChar): Char;
function  WideCharToWin1251(const Ch: WideChar): Char;
function  WideCharToWin1252(const Ch: WideChar): Char;
function  WideCharToEBCDIC_US(const Ch: WideChar): Char;

procedure UCS4CharToUTF8(const Ch: UCS4Char; const Dest: Pointer;
          const DestSize: Integer; var SeqSize: Integer);
procedure WideCharToUTF8(const Ch: WideChar; const Dest: Pointer;
          const DestSize: Integer; var SeqSize: Integer);



{                                                                              }
{ ASCII String functions                                                       }
{                                                                              }
function  IsASCIIString(const S: String): Boolean;
function  IsASCIIWideBuf(const Buf: PWideChar; const Len: Integer): Boolean;
function  IsASCIIWideString(const S: WideString): Boolean;



{                                                                              }
{ Long string functions                                                        }
{                                                                              }
procedure LongToWide(const Buf: Pointer; const BufSize: Integer;
          const DestBuf: Pointer);
function  LongStringToWideString(const S: String): WideString;
procedure WideToLong(const Buf: Pointer; const Len: Integer;
          const DestBuf: Pointer);
function  WideToLongString(const P: PWideChar; const Len: Integer): String;
function  WideStringToLongString(const S: WideString): String;



{                                                                              }
{ UTF-8 string functions                                                       }
{                                                                              }
function  UTF8CharSize(const P: PChar; const Size: Integer): Integer;
function  UTF8BufLength(const P: PChar; const Size: Integer): Integer;
function  UTF8StringLength(const S: String): Integer;
function  UTF8StringToWideString(const S: String): WideString;
function  UTF8StringToLongString(const S: String): String;

function  UCS4CharToUTF8CharSize(const Ch: UCS4Char): Integer;
function  WideBufToUTF8Size(const Buf: PWideChar; const Len: Integer): Integer;
function  WideStringToUTF8Size(const S: WideString): Integer;
function  WideBufToUTF8String(const Buf: PWideChar; const Len: Integer): String;
function  WideStringToUTF8String(const S: WideString): String;
function  LongBufToUTF8Size(const Buf: PChar; const Len: Integer): Integer;
function  LongStringToUTF8Size(const S: String): Integer;
function  LongStringToUTF8String(const S: String): String;
function  UCS4CharToUTF8String(const Ch: UCS4Char): String;
function  ISO8859_1StringToUTF8String(const S: String): String;

function  DetectUTF8Encoding(const P: PChar; const Size: Integer;
          var HeaderSize: Integer): Boolean;



{                                                                              }
{ UTF-16 functions                                                             }
{                                                                              }
function  DetectUTF16Encoding(const P: PChar; const Size: Integer;
          var SwapEndian: Boolean; var HeaderSize: Integer): Boolean;
function  SwapUTF16Endian(const P: WideChar): WideChar;



{                                                                              }
{ WideString functions                                                         }
{                                                                              }
function  CharToWideChar(const P: Char): WideChar;
function  WideCharsToWideString(const Chars: Array of WideChar): WideString;
procedure ISO8859ToWide(const Buf: Pointer; const BufSize: Integer;
          const DestBuf: Pointer);
function  ISO8859_1StringToWideString(const S: String): WideString;

function  IsWideSpace(const Ch: WideChar): Boolean;
function  IsWideControl(const Ch: WideChar): Boolean;
function  IsWideWhiteSpace(const Ch: WideChar): Boolean;



{                                                                              }
{ Unicode codec classes                                                        }
{   AUnicodeCodec is the base class for Unicode Codec implementations.         }
{                                                                              }
type
  TUnicodeCodecType = (
      ucCustom,
      ucASCII,
      ucISO8859_1, ucISO8859_2, ucISO8859_3, ucISO8859_4, ucISO8859_5,
      ucISO8859_6, ucISO8859_7, ucISO8859_8, ucISO8859_9, ucISO8859_10,
      ucISO8859_13, ucISO8859_14, ucISO8859_15,
      ucKOI8_R,
      ucMacLatin2, ucMacRoman, ucMacCyrillic,
      ucCP437,
      ucWin1250, ucWin1251, ucWin1252,
      ucEBCDIC_US,
      ucUTF8, ucUTF16, ucUTF16RE);
  TCodecErrorAction = (eaException, eaStop, eaIgnore, eaSkip, eaReplace);
  AUnicodeCodec = class
  protected
    FErrorAction       : TCodecErrorAction;
    FDecodeReplaceChar : WideChar;

    procedure Init; virtual;

  public
    class function GetUnicodeCodecType: TUnicodeCodecType; virtual; abstract;
    class function GetAliasCount: Integer; virtual; abstract;
    class function GetAliasByIndex(const Idx: Integer): String; virtual; abstract;

    constructor Create; virtual;

    property  ErrorAction: TCodecErrorAction read FErrorAction write FErrorAction;
    property  DecodeReplaceChar: WideChar read FDecodeReplaceChar write FDecodeReplaceChar;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              var ProcessedBytes, DestLength: Integer); virtual; abstract;
    function  Encode(const S: PWideChar; const Length: Integer;
              var ProcessedChars: Integer): String; virtual; abstract;

    procedure DecodeStr(const Buf: Pointer; const BufSize: Integer;
              var Dest: WideString; var ProcessedBytes: Integer);
    function  EncodeStr(const S: WideString;
              var ProcessedChars: Integer): String;
  end;
  TUnicodeCodecClass = class of AUnicodeCodec;



{                                                                              }
{ Unicode codec classes                                                        }
{                                                                              }
type
  AByteCodec = class(AUnicodeCodec)
  public
    function  DecodeChar(const P: Char): WideChar; virtual; abstract;
    function  EncodeChar(const Ch: WideChar): Char; virtual; abstract;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              var ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              var ProcessedChars: Integer): String; override;
  end;

  TASCIICodec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_1Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              var ProcessedBytes, DestLength: Integer); override;
  end;

  TISO8859_2Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_3Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_4Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_5Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_6Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_7Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_8Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_9Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_10Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_13Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_14Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TISO8859_15Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TKOI8_RCodec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TMacLatin2Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TMacRomanCodec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TMacCyrillicCodec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TCP437Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TWin1250Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TWin1251Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TWin1252Codec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TEBCDIC_USCodec = class(AByteCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    function  DecodeChar(const P: Char): WideChar; override;
    function  EncodeChar(const Ch: WideChar): Char; override;
  end;

  TUTF8Codec = class(AUnicodeCodec)
  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              var ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              var ProcessedChars: Integer): String; override;
  end;

  TUTF16Codec = class(AUnicodeCodec) // UTF-16 System Endian
  protected
    FSwapEndian : Boolean;

    procedure Init; override;

  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;

    procedure Decode(const Buf: Pointer; const BufSize: Integer;
              const DestBuf: Pointer; const DestSize: Integer;
              var ProcessedBytes, DestLength: Integer); override;
    function  Encode(const S: PWideChar; const Length: Integer;
              var ProcessedChars: Integer): String; override;
  end;

  TUTF16RECodec = class(TUTF16Codec) // UTF-16 Reverse Endian
  protected
    procedure Init; override;

  public
    class function GetUnicodeCodecType: TUnicodeCodecType; override;
    class function GetAliasCount: Integer; override;
    class function GetAliasByIndex(const Idx: Integer): String; override;
  end;

function  GetUnicodeCodecClassByType(const CodecType: TUnicodeCodecType): TUnicodeCodecClass;
function  GetUnicodeCodecTypeByName(const Name: String): TUnicodeCodecType;
function  GetUnicodeCodecClassByName(const Name: String): TUnicodeCodecClass;



{                                                                              }
{ Unicode conversion functions                                                 }
{                                                                              }
function  DetectUnicodeEncoding(const Buf: Pointer;
          const BufSize: Integer; var HeaderSize: Integer;
          var Codec: TUnicodeCodecType): Boolean;

function  DecodeUnicodeEncoding(const CodecClass: TUnicodeCodecClass;
          const Buf: Pointer; const BufSize: Integer;
          var ProcessedBytes: Integer): WideString; overload;
function  DecodeUnicodeEncoding(const Codec: TUnicodeCodecType;
          const Buf: Pointer; const BufSize: Integer;
          var ProcessedBytes: Integer): WideString; overload;

function  EncodeUnicodeEncoding(const CodecClass: TUnicodeCodecClass;
          const S: WideString; var ProcessedChars: Integer): String; overload;
function  EncodeUnicodeEncoding(const Codec: TUnicodeCodecType;
          const S: WideString; var ProcessedChars: Integer): String; overload;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUtils;



{                                                                              }
{ WideString functions                                                         }
{                                                                              }
function CharToWideChar(const P: Char): WideChar;
begin
  Result := WideChar(Ord(P));
end;

function WideCharsToWideString(const Chars: Array of WideChar): WideString;
var L: Integer;
begin
  L := Length(Chars);
  SetLength(Result, L);
  if L = 0 then
    exit;
  Move(Chars, Pointer(Result)^, Sizeof(WideChar) * L);
end;

function IsWideSpace(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$0009..#$000D,    // ASCII CONTROL
    #$0020,            // SPACE
    #$0085,            // <control>
    #$00A0,            // NO-BREAK SPACE
    #$1680,            // OGHAM SPACE MARK
    #$2000..#$200A,    // EN QUAD..HAIR SPACE
    #$2028,            // LINE SEPARATOR
    #$2029,            // PARAGRAPH SEPARATOR
    #$202F,            // NARROW NO-BREAK SPACE
    #$3000 :           // IDEOGRAPHIC SPACE
      Result := True;
  else
    Result := False;
  end;
end;

function IsWideControl(const Ch: WideChar): Boolean;
begin
  Case Ch of
    #$0000..#$001F,
    #$007F..#$009F :
      Result := True;
  else
    Result := False;
  end;
end;

function IsWideWhiteSpace(const Ch: WideChar): Boolean;
begin
  Result := IsWideControl(Ch) or IsWideSpace(Ch);
end;



{                                                                              }
{ AUnicodeCodec                                                                }
{                                                                              }
constructor AUnicodeCodec.Create;
begin
  inherited Create;
  Init;
end;

procedure AUnicodeCodec.Init;
begin
  FErrorAction := eaException;
  FDecodeReplaceChar := WideChar(#$FFFD);
end;

procedure AUnicodeCodec.DecodeStr(const Buf: Pointer; const BufSize: Integer;
    var Dest: WideString; var ProcessedBytes: Integer);
var P: PChar;
    Q: PWideChar;
    L, M, I, J: Integer;
begin
  P := Buf;
  L := BufSize;
  if not Assigned(P) or (L <= 0) then
    begin
      Dest := '';
      ProcessedBytes := 0;
      exit;
    end;
  SetLength(Dest, BufSize);
  M := 0;
  Repeat
    Q := Pointer(Dest);
    Inc(Q, M);
    Decode(P, L, Q, BufSize * Sizeof(WideChar), I, J);
    Dec(L, I);
    Inc(P, I);
    Inc(M, J);
    if (J < BufSize) or (L <= 0) then
      break;
    SetLength(Dest, M + BufSize);
  Until False;
  if Length(Dest) <> M then
    SetLength(Dest, M);
  ProcessedBytes := BufSize - L;
end;

function AUnicodeCodec.EncodeStr(const S: WideString; var ProcessedChars: Integer): String;
begin
  Result := Encode(Pointer(S), Length(S), ProcessedChars);
end;



{                                                                              }
{ AByteCodec                                                                   }
{                                                                              }
procedure AByteCodec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    var ProcessedBytes, DestLength: Integer);
var P: PChar;
    Q: PWideChar;
    I, L, C: Integer;
begin
  P := Buf;
  Q := DestBuf;
  C := DestSize div Sizeof(WideChar);
  if not Assigned(P) or (BufSize <= 0) or not Assigned(Q) or (C <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  L := 0;
  For I := 1 to BufSize do
    try
      if L >= C then
        break;
      Q^ := DecodeChar(P^);
      Inc(P);
      Inc(Q);
      Inc(L);
    except
      Case FErrorAction of
        eaException : raise;
        eaStop      : break;
        eaSkip:
          Inc(P);
        eaIgnore:
          begin
            Q^ := WideChar(P^);
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
        eaReplace:
          begin
            Q^ := FDecodeReplaceChar;
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
      end;
    end;
  DestLength := L;
  ProcessedBytes := P - Buf;
end;

function AByteCodec.Encode(const S: PWideChar; const Length: Integer;
    var ProcessedChars: Integer): String;
var P: PChar;
    Q: PWideChar;
    I, L: Integer;
begin
  Q := S;
  if not Assigned(Q) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  L := 0;
  P := Pointer(Result);
  For I := 1 to Length do
    try
      P^ := EncodeChar(Q^);
      Inc(P);
      Inc(Q);
      Inc(L);
    except
      Case FErrorAction of
        eaException : raise;
        eaStop      : break;
        eaSkip:
          begin
            Inc(Q);
            Inc(L);
          end;
        eaIgnore:
          begin
            P^ := Char(Q^);
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
        eaReplace:
          begin
            P^ := Char(FDecodeReplaceChar);
            Inc(P);
            Inc(Q);
            Inc(L);
          end;
      end;
    end;
  ProcessedChars := L;
end;



{                                                                              }
{ ASCII                                                                        }
{                                                                              }
const
  ASCIIAliases = 15;
  ASCIIAlias: Array[0..ASCIIAliases - 1] of String = (
      'ASCII', 'US-ASCII', 'us',
      'ANSI_X3.4-1968', 'ANSI_X3.4-1986', 'iso-ir-6',
      'ISO_646.irv:1991', 'ISO_646.irv', 'ISO_646',
      'ISO-646', 'ISO646', 'ISO646-US',
      'IBM367', 'cp367', 'csASCII');

function ASCIIToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    raise EConvertError.Create('Invalid ASCII encoding');
  Result := WideChar(Ord(P));
end;

function WideCharToASCII(const Ch: WideChar): Char;
begin
  if Ord(Ch) >= $80 then
    raise EConvertError.Create('Can not convert to ASCII');
  Result := Char(Ord(Ch));
end;

function IsASCIIString(const S: String): Boolean;
var I: Integer;
    P: PChar;
begin
  P := Pointer(S);
  For I := 1 to Length(S) do
    if Ord(P^) >= $80 then
      begin
        Result := False;
        exit;
      end else
      Inc(P);
  Result := True;
end;

function IsASCIIWideBuf(const Buf: PWideChar; const Len: Integer): Boolean;
var I: Integer;
    P: PWideChar;
begin
  P := Buf;
  For I := 1 to Len do
    if Ord(P^) >= $80 then
      begin
        Result := False;
        exit;
      end else
      Inc(P);
  Result := True;
end;

function IsASCIIWideString(const S: WideString): Boolean;
begin
  Result := IsASCIIWideBuf(Pointer(S), Length(S));
end;

procedure LongToWide(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer);
var I: Integer;
    P: Pointer;
    Q: Pointer;
    V: LongWord;
begin
  if BufSize <= 0 then
    exit;
  P := Buf;
  Q := DestBuf;
  For I := 1 to BufSize div 4 do
    begin
      // convert 4 characters per iteration
      V := PLongWord(P)^;
      Inc(PLongWord(P));
      PLongWord(Q)^ := (V and $FF) or ((V and $FF00) shl 8);
      Inc(PLongWord(Q));
      V := V shr 16;
      PLongWord(Q)^ := (V and $FF) or ((V and $FF00) shl 8);
      Inc(PLongWord(Q));
    end;
  // convert remaining (<4)
  For I := 1 to BufSize mod 4 do
    begin
      PWord(Q)^ := PByte(P)^;
      Inc(PByte(P));
      Inc(PWord(Q));
    end;
end;

function LongStringToWideString(const S: String): WideString;
var L: Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  if L = 0 then
    exit;
  LongToWide(Pointer(S), L, Pointer(Result));
end;

procedure WideToLong(const Buf: Pointer; const Len: Integer;
    const DestBuf: Pointer);
var I: Integer;
    S: PWideChar;
    Q: PChar;
    V: LongWord;
    W: Word;
begin
  if Len <= 0 then
    exit;
  S := Buf;
  Q := DestBuf;
  For I := 1 to Len div 2 do
    begin
      // convert 2 characters per iteration
      V := PLongWord(S)^;
      if V and $FF00FF00 <> 0 then
        raise EConvertError.Create('Can not convert to long string');
      Q^ := Char(V);
      Inc(Q);
      Q^ := Char(V shr 16);
      Inc(Q);
      Inc(S, 2);
    end;
  // convert remaining character
  if Len mod 2 = 1 then
    begin
      W := Ord(S^);
      if W > $FF then
        raise EConvertError.Create('Can not convert to long string');
      Q^ := Char(W);
    end;
end;

function WideToLongString(const P: PWideChar; const Len: Integer): String;
var I: Integer;
    S: PWideChar;
    Q: PChar;
    V: WideChar;
begin
  if Len <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Len);
  S := P;
  Q := Pointer(Result);
  For I := 1 to Len do
    begin
      V := S^;
      if Ord(V) > $FF then
        raise EConvertError.Create('Can not convert to long string');
      Q^ := Char(Byte(V));
      Inc(S);
      Inc(Q);
    end;
end;

function WideStringToLongString(const S: WideString): String;
begin
  Result := WideToLongString(Pointer(S), Length(S));
end;

class function TASCIICodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucASCII;
end;

class function TASCIICodec.GetAliasCount: Integer;
begin
  Result := ASCIIAliases;
end;

class function TASCIICodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ASCIIAlias[Idx];
end;

function TASCIICodec.DecodeChar(const P: Char): WideChar;
begin
  Result := ASCIIToWideChar(P);
end;

function TASCIICodec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToASCII(Ch);
end;




{                                                                              }
{ ISO-8859-1 - Latin 1                                                         }
{ Western Europe and Americas: Afrikaans, Basque, Catalan, Danish, Dutch,      }
{ English, Faeroese, Finnish, French, Galician, German, Icelandic, Irish,      }
{ Italian, Norwegian, Portuguese, Spanish and Swedish.                         }
{ Default for HTTP Protocol                                                    }
{                                                                              }
const
  ISO8859_1Aliases = 8;
  ISO8859_1Alias: Array[0..ISO8859_1Aliases - 1] of String = (
      'ISO-8859-1', 'ISO_8859-1:1987', 'ISO_8859-1',
      'iso-ir-100', 'latin1', 'l1', 'IBM819', 'cp819');

function ISO8859_1ToWideChar(const P: Char): WideChar;
begin
  Result := WideChar(Ord(P));
end;

function WideCharToISO8859_1(const Ch: WideChar): Char;
begin
  if Ord(Ch) >= $100 then
    raise EConvertError.Create('Can not convert to ISO-8859-1');
  Result := Char(Ord(Ch));
end;

procedure ISO8859ToWide(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer);
begin
  LongToWide(Buf, BufSize, DestBuf);
end;

function ISO8859_1StringToWideString(const S: String): WideString;
var L: Integer;
begin
  L := Length(S);
  SetLength(Result, L);
  if L = 0 then
    exit;
  ISO8859ToWide(Pointer(S), L, Pointer(Result));
end;

class function TISO8859_1Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_1;
end;

class function TISO8859_1Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_1Aliases;
end;

class function TISO8859_1Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_1Alias[Idx];
end;

function TISO8859_1Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_1ToWideChar(P);
end;

function TISO8859_1Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_1(Ch);
end;

procedure TISO8859_1Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    var ProcessedBytes, DestLength: Integer);
var L, C: Integer;
begin
  L := BufSize;
  C := DestSize div Sizeof(WideChar);
  if C < L then
    L := C;
  if L < 0 then
    L := 0;
  ProcessedBytes := L;
  DestLength := L;
  ISO8859ToWide(Buf, L, DestBuf);
end;



{                                                                              }
{ ISO-8859-2 Latin 2                                                           }
{ Latin-written Slavic and Central European languages: Czech, German,          }
{ Hungarian, Polish, Romanian, Croatian, Slovak, Slovene.                      }
{                                                                              }
const
  ISO8859_2Aliases = 6;
  ISO8859_2Alias: Array[0..ISO8859_2Aliases - 1] of String = (
      'ISO-8859-2', 'ISO_8859-2:1987', 'ISO_8859-2',
      'iso-ir-101', 'latin2', 'l2');

const
  ISO8859_2Map: Array[$A0..$FF] of WideChar = (
      #$00A0, #$0104, #$02D8, #$0141, #$00A4, #$013D, #$015A, #$00A7,
      #$00A8, #$0160, #$015E, #$0164, #$0179, #$00AD, #$017D, #$017B,
      #$00B0, #$0105, #$02DB, #$0142, #$00B4, #$013E, #$015B, #$02C7,
      #$00B8, #$0161, #$015F, #$0165, #$017A, #$02DD, #$017E, #$017C,
      #$0154, #$00C1, #$00C2, #$0102, #$00C4, #$0139, #$0106, #$00C7,
      #$010C, #$00C9, #$0118, #$00CB, #$011A, #$00CD, #$00CE, #$010E,
      #$0110, #$0143, #$0147, #$00D3, #$00D4, #$0150, #$00D6, #$00D7,
      #$0158, #$016E, #$00DA, #$0170, #$00DC, #$00DD, #$0162, #$00DF,
      #$0155, #$00E1, #$00E2, #$0103, #$00E4, #$013A, #$0107, #$00E7,
      #$010D, #$00E9, #$0119, #$00EB, #$011B, #$00ED, #$00EE, #$010F,
      #$0111, #$0144, #$0148, #$00F3, #$00F4, #$0151, #$00F6, #$00F7,
      #$0159, #$016F, #$00FA, #$0171, #$00FC, #$00FD, #$0163, #$02D9);

function ISO8859_2ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    Result := ISO8859_2Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_2(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_2Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-2');
end;

class function TISO8859_2Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_2;
end;

class function TISO8859_2Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_2Aliases;
end;

class function TISO8859_2Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_2Alias[Idx];
end;

function TISO8859_2Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_2ToWideChar(P);
end;

function TISO8859_2Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_2(Ch);
end;



{                                                                              }
{ ISO-8859-3 - Latin 3                                                         }
{ Esperanto, Galician, Maltese, and Turkish.                                   }
{                                                                              }
const
  ISO8859_3Aliases = 6;
  ISO8859_3Alias: Array[0..ISO8859_3Aliases - 1] of String = (
      'ISO-8859-3', 'ISO_8859-3:1988', 'ISO_8859-3',
      'iso-ir-109', 'latin3', 'l3');

const
  ISO8859_3Map: Array[$A0..$FF] of WideChar = (
      #$00A0, #$0126, #$02D8, #$00A3, #$00A4, #$0000, #$0124, #$00A7,
      #$00A8, #$0130, #$015E, #$011E, #$0134, #$00AD, #$0000, #$017B,
      #$00B0, #$0127, #$00B2, #$00B3, #$00B4, #$00B5, #$0125, #$00B7,
      #$00B8, #$0131, #$015F, #$011F, #$0135, #$00BD, #$0000, #$017C,
      #$00C0, #$00C1, #$00C2, #$0000, #$00C4, #$010A, #$0108, #$00C7,
      #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
      #$0000, #$00D1, #$00D2, #$00D3, #$00D4, #$0120, #$00D6, #$00D7,
      #$011C, #$00D9, #$00DA, #$00DB, #$00DC, #$016C, #$015C, #$00DF,
      #$00E0, #$00E1, #$00E2, #$0000, #$00E4, #$010B, #$0109, #$00E7,
      #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
      #$0000, #$00F1, #$00F2, #$00F3, #$00F4, #$0121, #$00F6, #$00F7,
      #$011D, #$00F9, #$00FA, #$00FB, #$00FC, #$016D, #$015D, #$02D9);

function ISO8859_3ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    begin
      Result := ISO8859_3Map[Ord(P)];
      if Result = #$0000 then
        raise EConvertError.Create('Invalid ISO-8859-3 encoding');
    end else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_3(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_3Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-3');
end;

class function TISO8859_3Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_3;
end;

class function TISO8859_3Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_3Aliases;
end;

class function TISO8859_3Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_3Alias[Idx];
end;

function TISO8859_3Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_3ToWideChar(P);
end;

function TISO8859_3Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_3(Ch);
end;



{                                                                              }
{ ISO-8859-4 - Latin 4                                                         }
{ Scandinavia/Baltic (mostly covered by 8859-1 also): Estonian, Latvian, and   }
{ Lithuanian. It is an incomplete predecessor of Latin 6.                      }
{                                                                              }
const
  ISO8859_4Aliases = 6;
  ISO8859_4Alias: Array[0..ISO8859_4Aliases - 1] of String = (
      'ISO-8859-4', 'ISO_8859-4:1988', 'ISO_8859-4',
      'iso-ir-110', 'latin4', 'l4');

const
  ISO8859_4Map: Array[$A0..$FF] of WideChar = (
      #$00A0, #$0104, #$0138, #$0156, #$00A4, #$0128, #$013B, #$00A7,
      #$00A8, #$0160, #$0112, #$0122, #$0166, #$00AD, #$017D, #$00AF,
      #$00B0, #$0105, #$02DB, #$0157, #$00B4, #$0129, #$013C, #$02C7,
      #$00B8, #$0161, #$0113, #$0123, #$0167, #$014A, #$017E, #$014B,
      #$0100, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$012E,
      #$010C, #$00C9, #$0118, #$00CB, #$0116, #$00CD, #$00CE, #$012A,
      #$0110, #$0145, #$014C, #$0136, #$00D4, #$00D5, #$00D6, #$00D7,
      #$00D8, #$0172, #$00DA, #$00DB, #$00DC, #$0168, #$016A, #$00DF,
      #$0101, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$012F,
      #$010D, #$00E9, #$0119, #$00EB, #$0117, #$00ED, #$00EE, #$012B,
      #$0111, #$0146, #$014D, #$0137, #$00F4, #$00F5, #$00F6, #$00F7,
      #$00F8, #$0173, #$00FA, #$00FB, #$00FC, #$0169, #$016B, #$02D9);

function ISO8859_4ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    Result := ISO8859_4Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_4(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_4Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-4');
end;

class function TISO8859_4Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_4;
end;

class function TISO8859_4Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_4Aliases;
end;

class function TISO8859_4Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_4Alias[Idx];
end;

function TISO8859_4Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_4ToWideChar(P);
end;

function TISO8859_4Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_4(Ch);
end;



{                                                                              }
{ ISO-8859-5 - Cyrillic                                                        }
{ Bulgarian, Byelorussian, Macedonian, Russian, Serbian and Ukrainian.         }
{                                                                              }
const
  ISO8859_5Aliases = 5;
  ISO8859_5Alias: Array[0..ISO8859_5Aliases - 1] of String = (
      'ISO-8859-5', 'ISO_8859-5:1988', 'ISO_8859-5',
      'iso-ir-144', 'cyrillic');

function ISO8859_5ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $00..$A0, $AD : Result := WideChar(Ord(P));
    $F0 : Result := #$2116;
    $FD : Result := #$00A7;
  else
    Result := WideChar(Ord(P) + $0360);
  end;
end;

function WideCharToISO8859_5(const Ch: WideChar): Char;
begin
  if Ord(Ch) <= $A0 then
    Result := Char(Ord(Ch)) else
    Case Ch of
      #$2116 : Result := #$F0;
      #$00A7 : Result := #$FD;
      #$00AD : Result := #$AD;
      #$0401..#$045F :
        Case Ch of
          #$0450, #$045D, #$040D :
            raise EConvertError.Create('Can not convert to ISO-8859-5');
        else
          Result := Char(Ord(Ch) - $0360);
        end;
    else
      raise EConvertError.Create('Can not convert to ISO-8859-5');
    end;
end;

class function TISO8859_5Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_5;
end;

class function TISO8859_5Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_5Aliases;
end;

class function TISO8859_5Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_5Alias[Idx];
end;

function TISO8859_5Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_5ToWideChar(P);
end;

function TISO8859_5Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_5(Ch);
end;



{                                                                              }
{ ISO-8859-6 - Arabic                                                          }
{ Non-accented Arabic.                                                         }
{                                                                              }
const
  ISO8859_6Aliases = 7;
  ISO8859_6Alias: Array[0..ISO8859_6Aliases - 1] of String = (
      'ISO-8859-6', 'ISO_8859-6:1987', 'ISO_8859-6',
      'iso-ir-127', 'ECMA-114', 'ASMO-708', 'arabic');

function ISO8859_6ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $00..$A0, $A4, $AD : Result := WideChar(Ord(P));
    $AC, $BB, $BF, $C1..$DA, $E0..$F2 : Result := WideChar(Ord(P) + $0580);
  else
    raise EConvertError.Create('Invalid ISO-8859-6 encoding');
  end;
end;

function WideCharToISO8859_6(const Ch: WideChar): Char;
begin
  if Ord(Ch) <= $A0 then
    Result := Char(Ord(Ch)) else
    Case Ch of
      #$00A4 : Result := #$A4;
      #$00AD : Result := #$AD;
      #$062C, #$063B, #$063F, #$0641..#$065A, #$0660..#$0672 :
        Result := Char(Ord(Ch) - $0580);
    else
      raise EConvertError.Create('Can not convert to ISO-8859-6');
    end;
end;

class function TISO8859_6Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_6;
end;

class function TISO8859_6Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_6Aliases;
end;

class function TISO8859_6Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_6Alias[Idx];
end;

function TISO8859_6Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_6ToWideChar(P);
end;

function TISO8859_6Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_6(Ch);
end;



{                                                                              }
{ ISO-8859-7 - Modern Greek                                                    }
{ Greek.                                                                       }
{                                                                              }
const
  ISO8859_7Aliases = 8;
  ISO8859_7Alias: Array[0..ISO8859_7Aliases - 1] of String = (
      'ISO-8859-7', 'ISO_8859-7:1987', 'ISO_8859-7',
      'iso-ir-126', 'ELOT_928', 'ECMA-118', 'greek', 'greek8');

function ISO8859_7ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $00..$A0, $A6..$A9, $AB..$AD, $B0..$B3, $B7, $BB, $BD :
      Result := WideChar(Ord(P));
    $A1 : Result := #$2018;
    $A2 : Result := #$2019;
    $AF : Result := #$2015;
    $D2, $FF : raise EConvertError.Create('Invalid ISO-8859-7 encoding');
  else
    Result := WideChar(Ord(P) + $02D0);
  end;
end;

function WideCharToISO8859_7(const Ch: WideChar): Char;
begin
  if Ord(Ch) <= $A0 then
    Result := Char(Ord(Ch)) else
    Case Ch of
      #$00A6..#$00A9, #$00AB..#$00AD, #$00B0..#$00B3, #$00B7, #$00BB, #$00BD :
        Result := Char(Ord(Ch));
      #$2018 : Result := #$A1;
      #$2019 : Result := #$A2;
      #$2015 : Result := #$AF;
    else
      raise EConvertError.Create('Can not convert to ISO-8859-7');
    end;
end;

class function TISO8859_7Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_7;
end;

class function TISO8859_7Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_7Aliases;
end;

class function TISO8859_7Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_7Alias[Idx];
end;

function TISO8859_7Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_7ToWideChar(P);
end;

function TISO8859_7Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_7(Ch);
end;



{                                                                              }
{ ISO-8859-8 - Hebrew                                                          }
{ Non-accented Hebrew.                                                         }
{                                                                              }
const
  ISO8859_8Aliases = 5;
  ISO8859_8Alias: Array[0..ISO8859_8Aliases - 1] of String = (
      'ISO-8859-8', 'ISO_8859-8:1988', 'ISO_8859-8',
      'iso-ir-138', 'hebrew');

function ISO8859_8ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $00..$A0, $A2..$A9, $AB..$AE, $B0..$B9, $BB..$BE :
      Result := WideChar(Ord(P));
    $AA : Result := #$00D7;
    $AF : Result := #$203E;
    $BA : Result := #$00F7;
    $DF : Result := #$2017;
    $E0..$FA :
      Result := WideChar(Ord(P) + $04E0);
  else
    raise EConvertError.Create('Invalid ISO-8859-8 encoding')
  end;
end;

function WideCharToISO8859_8(const Ch: WideChar): Char;
begin
  if Ord(Ch) <= $A0 then
    Result := Char(Ord(Ch)) else
    Case Ch of
      #$00A2..#$00A9, #$00AB..#$00AE, #$00B0..#$00B9, #$00BB..#$00BE :
        Result := Char(Ord(Ch));
      #$00D7 : Result := #$AA;
      #$203E : Result := #$AF;
      #$00F7 : Result := #$BA;
      #$2017 : Result := #$DF;
      #$05C0..#$05DA : Result := Char(Ord(Ch) - $04E0);
    else
      raise EConvertError.Create('Can not convert to ISO-8859-8');
    end;
end;

class function TISO8859_8Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_8;
end;

class function TISO8859_8Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_8Aliases;
end;

class function TISO8859_8Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_8Alias[Idx];
end;

function TISO8859_8Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_8ToWideChar(P);
end;

function TISO8859_8Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_8(Ch);
end;



{                                                                              }
{ ISO-8859-9 - Latin 5                                                         }
{ Same as 8859-1 except for Turkish instead of Icelandic                       }
{                                                                              }
const
  ISO8859_9Aliases = 6;
  ISO8859_9Alias: Array[0..ISO8859_9Aliases - 1] of String = (
      'ISO-8859-9', 'ISO_8859-9:1989', 'ISO_8859-9',
      'iso-ir-148', 'latin5', 'l5');

function ISO8859_9ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $D0 : Result := #$011E;
    $DD : Result := #$0130;
    $DE : Result := #$015E;
    $F0 : Result := #$011F;
    $FD : Result := #$0131;
    $FE : Result := #$015F;
  else
    Result := WideChar(Ord(P));
  end;
end;

function WideCharToISO8859_9(const Ch: WideChar): Char;
begin
  Case Ch of
    #$011E : Result := #$D0;
    #$0130 : Result := #$DD;
    #$015E : Result := #$DE;
    #$011F : Result := #$F0;
    #$0131 : Result := #$FD;
    #$015F : Result := #$FE;
  else
    if Ord(Ch) <= $00FF then
      Result := Char(Ord(Ch)) else
      raise EConvertError.Create('Can not convert to ISO-8859-9');
  end;
end;

class function TISO8859_9Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_9;
end;

class function TISO8859_9Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_9Aliases;
end;

class function TISO8859_9Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_9Alias[Idx];
end;

function TISO8859_9Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_9ToWideChar(P);
end;

function TISO8859_9Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_9(Ch);
end;



{                                                                              }
{ ISO-8859-10 - Latin 6                                                        }
{ Latin6, for Lappish/Nordic/Eskimo languages: Adds the last Inuit             }
{ (Greenlandic) and Sami (Lappish) letters that were missing in Latin 4 to     }
{ cover the entire Nordic area.                                                }
{                                                                              }
const
  ISO8859_10Aliases = 6;
  ISO8859_10Alias: Array[0..ISO8859_10Aliases - 1] of String = (
      'ISO-8859-10', 'ISO_8859-10:1992', 'ISO_8859-10',
      'iso-ir-157', 'latin6', 'l6');

const
  ISO8859_10Map: Array[$A0..$FF] of WideChar = (
      #$00A0, #$0104, #$0112, #$0122, #$012A, #$0128, #$0136, #$00A7,
      #$013B, #$0110, #$0160, #$0166, #$017D, #$00AD, #$016A, #$014A,
      #$00B0, #$0105, #$0113, #$0123, #$012B, #$0129, #$0137, #$00B7,
      #$013C, #$0111, #$0161, #$0167, #$017E, #$2014, #$016B, #$014B,
      #$0100, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$012E,
      #$010C, #$00C9, #$0118, #$00CB, #$0116, #$00CD, #$00CE, #$00CF,
      #$00D0, #$0145, #$014C, #$00D3, #$00D4, #$00D5, #$00D6, #$0168,
      #$00D8, #$0172, #$00DA, #$00DB, #$00DC, #$00DD, #$00DE, #$00DF,
      #$0101, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$012F,
      #$010D, #$00E9, #$0119, #$00EB, #$0117, #$00ED, #$00EE, #$00EF,
      #$00F0, #$0146, #$014D, #$00F3, #$00F4, #$00F5, #$00F6, #$0169,
      #$00F8, #$0173, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$0138);

function ISO8859_10ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    Result := ISO8859_10Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_10(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_10Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-10');
end;

class function TISO8859_10Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_10;
end;

class function TISO8859_10Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_10Aliases;
end;

class function TISO8859_10Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_10Alias[Idx];
end;

function TISO8859_10Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_10ToWideChar(P);
end;

function TISO8859_10Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_10(Ch);
end;



{                                                                              }
{ ISO-8859-13 - Latin 7                                                        }
{                                                                              }
const
  ISO8859_13Aliases = 4;
  ISO8859_13Alias: Array[0..ISO8859_13Aliases - 1] of String = (
      'ISO-8859-13', 'ISO_8859-13', 'latin7', 'l7');

const
  ISO8859_13Map: Array[$A0..$FF] of WideChar = (
    #$00A0, #$201D, #$00A2, #$00A3, #$00A4, #$201E, #$00A6, #$00A7,
    #$00D8, #$00A9, #$0156, #$00AB, #$00AC, #$00AD, #$00AE, #$00C6,
    #$00B0, #$00B1, #$00B2, #$00B3, #$201C, #$00B5, #$00B6, #$00B7,
    #$00F8, #$00B9, #$0157, #$00BB, #$00BC, #$00BD, #$00BE, #$00E6,
    #$0104, #$012E, #$0100, #$0106, #$00C4, #$00C5, #$0118, #$0112,
    #$010C, #$00C9, #$0179, #$0116, #$0122, #$0136, #$012A, #$013B,
    #$0160, #$0143, #$0145, #$00D3, #$014C, #$00D5, #$00D6, #$00D7,
    #$0172, #$0141, #$015A, #$016A, #$00DC, #$017B, #$017D, #$00DF,
    #$0105, #$012F, #$0101, #$0107, #$00E4, #$00E5, #$0119, #$0113,
    #$010D, #$00E9, #$017A, #$0117, #$0123, #$0137, #$012B, #$013C,
    #$0161, #$0144, #$0146, #$00F3, #$014D, #$00F5, #$00F6, #$00F7,
    #$0173, #$0142, #$015B, #$016B, #$00FC, #$017B, #$017E, #$2019);

function ISO8859_13ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    Result := ISO8859_13Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_13(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_13Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-13');
end;

class function TISO8859_13Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_13;
end;

class function TISO8859_13Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_13Aliases;
end;

class function TISO8859_13Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_13Alias[Idx];
end;

function TISO8859_13Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_13ToWideChar(P);
end;

function TISO8859_13Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_13(Ch);
end;



{                                                                              }
{ ISO-8859-14 - Latin 8                                                        }
{                                                                              }
const
  ISO8859_14Aliases = 7;
  ISO8859_14Alias: Array[0..ISO8859_14Aliases - 1] of String = (
      'ISO-8859-14', 'ISO_8859-14:1998', 'ISO_8859-14',
      'iso-ir-199', 'latin8', 'l8', 'iso-celtic');

const
  ISO8859_14Map: Array[$A0..$FF] of WideChar = (
    #$00A0, #$1E02, #$1E03, #$00A3, #$010A, #$010B, #$1E0A, #$00A7,
    #$1E80, #$00A9, #$1E82, #$1E0B, #$1EF2, #$00AD, #$00AE, #$0178,
    #$1E1E, #$1E1F, #$0120, #$0121, #$1E40, #$1E41, #$00B6, #$1E56,
    #$1E81, #$1E57, #$1E83, #$1E60, #$1EF3, #$1E84, #$1E85, #$1E61,
    #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7,
    #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
    #$0174, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$1E6A,
    #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$00DD, #$0176, #$00DF,
    #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
    #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
    #$0175, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$1E6B,
    #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$0177, #$00FF);

function ISO8859_14ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $A0 then
    Result := ISO8859_14Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToISO8859_14(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $A0 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $A0 to $FF do
    if ISO8859_14Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to ISO-8859-14');
end;

class function TISO8859_14Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_14;
end;

class function TISO8859_14Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_14Aliases;
end;

class function TISO8859_14Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_14Alias[Idx];
end;

function TISO8859_14Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_14ToWideChar(P);
end;

function TISO8859_14Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_14(Ch);
end;



{                                                                              }
{ ISO-8859-15 - Latin 9                                                        }
{                                                                              }
const
  ISO8859_15Aliases = 6;
  ISO8859_15Alias: Array[0..ISO8859_15Aliases - 1] of String = (
      'ISO-8859-15', 'ISO_8859-15',
      'latin9', 'l9', 'latin0', 'l0');

function ISO8859_15ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $A4 : Result := #$20AC;
    $A6 : Result := #$00A6;
    $A8 : Result := #$0161;
    $B4 : Result := #$017D;
    $B8 : Result := #$017E;
    $BC : Result := #$0152;
    $BD : Result := #$0153;
    $BE : Result := #$0178;
  else
    Result := WideChar(Ord(P));
  end;
end;

function WideCharToISO8859_15(const Ch: WideChar): Char;
begin
  Case Ch of
    #$20AC : Result := #$A4;
    #$00A6 : Result := #$A6;
    #$0161 : Result := #$A8;
    #$017D : Result := #$B4;
    #$017E : Result := #$B8;
    #$0152 : Result := #$BC;
    #$0153 : Result := #$BD;
    #$0178 : Result := #$BE;
  else
    if Ord(Ch) <= $00FF then
      Result := Char(Ord(Ch)) else
      raise EConvertError.Create('Can not convert to ISO-8859-15');
  end;
end;

class function TISO8859_15Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucISO8859_15;
end;

class function TISO8859_15Codec.GetAliasCount: Integer;
begin
  Result := ISO8859_15Aliases;
end;

class function TISO8859_15Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := ISO8859_15Alias[Idx];
end;

function TISO8859_15Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := ISO8859_15ToWideChar(P);
end;

function TISO8859_15Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToISO8859_15(Ch);
end;



{                                                                              }
{ KOI8-R                                                                       }
{                                                                              }
const
  KOI8_RAliases = 1;
  KOI8_RAlias: Array[0..KOI8_RAliases - 1] of String = (
      'KOI8-R');

const
  KOI8_RMap: Array[$80..$FF] of WideChar = (
    #$2500, #$2502, #$250C, #$2510, #$2514, #$2518, #$251C, #$2524,
    #$252C, #$2534, #$253C, #$2580, #$2584, #$2588, #$258C, #$2590,
    #$2591, #$2592, #$2593, #$2320, #$25A0, #$2219, #$221A, #$2248,
    #$2264, #$2265, #$00A0, #$2321, #$00B0, #$00B2, #$00B7, #$00F7,
    #$2550, #$2551, #$2552, #$0451, #$2553, #$2554, #$2555, #$2556,
    #$2557, #$2558, #$2559, #$255A, #$255B, #$255C, #$255D, #$255E,
    #$255F, #$2560, #$2561, #$0401, #$2562, #$2563, #$2564, #$2565,
    #$2566, #$2567, #$2568, #$2569, #$256A, #$256B, #$256C, #$00A9,
    #$044E, #$0430, #$0431, #$0446, #$0434, #$0435, #$0444, #$0433,
    #$0445, #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E,
    #$043F, #$044F, #$0440, #$0441, #$0442, #$0443, #$0436, #$0432,
    #$044C, #$044B, #$0437, #$0448, #$044D, #$0449, #$0447, #$044A,
    #$042E, #$0410, #$0411, #$0426, #$0414, #$0415, #$0424, #$0413,
    #$0425, #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E,
    #$041F, #$042F, #$0420, #$0421, #$0422, #$0423, #$0416, #$0412,
    #$042C, #$042B, #$0417, #$0428, #$042D, #$0429, #$0427, #$042A);

function KOI8_RToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    Result := KOI8_RMap[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToKOI8_R(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if KOI8_RMap[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to KOI8-R');
end;

class function TKOI8_RCodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucKOI8_R;
end;

class function TKOI8_RCodec.GetAliasCount: Integer;
begin
  Result := KOI8_RAliases;
end;

class function TKOI8_RCodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := KOI8_RAlias[Idx];
end;

function TKOI8_RCodec.DecodeChar(const P: Char): WideChar;
begin
  Result := KOI8_RToWideChar(P);
end;

function TKOI8_RCodec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToKOI8_R(Ch);
end;



{                                                                              }
{ Mac Latin-2                                                                  }
{                                                                              }
const
  MacLatin2Aliases = 3;
  MacLatin2Alias: Array[0..MacLatin2Aliases - 1] of String = (
      'MacLatin2', 'Mac', 'Macintosh');

const
  MacLatin2Map: Array[$80..$FF] of WideChar = (
    #$00C4, #$0100, #$0101, #$00C9, #$0104, #$00D6, #$00DC, #$00E1,
    #$0105, #$010C, #$00E4, #$010D, #$0106, #$0107, #$00E9, #$0179,
    #$017A, #$010E, #$00ED, #$010F, #$0112, #$0113, #$0116, #$00F3,
    #$0117, #$00F4, #$00F6, #$00F5, #$00FA, #$011A, #$011B, #$00FC,
    #$2020, #$00B0, #$0118, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$0119, #$00A8, #$2260, #$0123, #$012E,
    #$012F, #$012A, #$2264, #$2265, #$012B, #$0136, #$2202, #$2211,
    #$0142, #$013B, #$013C, #$013D, #$013E, #$0139, #$013A, #$0145,
    #$0146, #$0143, #$00AC, #$221A, #$0144, #$0147, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$0148, #$0150, #$00D5, #$0151, #$014C,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$014D, #$0154, #$0155, #$0158, #$2039, #$203A, #$0159, #$0156,
    #$0157, #$0160, #$201A, #$201E, #$0161, #$015A, #$015B, #$00C1,
    #$0164, #$0165, #$00CD, #$017D, #$017E, #$016A, #$00D3, #$00D4,
    #$016B, #$016E, #$00DA, #$016F, #$0170, #$0171, #$0172, #$0173,
    #$00DD, #$00FD, #$0137, #$017B, #$0141, #$017C, #$0122, #$02C7);

function MacLatin2ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    Result := MacLatin2Map[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToMacLatin2(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if MacLatin2Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to MacLatin2');
end;

class function TMacLatin2Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucMacLatin2;
end;

class function TMacLatin2Codec.GetAliasCount: Integer;
begin
  Result := MacLatin2Aliases;
end;

class function TMacLatin2Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := MacLatin2Alias[Idx];
end;

function TMacLatin2Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := MacLatin2ToWideChar(P);
end;

function TMacLatin2Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToMacLatin2(Ch);
end;



{                                                                              }
{ Mac Roman                                                                    }
{                                                                              }
const
  MacRomanAliases = 1;
  MacRomanAlias: Array[0..MacRomanAliases - 1] of String = (
      'MacRoman');

const
  MacRomanMap: Array[$80..$FF] of WideChar = (
    #$00C4, #$00C5, #$00C7, #$00C9, #$00D1, #$00D6, #$00DC, #$00E1,
    #$00E0, #$00E2, #$00E4, #$00E3, #$00E5, #$00E7, #$00E9, #$00E8,
    #$00EA, #$00EB, #$00ED, #$00EC, #$00EE, #$00EF, #$00F1, #$00F3,
    #$00F2, #$00F4, #$00F6, #$00F5, #$00FA, #$00F9, #$00FB, #$00FC,
    #$2020, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$00DF,
    #$00AE, #$00A9, #$2122, #$00B4, #$00A8, #$2260, #$00C6, #$00D8,
    #$221E, #$00B1, #$2264, #$2265, #$00A5, #$00B5, #$2202, #$2211,
    #$220F, #$03C0, #$222B, #$00AA, #$00BA, #$2126, #$00E6, #$00F8,
    #$00BF, #$00A1, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$00C0, #$00C3, #$00D5, #$0152, #$0153,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$25CA,
    #$00FF, #$0178, #$2044, #$00A4, #$2039, #$203A, #$FB01, #$FB02,
    #$2021, #$00B7, #$201A, #$201E, #$2030, #$00C2, #$00CA, #$00C1,
    #$00CB, #$00C8, #$00CD, #$00CE, #$00CF, #$00CC, #$00D3, #$00D4,
    #$0000, #$00D2, #$00DA, #$00DB, #$00D9, #$0131, #$02C6, #$02DC,
    #$00AF, #$02D8, #$02D9, #$02DA, #$00B8, #$02DD, #$02DB, #$02C7);

function MacRomanToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    begin
      Result := MacRomanMap[Ord(P)];
      if Result = #$0000 then
        raise EConvertError.Create('Invalid Mac Roman encoding');
    end else
    Result := WideChar(Ord(P));
end;

function WideCharToMacRoman(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if MacRomanMap[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to MacRoman');
end;

class function TMacRomanCodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucMacRoman;
end;

class function TMacRomanCodec.GetAliasCount: Integer;
begin
  Result := MacRomanAliases;
end;

class function TMacRomanCodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := MacRomanAlias[Idx];
end;

function TMacRomanCodec.DecodeChar(const P: Char): WideChar;
begin
  Result := MacRomanToWideChar(P);
end;

function TMacRomanCodec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToMacRoman(Ch);
end;



{                                                                              }
{ Mac Cyrillic                                                                 }
{                                                                              }
const
  MacCyrillicAliases = 1;
  MacCyrillicAlias: Array[0..MacCyrillicAliases - 1] of String = (
      'MacCyrillic');

const
  MacCyrillicMap: Array[$80..$FF] of WideChar = (
    #$0410, #$0411, #$0412, #$0413, #$0414, #$0415, #$0416, #$0417,
    #$0418, #$0419, #$041A, #$041B, #$041C, #$041D, #$041E, #$041F,
    #$0420, #$0421, #$0422, #$0423, #$0424, #$0425, #$0426, #$0427,
    #$0428, #$0429, #$042A, #$042B, #$042C, #$042D, #$042E, #$042F,
    #$2020, #$00B0, #$00A2, #$00A3, #$00A7, #$2022, #$00B6, #$0406,
    #$00AE, #$00A9, #$2122, #$0402, #$0452, #$2260, #$0403, #$0453,
    #$221E, #$00B1, #$2264, #$2265, #$0456, #$00B5, #$2202, #$0408,
    #$0404, #$0454, #$0407, #$0457, #$0409, #$0459, #$040A, #$045A,
    #$0458, #$0405, #$00AC, #$221A, #$0192, #$2248, #$2206, #$00AB,
    #$00BB, #$2026, #$00A0, #$040B, #$045B, #$040C, #$045C, #$0455,
    #$2013, #$2014, #$201C, #$201D, #$2018, #$2019, #$00F7, #$201E,
    #$040E, #$045E, #$040F, #$045F, #$2116, #$0401, #$0451, #$044F,
    #$0430, #$0431, #$0432, #$0433, #$0434, #$0435, #$0436, #$0437,
    #$0438, #$0439, #$043A, #$043B, #$043C, #$043D, #$043E, #$043F,
    #$0440, #$0441, #$0442, #$0443, #$0444, #$0445, #$0446, #$0447,
    #$0448, #$0449, #$044A, #$044B, #$044C, #$044D, #$044E, #$00A4);

function MacCyrillicToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    Result := MacCyrillicMap[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToMacCyrillic(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if MacCyrillicMap[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to MacCyrillic');
end;

class function TMacCyrillicCodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucMacCyrillic;
end;

class function TMacCyrillicCodec.GetAliasCount: Integer;
begin
  Result := MacCyrillicAliases;
end;

class function TMacCyrillicCodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := MacCyrillicAlias[Idx];
end;

function TMacCyrillicCodec.DecodeChar(const P: Char): WideChar;
begin
  Result := MacCyrillicToWideChar(P);
end;

function TMacCyrillicCodec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToMacCyrillic(Ch);
end;



{                                                                              }
{ CP437 - DOSLatinUS                                                           }
{ Original IBM PC encoding                                                     }
{                                                                              }
const
  CP437Aliases = 3;
  CP437Alias: Array[0..CP437Aliases - 1] of String = (
      'IBM437', 'cp437', 'DOSLatinUS');
      
const
  CP437Map: Array[$80..$FF] of WideChar = (
    #$00C7, #$00FC, #$00E9, #$00E2, #$00E4, #$00E0, #$00E5, #$00E7,
    #$00EA, #$00EB, #$00E8, #$00EF, #$00EE, #$00EC, #$00C4, #$00C5,
    #$00C9, #$00E6, #$00C6, #$00F4, #$00F6, #$00F2, #$00FB, #$00F9,
    #$00FF, #$00D6, #$00DC, #$00A2, #$00A3, #$00A5, #$20A7, #$0192,
    #$00E1, #$00ED, #$00F3, #$00FA, #$00F1, #$00D1, #$00AA, #$00BA,
    #$00BF, #$2310, #$00AC, #$00BD, #$00BC, #$00A1, #$00AB, #$00BB,
    #$2591, #$2592, #$2593, #$2502, #$2524, #$2561, #$2562, #$2556,
    #$2555, #$2563, #$2551, #$2557, #$255D, #$255C, #$255B, #$2510,
    #$2514, #$2534, #$252C, #$251C, #$2500, #$253C, #$255E, #$255F,
    #$255A, #$2554, #$2569, #$2566, #$2560, #$2550, #$256C, #$2567,
    #$2568, #$2564, #$2565, #$2559, #$2558, #$2552, #$2553, #$256B,
    #$256A, #$2518, #$250C, #$2588, #$2584, #$258C, #$2590, #$2580,
    #$03B1, #$00DF, #$0393, #$03C0, #$03A3, #$03C3, #$00B5, #$03C4,
    #$03A6, #$0398, #$03A9, #$03B4, #$221E, #$03C6, #$03B5, #$2229,
    #$2261, #$00B1, #$2265, #$2264, #$2320, #$2321, #$00F7, #$2248,
    #$00B0, #$2219, #$00B7, #$221A, #$207F, #$00B2, #$25A0, #$00A0);

function CP437ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    Result := MacCyrillicMap[Ord(P)] else
    Result := WideChar(Ord(P));
end;

function WideCharToCP437(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if CP437Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to CP437');
end;

class function TCP437Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucCP437;
end;

class function TCP437Codec.GetAliasCount: Integer;
begin
  Result := CP437Aliases;
end;

class function TCP437Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := CP437Alias[Idx];
end;

function TCP437Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := CP437ToWideChar(P);
end;

function TCP437Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToCP437(Ch);
end;



{                                                                              }
{ Windows-1250                                                                 }
{                                                                              }
const
  Win1250Aliases = 3;
  Win1250Alias: Array[0..Win1250Aliases - 1] of String = (
      'windows-1250', 'cp1250', 'WinLatin2');

const
  Win1250Map: Array[$80..$FF] of WideChar = (
    #$20AC, #$0000, #$201A, #$0000, #$201E, #$2026, #$2020, #$2021,
    #$0000, #$2030, #$0160, #$2039, #$015A, #$0164, #$017D, #$0179,
    #$0000, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
    #$0000, #$2122, #$0161, #$203A, #$015B, #$0165, #$017E, #$017A,
    #$00A0, #$02C7, #$02D8, #$0141, #$00A4, #$0104, #$00A6, #$00A7,
    #$00A8, #$00A9, #$015E, #$00AB, #$00AC, #$00AD, #$00AE, #$017B,
    #$00B0, #$00B1, #$02DB, #$0142, #$00B4, #$00B5, #$00B6, #$00B7,
    #$00B8, #$0105, #$015F, #$00BB, #$013D, #$02DD, #$013E, #$017C,
    #$0154, #$00C1, #$00C2, #$0102, #$00C4, #$0139, #$0106, #$00C7,
    #$010C, #$00C9, #$0118, #$00CB, #$011A, #$00CD, #$00CE, #$010E,
    #$0110, #$0143, #$0147, #$00D3, #$00D4, #$0150, #$00D6, #$00D7,
    #$0158, #$016E, #$00DA, #$0170, #$00DC, #$00DD, #$0162, #$00DF,
    #$0155, #$00E1, #$00E2, #$0103, #$00E4, #$013A, #$0107, #$00E7,
    #$010D, #$00E9, #$0119, #$00EB, #$011B, #$00ED, #$00EE, #$010F,
    #$0111, #$0144, #$0148, #$00F3, #$00F4, #$0151, #$00F6, #$00F7,
    #$0159, #$016F, #$00FA, #$0171, #$00FC, #$00FD, #$0163, #$02D9);

function Win1250ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) >= $80 then
    begin
      Result := Win1250Map[Ord(P)];
      if Result = #$0000 then
        raise EConvertError.Create('Invalid Windows-1250 encoding');
    end else
    Result := WideChar(Ord(P));
end;

function WideCharToWin1250(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $FF do
    if Win1250Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to Windows-1250');
end;

class function TWin1250Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucWin1250;
end;

class function TWin1250Codec.GetAliasCount: Integer;
begin
  Result := Win1250Aliases;
end;

class function TWin1250Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := Win1250Alias[Idx];
end;

function TWin1250Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := Win1250ToWideChar(P);
end;

function TWin1250Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToWin1250(Ch);
end;


{                                                                              }
{ Windows-1251                                                                 }
{                                                                              }
const
  Win1251Aliases = 3;
  Win1251Alias: Array[0..Win1251Aliases - 1] of String = (
      'windows-1251', 'cp1251', 'WinCyrillic');

const
  Win1251Map: Array[$80..$BF] of WideChar = (
    #$0402, #$0403, #$201A, #$0453, #$201E, #$2026, #$2020, #$2021,
    #$20AC, #$2030, #$0409, #$2039, #$040A, #$040C, #$040B, #$040F,
    #$0452, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
    #$0000, #$2122, #$0459, #$203A, #$045A, #$045C, #$045B, #$045F,
    #$00A0, #$040E, #$045E, #$0408, #$00A4, #$0490, #$00A6, #$00A7,
    #$0401, #$00A9, #$0404, #$00AB, #$00AC, #$00AD, #$00AE, #$0407,
    #$00B0, #$00B1, #$0406, #$0456, #$0491, #$00B5, #$00B6, #$00B7,
    #$0451, #$2116, #$0454, #$00BB, #$0458, #$0405, #$0455, #$0457);

function Win1251ToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $00..$7F : Result := WideChar(Ord(P));
    $80..$BF :
      begin
        Result := Win1251Map[Ord(P)];
        if Result = #$0000 then
          raise EConvertError.Create('Invalid Windows-1251 encoding');
      end;
    $C0..$FF :
      Result := WideChar(Ord(P) + $0350);
    else
      raise EConvertError.Create('Invalid Windows-1251 encoding');
  end;
end;

function WideCharToWin1251(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  Case Ch of
    #$0410..#$044F : Result := Char(Ord(Ch) - $0350);
  else
    begin
      For I := $80 to $BF do
        if Win1251Map[I] = Ch then
          begin
            Result := Char(I);
            exit;
          end;
      raise EConvertError.Create('Can not convert to Windows-1251');
    end;
  end;
end;

class function TWin1251Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucWin1251;
end;

class function TWin1251Codec.GetAliasCount: Integer;
begin
  Result := Win1251Aliases;
end;

class function TWin1251Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := Win1251Alias[Idx];
end;

function TWin1251Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := Win1251ToWideChar(P);
end;

function TWin1251Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToWin1251(Ch);
end;



{                                                                              }
{ Windows-1252                                                                 }
{                                                                              }
const
  Win1252Aliases = 3;
  Win1252Alias: Array[0..Win1252Aliases - 1] of String = (
      'windows-1252', 'cp1252', 'WinLatin1');

const
  Win1252Map: Array[$80..$9F] of WideChar = (
    #$20AC, #$0000, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
    #$02C6, #$2030, #$0160, #$2039, #$0152, #$0000, #$017D, #$0000,
    #$0000, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
    #$02DC, #$2122, #$0161, #$203A, #$0153, #$0000, #$017E, #$0178);

function Win1252ToWideChar(const P: Char): WideChar;
begin
  if Ord(P) in [$80..$9F] then
    begin
      Result := Win1252Map[Ord(P)];
      if Result = #$0000 then
        raise EConvertError.Create('Invalid Windows-1252 encoding');
    end else
    Result := WideChar(Ord(P));
end;

function WideCharToWin1252(const Ch: WideChar): Char;
var I: Byte;
begin
  if Ord(Ch) < $80 then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  if (Ord(Ch) < $100) and (Ord(Ch) > $9F) then
    begin
      Result := Char(Ord(Ch));
      exit;
    end;
  For I := $80 to $9F do
    if Win1252Map[I] = Ch then
      begin
        Result := Char(I);
        exit;
      end;
  raise EConvertError.Create('Can not convert to Windows-1252');
end;

class function TWin1252Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucWin1252;
end;

class function TWin1252Codec.GetAliasCount: Integer;
begin
  Result := Win1252Aliases;
end;

class function TWin1252Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := Win1252Alias[Idx];
end;

function TWin1252Codec.DecodeChar(const P: Char): WideChar;
begin
  Result := Win1252ToWideChar(P);
end;

function TWin1252Codec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToWin1252(Ch);
end;



{                                                                              }
{ EBCDIC-US                                                                    }
{                                                                              }
const
  EBCDIC_USAliases = 2;
  EBCDIC_USAlias: Array[0..EBCDIC_USAliases - 1] of String = (
      'ebcdic-us', 'ebcdic');

function EBCDIC_USToWideChar(const P: Char): WideChar;
begin
  Case Ord(P) of
    $40 : Result := #$0020;   // SPACE
    $4A : Result := #$00A2;   // CENT SIGN
    $4B : Result := #$002E;   // FULL STOP
    $4C : Result := #$003C;   // LESS-THAN SIGN
    $4D : Result := #$0028;   // LEFT PARENTHESIS
    $4E : Result := #$002B;   // PLUS SIGN
    $4F : Result := #$007C;   // VERTICAL LINE
    $50 : Result := #$0026;   // AMPERSAND
    $5A : Result := #$0021;   // EXCLAMATION MARK
    $5B : Result := #$0024;   // DOLLAR SIGN
    $5C : Result := #$002A;   // ASTERISK
    $5D : Result := #$0029;   // RIGHT PARENTHESIS
    $5E : Result := #$003B;   // SEMICOLON
    $5F : Result := #$00AC;   // NOT SIGN
    $60 : Result := #$002D;   // HYPHEN-MINUS
    $61 : Result := #$002F;   // SOLIDUS
    $6A : Result := #$00A6;   // BROKEN BAR
    $6B : Result := #$002C;   // COMMA
    $6C : Result := #$0025;   // PERCENT SIGN
    $6D : Result := #$005F;   // LOW LINE
    $6E : Result := #$003E;   // GREATER-THAN SIGN
    $6F : Result := #$003F;   // QUESTION MARK
    $79 : Result := #$0060;   // GRAVE ACCENT
    $7A : Result := #$003A;   // COLON
    $7B : Result := #$0023;   // NUMBER SIGN
    $7C : Result := #$0040;   // COMMERCIAL AT
    $7D : Result := #$0027;   // APOSTROPHE
    $7E : Result := #$003D;   // EQUALS SIGN
    $7F : Result := #$0022;   // QUOTATION MARK
    $81..$89 : Result := WideChar(Ord(P) - $81 + $0061);   // LATIN SMALL LETTER A..I
    $91..$99 : Result := WideChar(Ord(P) - $91 + $006A);   // LATIN SMALL LETTER J..R
    $A1 : Result := #$007E;   // TILDE
    $A2..$A9 : Result := WideChar(Ord(P) - $A2 + $0073);   // LATIN SMALL LETTER S..Z
    $C0 : Result := #$007B;   // LEFT CURLY BRACKET
    $C1..$C9 : Result := WideChar(Ord(P) - $C1 + $0041);   // LATIN CAPITAL LETTER A..I
    $D0 : Result := #$007D;   // RIGHT CURLY BRACKET
    $D1..$D9 : Result := WideChar(Ord(P) - $D1 + $004A);   // LATIN CAPITAL LETTER J..R
    $E0 : Result := #$005C;   // REVERSE SOLIDUS
    $E2..$E9 : Result := WideChar(Ord(P) - $E2 + $0053);   // LATIN CAPITAL LETTER S
    $F0..$F9 : Result := WideChar(Ord(P) - $F0 + $0030);   // DIGIT ZERO
  else
    raise EConvertError.Create('Invalid EBCDIC-US encoding');
  end;
end;

function WideCharToEBCDIC_US(const Ch: WideChar): Char;
begin
  Case Ord(Ch) of
    $0020 : Result := #$40;   // SPACE
    $0021 : Result := #$5A;   // EXCLAMATION MARK
    $0022 : Result := #$7F;   // QUOTATION MARK
    $0023 : Result := #$7B;   // NUMBER SIGN
    $0024 : Result := #$5B;   // DOLLAR SIGN
    $0025 : Result := #$6C;   // PERCENT SIGN
    $0026 : Result := #$50;   // AMPERSAND
    $0027 : Result := #$7D;   // APOSTROPHE
    $0028 : Result := #$4D;   // LEFT PARENTHESIS
    $0029 : Result := #$5D;   // RIGHT PARENTHESIS
    $002A : Result := #$5C;   // ASTERISK
    $002B : Result := #$4E;   // PLUS SIGN
    $002C : Result := #$6B;   // COMMA
    $002D : Result := #$60;   // HYPHEN-MINUS
    $002E : Result := #$4B;   // FULL STOP
    $002F : Result := #$61;   // SOLIDUS
    $0030..$0039 : Result := Char(Ord(Ch) - $0030 + $F0);   // DIGIT ZERO-NINE
    $003A : Result := #$7A;   // COLON
    $003B : Result := #$5E;   // SEMICOLON
    $003C : Result := #$4C;   // LESS-THAN SIGN
    $003D : Result := #$7E;   // EQUALS SIGN
    $003E : Result := #$6E;   // GREATER-THAN SIGN
    $003F : Result := #$6F;   // QUESTION MARK
    $0040 : Result := #$7C;   // COMMERCIAL AT
    $0041..$0049 : Result := Char(Ord(Ch) - $0041 + $C1);   // LATIN CAPITAL LETTER A..I
    $004A..$0052 : Result := Char(Ord(Ch) - $004A + $D1);   // LATIN CAPITAL LETTER J..R
    $0053..$005A : Result := Char(Ord(Ch) - $0053 + $E2);   // LATIN CAPITAL LETTER S..Z
    $005C : Result := #$E0;   // REVERSE SOLIDUS
    $005F : Result := #$6D;   // LOW LINE
    $0060 : Result := #$79;   // GRAVE ACCENT
    $0061..$0069 : Result := Char(Ord(Ch) - $0061 + $81);   // LATIN SMALL LETTER A..I
    $006A..$0072 : Result := Char(Ord(Ch) - $006A + $91);   // LATIN SMALL LETTER J..R
    $0073..$007A : Result := Char(Ord(Ch) - $0073 + $A2);   // LATIN SMALL LETTER S..Z
    $007B : Result := #$C0;   // LEFT CURLY BRACKET
    $007C : Result := #$4F;   // VERTICAL LINE
    $007D : Result := #$D0;   // RIGHT CURLY BRACKET
    $007E : Result := #$A1;   // TILDE
    $00A2 : Result := #$4A;   // CENT SIGN
    $00A6 : Result := #$6A;   // BROKEN BAR
    $00AC : Result := #$5F;   // NOT SIGN
  else
    raise EConvertError.Create('Can not convert to EBCDIC-US');
  end;
end;

class function TEBCDIC_USCodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucEBCDIC_US;
end;

class function TEBCDIC_USCodec.GetAliasCount: Integer;
begin
  Result := EBCDIC_USAliases;
end;

class function TEBCDIC_USCodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := EBCDIC_USAlias[Idx];
end;

function TEBCDIC_USCodec.DecodeChar(const P: Char): WideChar;
begin
  Result := EBCDIC_USToWideChar(P);
end;

function TEBCDIC_USCodec.EncodeChar(const Ch: WideChar): Char;
begin
  Result := WideCharToEBCDIC_US(Ch);
end;



{                                                                              }
{ UTF-8                                                                        }
{                                                                              }
const
  UTF8Aliases = 2;
  UTF8Alias: Array[0..UTF8Aliases - 1] of String = (
      'UTF-8', 'utf8');

  MaxUTF8SequenceSize = 4;

{ UTF8ToUCS4Char returns UTF8ErrorNone if a valid UTF-8 sequence was decoded   }
{ (and Ch contains the decoded UCS4 character and SeqSize contains the size    }
{ of the UTF-8 sequence). If an incomplete UTF-8 sequence is encountered, the  }
{ function returns UTF8ErrorIncompleteEncoding and SeqSize > Size. If an       }
{ invalid UTF-8 sequence is encountered, the function returns                  }
{ UTF8ErrorInvalidEncoding and SeqSize (<= Size) is the size of the            }
{ invalid sequence, and Ch may be the intended character.                      }
function UTF8ToUCS4Char(const P: PChar; const Size: Integer;
    var SeqSize: Integer; var Ch: UCS4Char): TUTF8Error;
var C, D: Byte;
    V: LongWord;
    I: Integer;
begin
  if not Assigned(P) or (Size <= 0) then
    begin
      SeqSize := 0;
      Ch := 0;
      Result := UTF8ErrorInvalidBuffer;
      exit;
    end;
  C := Ord(P^);
  if C < $80 then
    begin
      SeqSize := 1;
      Ch := C;
      Result := UTF8ErrorNone;
      exit;
    end;
  // multi-byte characters always start with 11xxxxxx ($C0)
  // following bytes always start with 10xxxxxx ($80)
  if C and $C0 = $80 then
    begin
      SeqSize := 1;
      Ch := C;
      Result := UTF8ErrorInvalidEncoding;
      exit;
    end;
  if C and $20 = 0 then // 2-byte sequence
    begin
      SeqSize := 2;
      V := C and $1F;
    end else
  if C and $10 = 0 then // 3-byte sequence
    begin
      SeqSize := 3;
      V := C and $0F;
    end else
  if C and $08 = 0 then // 4-byte sequence (max needed for Unicode $0-$1FFFFF)
    begin
      SeqSize := 4;
      V := C and $07;
    end else
    begin
      SeqSize := 1;
      Ch := C;
      Result := UTF8ErrorInvalidEncoding;
      exit;
    end;
  if Size < SeqSize then // incomplete
    begin
      Ch := C;
      Result := UTF8ErrorIncompleteEncoding;
      exit;
    end;
  For I := 1 to SeqSize - 1 do
    begin
      D := Ord(P[I]);
      if D and $C0 <> $80 then // following byte must start with 10xxxxxx
        begin
          SeqSize := 1;
          Ch := C;
          Result := UTF8ErrorInvalidEncoding;
          exit;
        end;
      V := (V shl 6) or (D and $3F); // decode 6 bits
    end;
  Ch := V;
  Result := UTF8ErrorNone;
end;

function UTF8CharSize(const P: PChar; const Size: Integer): Integer;
var C: Byte;
    I: Integer;
    Q: PChar;
begin
  if not Assigned(P) or (Size <= 0) then
    begin
      Result := 0;
      exit;
    end;
  C := Ord(P^);
  if C < $80 then // 1-byte (ASCII value)
    Result := 1 else
  if C and $C0 = $80 then // invalid encoding
    Result := 1 else
    begin
      // multi-byte character
      if C and $20 = 0 then
        Result := 2 else
      if C and $10 = 0 then
        Result := 3 else
      if C and $08 = 0 then
        Result := 4 else
        begin
          Result := 1; // invalid encoding
          exit;
        end;
      if Size < Result then // incomplete encoding
        exit;
      Q := P;
      Inc(Q);
      For I := 1 to Result - 1 do
        if Ord(Q^) and $C0 <> $80 then
          begin
            Result := 1; // invalid encoding
            exit;
          end else
          Inc(Q);
    end;
end;

function UTF8BufLength(const P: PChar; const Size: Integer): Integer;
var Q: PChar;
    L, C: Integer;
begin
  Q := P;
  L := Size;
  Result := 0;
  While L > 0 do
    begin
      C := UTF8CharSize(Q, L);
      Dec(L, C);
      Inc(Q, C);
      Inc(Result);
    end;
end;

function UTF8StringLength(const S: String): Integer;
begin
  Result := UTF8BufLength(Pointer(S), Length(S));
end;

function UTF8ToWideChar(const P: PChar; const Size: Integer; var SeqSize: Integer; var Ch: WideChar): TUTF8Error;
var Ch4: UCS4Char;
begin
  Result := UTF8ToUCS4Char(P, Size, SeqSize, Ch4);
  if Ch4 > $FFFF then
    begin
      Result := UTF8ErrorOutOfRange;
      Ch := #$0000;
    end else
    Ch := WideChar(Ch4);
end;

function UCS4CharToUTF8CharSize(const Ch: UCS4Char): Integer;
begin
  if Ch < $80 then
    Result := 1 else
  if Ch < $800 then
    Result := 2 else
  if Ch < $10000 then
    Result := 3 else
  if Ch < $200000 then
    Result := 4 else
    raise EConvertError.Create('Character out of Unicode range');
end;

function WideBufToUTF8Size(const Buf: PWideChar; const Len: Integer): Integer;
var P: PWideChar;
    I: Integer;
    C: UCS4Char;
begin
  P := Buf;
  Result := 0;
  For I := 1 to Len do
    begin
      C := UCS4Char(P^);
      Inc(Result);
      if C >= $80 then
        if C >= $800 then
          Inc(Result, 2) else
          Inc(Result);
      Inc(P);
    end;
end;

function LongBufToUTF8Size(const Buf: PChar; const Len: Integer): Integer;
var P: PChar;
    I: Integer;
begin
  P := Buf;
  Result := 0;
  For I := 1 to Len do
    begin
      Inc(Result);
      if Ord(P^) >= $80 then
        Inc(Result);
      Inc(P);
    end;
end;

function WideStringToUTF8Size(const S: WideString): Integer;
begin
  Result := WideBufToUTF8Size(Pointer(S), Length(S));
end;

function LongStringToUTF8Size(const S: String): Integer;
begin
  Result := LongBufToUTF8Size(Pointer(S), Length(S));
end;

{ UCS4CharToUTF8 transforms the UCS4 char Ch to UTF-8 encoding. SeqSize        }
{ returns the number of bytes needed to transform Ch. Up to DestSize           }
{ bytes of the UTF-8 encoding will be placed in Dest.                          }
procedure UCS4CharToUTF8(const Ch: UCS4Char; const Dest: Pointer; const DestSize: Integer; var SeqSize: Integer);
var P: PByte;
begin
  P := Dest;
  if Ch < $80 then // ASCII (1-byte sequence)
    begin
      SeqSize := 1;
      if not Assigned(P) or (DestSize <= 0) then
        exit;
      P^ := Byte(Ch);
    end else
  if Ch < $800 then // 2-byte sequence
    begin
      SeqSize := 2;
      if not Assigned(P) or (DestSize <= 0) then
        exit;
      P^ := $C0 or Byte(Ch shr 6);
      if DestSize = 1 then
        exit;
      Inc(P);
      P^ := $80 or (Ch and $3F);
    end else
  if Ch < $10000 then // 3-byte sequence
    begin
      SeqSize := 3;
      if not Assigned(P) or (DestSize <= 0) then
        exit;
      P^ := $E0 or Byte(Ch shr 12);
      if DestSize = 1 then
        exit;
      Inc(P);
      P^ := $80 or ((Ch shr 6) and $3F);
      if DestSize = 2 then
        exit;
      Inc(P);
      P^ := $80 or (Ch and $3F);
    end else
  if Ch < $200000 then // 4-byte sequence
    begin
      SeqSize := 4;
      if not Assigned(P) or (DestSize <= 0) then
        exit;
      P^ := $F0 or Byte(Ch shr 18);
      if DestSize = 1 then
        exit;
      Inc(P);
      P^ := $80 or ((Ch shr 12) and $3F);
      if DestSize = 2 then
        exit;
      Inc(P);
      P^ := $80 or ((Ch shr 6) and $3F);
      if DestSize = 3 then
        exit;
      Inc(P);
      P^ := $80 or (Ch and $3F);
    end else
    raise EConvertError.Create('Character out of Unicode range');
end;

procedure WideCharToUTF8(const Ch: WideChar; const Dest: Pointer; const DestSize: Integer; var SeqSize: Integer);
begin
  UCS4CharToUTF8(Ord(Ch), Dest, DestSize, SeqSize);
end;

function UTF8StringToWideString(const S: String): WideString;
var P: PChar;
    Q: PWideChar;
    L, M, I: Integer;
    C: WideChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if IsASCIIString(S) then // optimize for ASCII strings
    begin
      Result := LongStringToWideString(S);
      exit;
    end;
  // Decode UTF-8
  P := Pointer(S);
  SetLength(Result, L); // maximum size
  Q := Pointer(Result);
  M := 0;
  Repeat
    UTF8ToWideChar(P, L, I, C);
    Assert(I > 0, 'I > 0');
    Q^ := C;
    Inc(Q);
    Inc(M);
    Inc(P, I);
    Dec(L, I);
  Until L <= 0;
  SetLength(Result, M); // actual size
end;

function UTF8StringToLongString(const S: String): String;
var P: PChar;
    Q: PChar;
    L, M, I: Integer;
    C: WideChar;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  if IsASCIIString(S) then // optimize for ASCII strings
    begin
      Result := S;
      exit;
    end;
  // Decode UTF-8
  P := Pointer(S);
  SetLength(Result, L); // maximum size
  Q := Pointer(Result);
  M := 0;
  Repeat
    UTF8ToWideChar(P, L, I, C);
    Assert(I > 0, 'I > 0');
    if Ord(C) > $FF then
      raise EConvertError.Create('Can not convert to long string');
    Q^ := Char(Ord(C));
    Inc(Q);
    Inc(M);
    Inc(P, I);
    Dec(L, I);
  Until L <= 0;
  SetLength(Result, M); // actual size
end;

function WideBufToUTF8String(const Buf: PWideChar; const Len: Integer): String;
var P: PWideChar;
    Q: PChar;
    I, M, N, J: Integer;
begin
  if Len = 0 then
    begin
      Result := '';
      exit;
    end;
  N := WideBufToUTF8Size(Buf, Len);
  if N = Len then // optimize for ASCII strings
    begin
      Result := WideToLongString(Buf, Len);
      exit;
    end;
  SetLength(Result, N);
  P := Buf;
  Q := Pointer(Result);
  M := 0;
  For I := 1 to Len do
    begin
      UCS4CharToUTF8(UCS4Char(P^), Q, N, J);
      Inc(P);
      Inc(Q, J);
      Dec(N, J);
      Inc(M, J);
    end;
  SetLength(Result, M); // actual size
end;

function LongStringToUTF8String(const S: String): String;
var P: PChar;
    Q: PChar;
    I, M, N, J, L: Integer;
begin
  P := Pointer(S);
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  N := LongBufToUTF8Size(P, L);
  if N = L then // optimize for ASCII strings
    begin
      Result := S;
      exit;
    end;
  SetLength(Result, N);
  Q := Pointer(Result);
  M := 0;
  For I := 1 to L do
    begin
      UCS4CharToUTF8(UCS4Char(Ord(P^)), Q, N, J);
      Inc(P);
      Inc(Q, J);
      Dec(N, J);
      Inc(M, J);
    end;
  SetLength(Result, M); // actual size
end;

function WideStringToUTF8String(const S: WideString): String;
begin
  Result := WideBufToUTF8String(Pointer(S), Length(S));
end;

function UCS4CharToUTF8String(const Ch: UCS4Char): String;
var Buf: Array[0..MaxUTF8SequenceSize - 1] of Byte;
    Size, I: Integer;
    P, Q: PChar;
begin
  Size := 0;
  UCS4CharToUTF8(Ch, @Buf, Sizeof(Buf), Size);
  SetLength(Result, Size);
  if Size > 0 then
    begin
      P := Pointer(Result);
      Q := @Buf;
      For I := 0 to Size - 1 do
        begin
          P^ := Q^;
          Inc(P);
          Inc(Q);
        end;
    end;
end;

function ISO8859_1StringToUTF8String(const S: String): String;
var P, Q: PChar;
    L, I, M, J: Integer;
begin
  L := Length(S);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  // Calculate size
  M := L;
  P := Pointer(S);
  For I := 1 to L do
    begin
      if Ord(P^) >= $80 then
        Inc(M); // 2 bytes required for #$80-#$FF
      Inc(P);
    end;
  // Check if conversion is required
  if M = L then
    begin
      // All characters are ASCII, return reference to same string
      Result := S;
      exit;
    end;
  // Convert
  SetLength(Result, M);
  Q := Pointer(Result);
  P := Pointer(S);
  For I := 1 to L do
    begin
      WideCharToUTF8(ISO8859_1ToWideChar(P^), Q, M, J);
      Inc(P);
      Inc(Q, J);
      Dec(M, J);
    end;
end;

function DetectUTF8Encoding(const P: PChar; const Size: Integer;
    var HeaderSize: Integer): Boolean;
var Q : PChar;
begin
  HeaderSize := 0;
  Result := False;
  if Assigned(P) and (Size >= 3) and (P^ = #$EF) then
    begin
      Q := P;
      Inc(Q);
      if Q^ = #$BB then
        begin
          Inc(Q);
          if Q^ = #$BF then
            begin
              HeaderSize := 3;
              Result := True;
            end;
        end;
    end;
end;

class function TUTF8Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucUTF8;
end;

class function TUTF8Codec.GetAliasCount: Integer;
begin
  Result := UTF8Aliases;
end;

class function TUTF8Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := UTF8Alias[Idx];
end;

procedure TUTF8Codec.Decode(const Buf: Pointer; const BufSize: Integer;
    const DestBuf: Pointer; const DestSize: Integer;
    var ProcessedBytes, DestLength: Integer);
var P: PChar;
    Q: PWideChar;
    L, I, M, N: Integer;
    R: TUTF8Error;
    C: WideChar;
begin
  P := Buf;
  L := BufSize;
  Q := DestBuf;
  N := DestSize div Sizeof(WideChar);
  if not Assigned(P) or (L <= 0) or not Assigned(Q) or (N <= 0) then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  M := 0;
  Repeat
    if M >= N then
      break;
    try
      R := UTF8ToWideChar(P, L, I, C);
      Case R of
        UTF8ErrorNone:
          begin
            Q^ := C;
            Inc(Q);
            Inc(M);
            Inc(P, I);
            Dec(L, I);
          end;
        UTF8ErrorInvalidEncoding:
          raise EConvertError.Create('Invalid UTF-8 encoding');
        UTF8ErrorIncompleteEncoding:
          begin
            ProcessedBytes := BufSize - L;
            DestLength := M;
            exit;
          end;
        UTF8ErrorOutOfRange :
          raise EConvertError.Create('UTF-8 encoding out of range');
      else
        raise EConvertError.Create('UTF-8 error (' + IntToStr(Ord(R)) + ')');
      end;
    except
      Case FErrorAction of
        eaException : raise;
        eaSkip:
          begin
            Inc(P, I);
            Dec(L, I);
          end;
        eaIgnore:
          begin
            Q^ := C;
            Inc(Q);
            Inc(M);
            Inc(P, I);
            Dec(L, I);
          end;
        eaReplace:
          begin
            Q^ := FDecodeReplaceChar;
            Inc(Q);
            Inc(M);
            Inc(P, I);
            Dec(L, I);
          end;
      end;
    end;
  Until L <= 0;
  ProcessedBytes := BufSize - L;
  DestLength := M;
end;

function TUTF8Codec.Encode(const S: PWideChar; const Length: Integer;
    var ProcessedChars: Integer): String;
var P: PWideChar;
    Q: PChar;
    I, L, M, J: Integer;
begin
  P := S;
  if not Assigned(P) or (Length <= 0) then
    begin
      ProcessedChars := 0;
      Result := '';
      exit;
    end;
  L := Length * 3;
  SetLength(Result, L);
  Q := Pointer(Result);
  M := 0;
  For I := 1 to Length do
    begin
      WideCharToUTF8(P^, Q, L, J);
      Inc(P);
      Inc(Q, J);
      Dec(L, J);
      Inc(M, J);
    end;
  ProcessedChars := Length;
  SetLength(Result, M);
end;



{                                                                              }
{ UTF-16                                                                       }
{                                                                              }
const
  UTF16Aliases = 3;
  UTF16Alias: Array[0..UTF16Aliases - 1] of String = (
      'UTF-16', 'UTF-16BE', 'utf16');

{ DetectUTF16Encoding returns True if the encoding was confirmed to be UTF-16. }
{ SwapEndian is True if it was detected that the UTF-16 data is in reverse     }
{ endian from that used by the cpu.                                            }
function DetectUTF16Encoding(const P: PChar; const Size: Integer;
    var SwapEndian: Boolean; var HeaderSize: Integer): Boolean;
begin
  if not Assigned(P) or (Size < Sizeof(WideChar)) then
    begin
      SwapEndian := False;
      HeaderSize := 0;
      Result := False;
    end else
  if PWideChar(P)^ = WideChar($FEFF) then
    begin
      SwapEndian := False;
      HeaderSize := Sizeof(WideChar);
      Result := True;
    end else
  if PWideChar(P)^ = WideChar($FFFE) then
    begin
      SwapEndian := True;
      HeaderSize := Sizeof(WideChar);
      Result := True;
    end else
    begin
      SwapEndian := False;
      HeaderSize := 0;
      Result := False;
    end;
end;

function SwapUTF16Endian(const P: WideChar): WideChar;
begin
  Result := WideChar(((Ord(P) and $FF) shl 8) or (Ord(P) shr 8));
end;

procedure TUTF16Codec.Init;
begin
  inherited Init;
  FSwapEndian := False;
end;

class function TUTF16Codec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucUTF16;
end;

class function TUTF16Codec.GetAliasCount: Integer;
begin
  Result := UTF16Aliases;
end;

class function TUTF16Codec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := UTF16Alias[Idx];
end;

procedure TUTF16Codec.Decode(const Buf: Pointer; const BufSize: Integer; const DestBuf: Pointer; const DestSize: Integer; var ProcessedBytes, DestLength: Integer);
var I, L, M: Integer;
    P, Q: PWideChar;
begin
  L := BufSize;
  if L > DestSize then
    L := DestSize;
  if L <= 1 then
    begin
      ProcessedBytes := 0;
      DestLength := 0;
      exit;
    end;
  Dec(L, L mod Sizeof(WideChar));
  M := L div Sizeof(WideChar);
  P := Buf;
  Q := DestBuf;
  if not FSwapEndian then
    Move(P^, Q^, L) else
    For I := 1 to M do
      begin
        Q^ := SwapUTF16Endian(P^);
        Inc(P);
        Inc(Q);
      end;
  DestLength := M;
  ProcessedBytes := L;
end;

function TUTF16Codec.Encode(const S: PWideChar; const Length: Integer;
    var ProcessedChars: Integer): String;
var I, L: Integer;
    P, Q: PWideChar;
begin
  if Length <= 0 then
    begin
      ProcessedChars := 0;
      Result := '';
      exit;
    end;
  L := Length * 2;
  SetLength(Result, L);
  if not FSwapEndian then
    Move(S^, Pointer(Result)^, L) else
    begin
      P := S;
      Q := Pointer(Result);
      For I := 1 to Length do
        begin
          Q^ := SwapUTF16Endian(P^);
          Inc(P);
          Inc(Q);
        end;
    end;
  ProcessedChars := Length;
end;

const
  UTF16REAliases = 2;
  UTF16REAlias: Array[0..UTF16REAliases - 1] of String = (
      'UTF-16LE', 'utf16le');

class function TUTF16RECodec.GetUnicodeCodecType: TUnicodeCodecType;
begin
  Result := ucUTF16RE;
end;

class function TUTF16RECodec.GetAliasCount: Integer;
begin
  Result := UTF16REAliases;
end;

class function TUTF16RECodec.GetAliasByIndex(const Idx: Integer): String;
begin
  Result := UTF16REAlias[Idx];
end;

procedure TUTF16RECodec.Init;
begin
  inherited Init;
  FSwapEndian := True;
end;



{                                                                              }
{ TUnicodeCodecType                                                            }
{                                                                              }
const
  UnicodeCodecMap: Array[TUnicodeCodecType] of TUnicodeCodecClass = (
      nil,
      TASCIICodec,
      TISO8859_1Codec, TISO8859_2Codec, TISO8859_3Codec, TISO8859_4Codec, TISO8859_5Codec,
      TISO8859_6Codec, TISO8859_7Codec, TISO8859_8Codec, TISO8859_9Codec, TISO8859_10Codec,
      TISO8859_13Codec, TISO8859_14Codec, TISO8859_15Codec,
      TKOI8_RCodec,
      TMacLatin2Codec, TMacRomanCodec, TMacCyrillicCodec,
      TCP437Codec,
      TWin1250Codec, TWin1251Codec, TWin1252Codec,
      TEBCDIC_USCodec,
      TUTF8Codec, TUTF16Codec, TUTF16RECodec);

function GetUnicodeCodecClassByType(const CodecType: TUnicodeCodecType): TUnicodeCodecClass;
begin
  Result := UnicodeCodecMap[CodecType];
end;

function GetUnicodeCodecTypeByName(const Name: String): TUnicodeCodecType;
var I: TUnicodeCodecType;
    C, J: Integer;
    D: TUnicodeCodecClass;
begin
  For I := Low(TUnicodeCodecType) to High(TUnicodeCodecType) do
    begin
      D := UnicodeCodecMap[I];
      if Assigned(D) then
        begin
          C := D.GetAliasCount;
          For J := 0 to C - 1 do
            if AnsiCompareText(Name, D.GetAliasByIndex(J)) = 0 then
              begin
                Result := I;
                exit;
              end;
        end;
    end;
  Result := ucCustom;
end;

function GetUnicodeCodecClassByName(const Name: String): TUnicodeCodecClass;
begin
  Result := UnicodeCodecMap[GetUnicodeCodecTypeByName(Name)];
end;



{                                                                              }
{ Unicode conversion functions                                                 }
{                                                                              }
function DetectUnicodeEncoding(const Buf: Pointer; const BufSize: Integer;
    var HeaderSize: Integer; var Codec: TUnicodeCodecType): Boolean;
var R : Boolean;
begin
  Result := DetectUTF16Encoding(Buf, BufSize, R, HeaderSize);
  if Result then
    if R then
      Codec := ucUTF16RE
    else
      Codec := ucUTF16
  else
  begin
    Result := DetectUTF8Encoding(Buf, BufSize, HeaderSize);
    if Result then
      Codec := ucUTF8
    else
      begin
        HeaderSize := 0;
        Codec := ucCustom;
      end;
  end;
end;

function DecodeUnicodeEncoding(const CodecClass: TUnicodeCodecClass;
    const Buf: Pointer; const BufSize: Integer;
    var ProcessedBytes: Integer): WideString;
var C: AUnicodeCodec;
begin
  if not Assigned(CodecClass) then
    begin
      Result := '';
      exit;
    end;
  C := CodecClass.Create;
  try
    C.DecodeStr(Buf, BufSize, Result, ProcessedBytes);
  finally
    C.Free;
  end;
end;

function DecodeUnicodeEncoding(const Codec: TUnicodeCodecType;
    const Buf: Pointer; const BufSize: Integer;
    var ProcessedBytes: Integer): WideString;
begin
  Result := DecodeUnicodeEncoding(UnicodeCodecMap[Codec], Buf, BufSize,
      ProcessedBytes);
end;

function EncodeUnicodeEncoding(const CodecClass: TUnicodeCodecClass;
    const S: WideString; var ProcessedChars: Integer): String;
var C: AUnicodeCodec;
begin
  if not Assigned(CodecClass) then
    begin
      ProcessedChars := 0;
      Result := '';
      exit;
    end;
  C := CodecClass.Create;
  try
    Result := C.Encode(Pointer(S), Length(S), ProcessedChars);
  finally
    C.Free;
  end;
end;

function EncodeUnicodeEncoding(const Codec: TUnicodeCodecType;
    const S: WideString; var ProcessedChars: Integer): String;
begin
  Result := EncodeUnicodeEncoding(UnicodeCodecMap[Codec], S, ProcessedChars);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
begin
  // UTF-8 test cases from RFC2279
  Assert(WideStringToUTF8String(WideCharsToWideString([#$0041, #$2262, #$0391, #$2E])) =
         #$41#$E2#$89#$A2#$CE#$91#$2E, 'WideStringToUTF8String');
  Assert(WideStringToUTF8String(WideCharsToWideString([#$D55C, #$AD6D, #$C5B4])) =
         #$ED#$95#$9C#$EA#$B5#$AD#$EC#$96#$B4, 'WideStringToUTF8String');
  Assert(WideStringToUTF8String(WideCharsToWideString([#$65E5, #$672C, #$8A9E])) =
         #$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E, 'WideStringToUTF8String');

  Assert(UTF8StringToWideString(#$41#$E2#$89#$A2#$CE#$91#$2E) =
         WideCharsToWideString([#$0041, #$2262, #$0391, #$2E]), 'UTF8StringToWideString');
  Assert(UTF8StringToWideString(#$ED#$95#$9C#$EA#$B5#$AD#$EC#$96#$B4) =
         WideCharsToWideString([#$D55C, #$AD6D, #$C5B4]), 'UTF8StringToWideString');
  Assert(UTF8StringToWideString(#$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E) =
         WideCharsToWideString([#$65E5, #$672C, #$8A9E]), 'UTF8StringToWideString');

  Assert(UTF8StringLength(#$41#$E2#$89#$A2#$CE#$91#$2E) = 4, 'UTF8StringLength');
  Assert(UTF8StringLength(#$ED#$95#$9C#$EA#$B5#$AD#$EC#$96#$B4) = 3, 'UTF8StringLength');
  Assert(UTF8StringLength(#$E6#$97#$A5#$E6#$9C#$AC#$E8#$AA#$9E) = 3, 'UTF8StringLength');

  // ASCII
  Assert(IsASCIIString('012XYZabc{}_ '), 'IsASCIIString');
  Assert(not IsASCIIString(#$80), 'IsASCIIString');
  Assert(IsASCIIString(''), 'IsASCIIString');
  Assert(IsASCIIWideString('012XYZabc{}_ '), 'IsASCIIWideString');
  Assert(not IsASCIIWideString(CharToWideChar(#$80)), 'IsASCIIWideString');
  Assert(not IsASCIIWideString(WideCharsToWideString([#$2262])), 'IsASCIIWideString');
  Assert(IsASCIIWideString(''), 'IsASCIIWideString');
end;



end.

