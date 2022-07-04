{                                                                              }
{                              Readers v3.08                                   }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cReaders.pas                     }
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
{   01/03/1999  0.01  Initial version.                                         }
{   26/06/1999  1.02  Created cStreams unit.                                   }
{   03/05/2002  3.03  Created cReaders unit.                                   }
{   13/05/2002  3.04  Added TBufferedReader and TSplitBufferReader.            }
{   13/07/2002  3.05  Moved text reader functionality to AReaderEx.            }
{   23/08/2002  3.06  Added SelfTest procedure.                                }
{   09/04/2003  3.07  Memory reader support for blocks with unknown size.      }
{   06/03/2004  3.08  Improvements to AReaderEx.                               }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cReaders;

interface

uses
  { Delphi }
  Windows,
  SysUtils,

  { Fundamentals }
  cUtils;



{                                                                              }
{ AReader                                                                      }
{   Abstract base class for a Reader.                                          }
{                                                                              }
{   Inherited classes must implement the abstract methods from AReader.        }
{                                                                              }
{   Read returns the actual number of bytes copied to the buffer.              }
{   Size returns -1 for reader's with unknown data size.                       }
{                                                                              }
type
  AReader = class
  protected
    function  GetPosition: Int64; virtual; abstract;
    procedure SetPosition(const Position: Int64); virtual; abstract;
    function  GetSize: Int64; virtual; abstract;

  public
    function  Read(var Buffer; const Size: Integer): Integer; virtual; abstract;

    property  Position: Int64 read GetPosition write SetPosition;
    property  Size: Int64 read GetSize;
    function  EOF: Boolean; virtual; abstract;
  end;
  EReader = class(Exception);



{                                                                              }
{ AReaderEx                                                                    }
{   Base class for Reader implementations. AReaderEx extends AReader with      }
{   commonly used functions.                                                   }
{                                                                              }
{   All methods in AReaderEx is implemented using calls to the abstract        }
{   methods in AReader. Reader implementations can override the virtual        }
{   methods in AReaderEx with more efficient versions.                         }
{                                                                              }
{   Match functions return True when a match is found. Match leaves the        }
{   reader's position unchanged except if a match is made and SkipOnMatch      }
{   is True.                                                                   }
{                                                                              }
{   Locate returns the offset (relative to the current position) of the        }
{   first match in the stream. Locate preserves the reader's position.         }
{   Locate returns -1 if a match was not made.                                 }
{                                                                              }
type
  TReaderEOLType  = (
      eolEOF,       // EOF          Files, Internet
      eolEOFAtEOF,  // #26 at EOF   Files
      eolCR,        // #13          Unix, Internet
      eolLF,        // #10          Internet
      eolCRLF,      // #13#10       MS-DOS, Windows, Internet
      eolLFCR);     // #10#13       Mac
  TReaderEOLTypes = Set of TReaderEOLType;

const
  DefaultReaderEOLTypes = [eolEOF, eolEOFAtEOF, eolCR, eolLF, eolCRLF, eolLFCR];

type
  AReaderEx = class(AReader)
  private
    function  SkipLineTerminator(const EOLTypes: TReaderEOLTypes): Integer;

  public
    procedure RaiseReadError(const Msg: String = '');
    procedure RaiseSeekError;

    procedure ReadBuffer(var Buffer; const Size: Integer);
    function  ReadStr(const Size: Integer): String; virtual;
    function  ReadWideStr(const Length: Integer): WideString; virtual;
    function  GetToEOF: String; virtual;
    function  GetAsString: String; virtual;

    function  ReadByte: Byte; virtual;
    function  ReadWord: Word;
    function  ReadLongWord: LongWord;
    function  ReadLongInt: LongInt;
    function  ReadInt64: Int64;
    function  ReadSingle: Single;
    function  ReadDouble: Double;
    function  ReadExtended: Extended;
    function  ReadPackedString: String;
    function  ReadPackedStringArray: StringArray;
    function  ReadPackedWideString: WideString;

    function  Peek(var Buffer; const Size: Integer): Integer; virtual;
    procedure PeekBuffer(var Buffer; const Size: Integer);
    function  PeekStr(const Size: Integer): String; virtual;

    function  PeekByte: Byte; virtual;
    function  PeekWord: Word;
    function  PeekLongWord: LongWord;
    function  PeekLongInt: LongInt;
    function  PeekInt64: Int64;

    function  Match(const Buffer; const Size: Integer;
              const CaseSensitive: Boolean = True): Integer; virtual;
    function  MatchBuffer(const Buffer; const Size: Integer;
              const CaseSensitive: Boolean = True): Boolean;
    function  MatchStr(const S: String;
              const CaseSensitive: Boolean = True): Boolean; virtual;

    function  MatchChar(const Ch: Char;
              const MatchNonMatch: Boolean = False;
              const SkipOnMatch: Boolean = False): Boolean; overload;
    function  MatchChar(const C: CharSet; var Ch: Char;
              const MatchNonMatch: Boolean = False;
              const SkipOnMatch: Boolean = False): Boolean; overload;

    procedure Skip(const Count: Integer = 1); virtual;
    procedure SkipByte;

    function  SkipAll(const Ch: Char; const MatchNonMatch: Boolean = False;
              const MaxCount: Integer = -1): Integer; overload;
    function  SkipAll(const C: CharSet; const MatchNonMatch: Boolean = False;
              const MaxCount: Integer = -1): Integer; overload;

    function  Locate(const Ch: Char;
              const LocateNonMatch: Boolean = False;
              const MaxOffset: Integer = -1): Integer; overload; virtual;
    function  Locate(const C: CharSet;
              const LocateNonMatch: Boolean = False;
              const MaxOffset: Integer = -1): Integer; overload; virtual;

    function  LocateBuffer(const Buffer; const Size: Integer;
              const MaxOffset: Integer = -1;
              const CaseSensitive: Boolean = True): Integer; virtual;
    function  LocateStr(const S: String;
              const MaxOffset: Integer = -1;
              const CaseSensitive: Boolean = True): Integer; virtual;

    function  ExtractAll(const C: CharSet;
              const ExtractNonMatch: Boolean = False;
              const MaxCount: Integer = -1): String;

    function  ExtractLine(const MaxLineLength: Integer = -1;
              const EOLTypes: TReaderEOLTypes = DefaultReaderEOLTypes): String;
    function  SkipLine(const MaxLineLength: Integer = -1;
              const EOLTypes: TReaderEOLTypes = DefaultReaderEOLTypes): Boolean;
  end;



{                                                                              }
{ TMemoryReader                                                                }
{   Reader implementation for a memory block.                                  }
{                                                                              }
{   If the reader is initialized with Size = -1, the content is unsized and    }
{   EOF will always return False.                                              }
{                                                                              }
type
  TMemoryReader = class(AReaderEx)
  protected
    FData : Pointer;
    FSize : Integer;
    FPos  : Integer;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

  public
    constructor Create(const Data: Pointer; const Size: Integer);

    property  Data: Pointer read FData;
    property  Size: Integer read FSize;
    procedure SetData(const Data: Pointer; const Size: Integer);

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;

    function  ReadByte: Byte; override;
    function  ReadLongInt: LongInt;
    function  ReadInt64: Int64;
    function  PeekByte: Byte; override;
    function  Match(const Buffer; const Size: Integer;
              const CaseSensitive: Boolean = True): Integer; override;
    procedure Skip(const Count: Integer = 1); override;
  end;
  EMemoryReader = class(EReader);



{                                                                              }
{ TStringReader                                                                }
{   Memory reader implementation for a reference counted string (long string). }
{                                                                              }
type
  TStringReader = class(TMemoryReader)
  protected
    FDataString : String;

    procedure SetDataString(const S: String);

  public
    constructor Create(const Data: String);

    property  DataString: String read FDataString write SetDataString;
    function  GetAsString: String; override;
  end;



{                                                                              }
{ TFileReader                                                                  }
{   Reader implementation for a file.                                          }
{                                                                              }
type
  TFileReaderAccessHint = (
      frahNone,
      frahRandomAccess,
      frahSequentialAccess);
  TFileReader = class(AReaderEx)
  protected
    FHandle      : Integer;
    FHandleOwner : Boolean;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

  public
    constructor Create(const FileName: String;
                const AccessHint: TFileReaderAccessHint = frahNone); overload;
    constructor Create(const FileHandle: Integer; const HandleOwner: Boolean = False); overload;
    destructor Destroy; override;

    property  Handle: Integer read FHandle;
    property  HandleOwner: Boolean read FHandleOwner;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;
  end;
  EFileReader = class(EReader);

function  ReadFileToStr(const FileName: String): String;



{                                                                              }
{ AReaderProxy                                                                 }
{   Base class for Reader Proxies.                                             }
{                                                                              }
type
  AReaderProxy = class(AReaderEx)
  protected
    FReader      : AReaderEx;
    FReaderOwner : Boolean;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

  public
    constructor Create(const Reader: AReaderEx; const ReaderOwner: Boolean = True);
    destructor Destroy; override;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;

    property  Reader: AReaderEx read FReader;
    property  ReaderOwner: Boolean read FReaderOwner write FReaderOwner;
  end;



{                                                                              }
{ TReaderProxy                                                                 }
{   Reader Proxy implementation.                                               }
{                                                                              }
{   Proxies a block of data from Reader from the Reader's current position.    }
{   Size specifies the size of the data to be proxied, or if Size = -1,        }
{   all data up to EOF is proxied.                                             }
{                                                                              }
type
  TReaderProxy = class(AReaderProxy)
  protected
    FOffset : Int64;
    FSize   : Int64;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

  public
    constructor Create(const Reader: AReaderEx; const ReaderOwner: Boolean = True;
                const Size: Int64 = -1);

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;
  end;



{                                                                              }
{ TBufferedReader                                                              }
{   ReaderProxy implementation for buffered reading.                           }
{                                                                              }
type
  TBufferedReader = class(AReaderProxy)
  protected
    FBufferSize  : Integer;
    FPos         : Int64;
    FBuffer      : Pointer;
    FBufUsed     : Integer;
    FBufPos      : Integer;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

    function  FillBuf: Boolean;
    procedure BufferByte;
    function  PosBuf(const C: CharSet; const LocateNonMatch: Boolean;
              const MaxOffset: Integer): Integer;

  public
    constructor Create(const Reader: AReaderEx; const BufferSize: Integer = 128;
                const ReaderOwner: Boolean = True);
    destructor Destroy; override;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;

    function  ReadByte: Byte; override;
    function  PeekByte: Byte; override;
    procedure Skip(const Count: Integer = 1); override;

    function  Locate(const C: CharSet; const LocateNonMatch: Boolean = False;
              const MaxOffset: Integer = -1): Integer; override;

    property  BufferSize: Integer read FBufferSize;
    procedure InvalidateBuffer;
  end;



{                                                                              }
{ TSplitBufferedReader                                                         }
{   ReaderProxy implementation for split buffered reading.                     }
{                                                                              }
{   One buffer is used for read-ahead buffering, the other for seek-back       }
{   buffering.                                                                 }
{                                                                              }
type
  TSplitBufferedReader = class(AReaderProxy)
  protected
    FBufferSize  : Integer;
    FPos         : Int64;
    FBuffer      : Array[0..1] of Pointer;
    FBufUsed     : Array[0..1] of Integer;
    FBufNr       : Integer;
    FBufPos      : Integer;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

    function  BufferStart: Integer;
    function  BufferRemaining: Integer;
    function  MoveBuf(var Dest: PByte; var Remaining: Integer): Boolean;
    function  FillBuf(var Dest: PByte; var Remaining: Integer): Boolean;

  public
    constructor Create(const Reader: AReaderEx; const BufferSize: Integer = 128;
                const ReaderOwner: Boolean = True);
    destructor Destroy; override;

    property  BufferSize: Integer read FBufferSize;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;

    procedure InvalidateBuffer;
  end;



{                                                                              }
{ TBufferedFileReader                                                          }
{   TBufferedReader instance using a TFileReader.                              }
{                                                                              }
type
  TBufferedFileReader = class(TBufferedReader)
  public
    constructor Create(const FileName: String; const BufferSize: Integer = 512); overload;
    constructor Create(const FileHandle: Integer; const HandleOwner: Boolean = False;
                const BufferSize: Integer = 512); overload;
  end;



{                                                                              }
{ TSplitBufferedFileReader                                                     }
{   TSplitBufferedReader instance using a TFileReader.                         }
{                                                                              }
type
  TSplitBufferedFileReader = class(TSplitBufferedReader)
  public
    constructor Create(const FileName: String; const BufferSize: Integer = 512);
  end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation



{                                                                              }
{ AReaderEx                                                                    }
{                                                                              }
const
  DefaultBufSize = 2048;

procedure AReaderEx.RaiseReadError(const Msg: String);
var S : String;
begin
  if Msg = '' then
    S := 'Read error'
  else
    S := Msg;
  raise EReader.Create(S);
end;

procedure AReaderEx.RaiseSeekError;
begin
  raise EReader.Create('Seek error');
end;

procedure AReaderEx.ReadBuffer(var Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if Read(Buffer, Size) <> Size then
    RaiseReadError;
end;

function AReaderEx.ReadStr(const Size: Integer): String;
var L : Integer;
begin
  if Size <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Size);
  L := Read(Pointer(Result)^, Size);
  if L <= 0 then
    begin
      Result := '';
      exit;
    end;
  if L < Size then
    SetLength(Result, L);
end;

function AReaderEx.ReadWideStr(const Length: Integer): WideString;
var L : Integer;
begin
  if Length <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Length);
  L := Read(Pointer(Result)^, Length * Sizeof(WideChar)) div Sizeof(WideChar);
  if L <= 0 then
    begin
      Result := '';
      exit;
    end;
  if L < Length then
    SetLength(Result, L);
end;

function AReaderEx.GetToEOF: String;
var S    : Int64;
    B    : Array[0..DefaultBufSize - 1] of Byte;
    I, J : Integer;
    L, N : Integer;
    R    : Boolean;
    Q    : PChar;
begin
  S := GetSize;
  if S < 0 then // size unknown
    begin
      Q := nil;
      Result := '';
      L := 0; // actual size
      N := 0; // allocated size
      R := EOF;
      While not R do
        begin
          I := Read(B[0], DefaultBufSize);
          R := EOF;
          if I > 0 then
            begin
              J := L + I;
              if J > N then
                begin
                  // allocate the exact buffer size for the first two reads
                  // allocate double the buffer size from the third read
                  if J <= DefaultBufSize * 2 then
                    N := J
                  else
                    N := J * 2;
                  SetLength(Result, N);
                  Q := Pointer(Result);
                  Inc(Q, L);
                end;
              Move(B[0], Q^, I);
              Inc(Q, I);
              Inc(L, I);
            end
          else
            if not R then
              RaiseReadError;
        end;
      if L < N then
        // set exact result size
        SetLength(Result, L);
    end
  else
    // size known
    Result := ReadStr(S - GetPosition);
end;

function AReaderEx.GetAsString: String;
begin
  SetPosition(0);
  Result := GetToEOF;
end;

function AReaderEx.ReadByte: Byte;
begin
  ReadBuffer(Result, Sizeof(Byte));
end;

function AReaderEx.ReadWord: Word;
begin
  ReadBuffer(Result, Sizeof(Word));
end;

function AReaderEx.ReadLongWord: LongWord;
begin
  ReadBuffer(Result, Sizeof(LongWord));
end;

function AReaderEx.ReadLongInt: LongInt;
begin
  ReadBuffer(Result, Sizeof(LongInt));
end;

function AReaderEx.ReadInt64: Int64;
begin
  ReadBuffer(Result, Sizeof(Int64));
end;

function AReaderEx.ReadSingle: Single;
begin
  ReadBuffer(Result, Sizeof(Single));
end;

function AReaderEx.ReadDouble: Double;
begin
  ReadBuffer(Result, Sizeof(Double));
end;

function AReaderEx.ReadExtended: Extended;
begin
  ReadBuffer(Result, Sizeof(Extended));
end;

function AReaderEx.ReadPackedString: String;
var L : Integer;
begin
  L := ReadLongInt;
  if L < 0 then
    raise EReader.Create('Invalid string');
  Result := ReadStr(L);
end;

function AReaderEx.ReadPackedStringArray: StringArray;
var I, L : Integer;
begin
  L := ReadLongInt;
  if L < 0 then
    raise EReader.Create('Invalid array');
  SetLength(Result, L);
  For I := 0 to L - 1 do
    Result[I] := ReadPackedString;
end;

function AReaderEx.ReadPackedWideString: WideString;
var L : Integer;
begin
  L := ReadLongInt;
  if L < 0 then
    raise EReader.Create('Invalid string');
  Result := ReadWideStr(L);
end;

function AReaderEx.Peek(var Buffer; const Size: Integer): Integer;
var P : Int64;
begin
  P := GetPosition;
  Result := Read(Buffer, Size);
  if Result > 0 then
    SetPosition(P);
end;

procedure AReaderEx.PeekBuffer(var Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if Peek(Buffer, Size) <> Size then
    RaiseReadError;
end;

function AReaderEx.PeekStr(const Size: Integer): String;
var L : Integer;
begin
  if Size <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Size);
  L := Peek(Pointer(Result)^, Size);
  if L <= 0 then
    begin
      Result := '';
      exit;
    end;
  if L < Size then
    SetLength(Result, L);
end;

function AReaderEx.PeekByte: Byte;
begin
  PeekBuffer(Result, Sizeof(Byte));
end;

function AReaderEx.PeekWord: Word;
begin
  PeekBuffer(Result, Sizeof(Word));
end;

function AReaderEx.PeekLongWord: LongWord;
begin
  PeekBuffer(Result, Sizeof(LongWord));
end;

function AReaderEx.PeekLongInt: LongInt;
begin
  PeekBuffer(Result, Sizeof(LongInt));
end;

function AReaderEx.PeekInt64: Int64;
begin
  PeekBuffer(Result, Sizeof(Int64));
end;

{ Returns the number of characters read and matched, or -1 if no match }
function AReaderEx.Match(const Buffer; const Size: Integer;
    const CaseSensitive: Boolean): Integer;
var B : Pointer;
    F : Array[0..DefaultBufSize - 1] of Byte;
    M : Boolean;
    R : Boolean;
begin
  if Size <= 0 then
    begin
      Result := -1;
      exit;
    end;
  M := Size > DefaultBufSize;
  if M then
    GetMem(B, Size)
  else
    B := @F[0];
  try
    Result := Peek(B^, Size);
    if Result <= 0 then
      exit;
    if CaseSensitive then
      R := CompareMem(Buffer, B^, Result)
    else
      R := CompareMemNoCase(Buffer, B^, Result) = crEqual;
    if not R then
      Result := -1;
  finally
    if M then
      FreeMem(B);
  end;
end;

function AReaderEx.MatchBuffer(const Buffer; const Size: Integer;
    const CaseSensitive: Boolean): Boolean;
var I : Integer;
begin
  I := Match(Buffer, Size, CaseSensitive);
  if I < 0 then
    begin
      Result := False;
      exit;
    end;
  if I < Size then
    RaiseReadError;
  Result := True;
end;

function AReaderEx.MatchStr(const S: String; const CaseSensitive: Boolean): Boolean;
begin
  Result := MatchBuffer(Pointer(S)^, Length(S), CaseSensitive);
end;

function AReaderEx.MatchChar(const Ch: Char; const MatchNonMatch: Boolean;
    const SkipOnMatch: Boolean): Boolean;
begin
  if EOF then
    begin
      Result := False;
      exit;
    end;
  Result := (Char(PeekByte) = Ch) xor MatchNonMatch;
  if Result and SkipOnMatch then
    Skip(Sizeof(Byte));
end;

function AReaderEx.MatchChar(const C: CharSet; var Ch: Char; const MatchNonMatch: Boolean;
    const SkipOnMatch: Boolean): Boolean;
begin
  if EOF then
    begin
      Result := False;
      exit;
    end;
  Ch := Char(PeekByte);
  Result := (Ch in C) xor MatchNonMatch;
  if Result and SkipOnMatch then
    Skip(Sizeof(Byte));
end;

procedure AReaderEx.Skip(const Count: Integer);
begin
  if Count < 0 then
    raise EReader.Create('Skip error');
  if Count = 0 then
    exit;
  SetPosition(GetPosition + Count);
end;

procedure AReaderEx.SkipByte;
begin
  Skip(Sizeof(Byte));
end;

function AReaderEx.SkipAll(const Ch: Char; const MatchNonMatch: Boolean;
    const MaxCount: Integer): Integer;
begin
  Result := 0;
  While (MaxCount < 0) or (Result < MaxCount) do
    if not MatchChar(Ch, MatchNonMatch, True) then
      exit
    else
      Inc(Result);
end;

function AReaderEx.SkipAll(const C: CharSet; const MatchNonMatch: Boolean;
    const MaxCount: Integer): Integer;
var Ch : Char;
begin
  Result := 0;
  While (MaxCount < 0) or (Result < MaxCount) do
    if not MatchChar(C, Ch, MatchNonMatch, True) then
      exit
    else
      Inc(Result);
end;

function AReaderEx.Locate(const Ch: Char; const LocateNonMatch: Boolean;
    const MaxOffset: Integer): Integer;
var P : Int64;
    I : Integer;
begin
  P := GetPosition;
  I := 0;
  While not EOF and ((MaxOffset < 0) or (I <= MaxOffset)) do
    if (Char(ReadByte) = Ch) xor LocateNonMatch then
      begin
        SetPosition(P);
        Result := I;
        exit;
      end
    else
      Inc(I);
  SetPosition(P);
  Result := -1;
end;

function AReaderEx.Locate(const C: CharSet; const LocateNonMatch: Boolean;
    const MaxOffset: Integer): Integer;
var P : Int64;
    I : Integer;
begin
  P := GetPosition;
  I := 0;
  While not EOF and ((MaxOffset < 0) or (I <= MaxOffset)) do
    if (Char(ReadByte) in C) xor LocateNonMatch then
      begin
        SetPosition(P);
        Result := I;
        exit;
      end
    else
      Inc(I);
  SetPosition(P);
  Result := -1;
end;

function AReaderEx.LocateBuffer(const Buffer; const Size: Integer;
    const MaxOffset: Integer; const CaseSensitive: Boolean): Integer;
var P    : Int64;
    I, J : Integer;
    B    : Pointer;
    R, M : Boolean;
    F    : Array[0..DefaultBufSize - 1] of Byte;
begin
  if Size <= 0 then
    begin
      Result := -1;
      exit;
    end;
  M := Size > DefaultBufSize;
  if M then
    GetMem(B, Size)
  else
    B := @F[0];
  try
    P := GetPosition;
    I := 0;
    While not EOF and
          ((MaxOffset < 0) or (I <= MaxOffset)) do
      begin
        J := Read(B^, Size);
        if J < Size then
          begin
            if EOF then
              begin
                SetPosition(P);
                Result := -1;
                exit;
              end
            else
              RaiseReadError;
          end;
        if CaseSensitive then
          R := CompareMem(Buffer, B^, Size)
        else
          R := CompareMemNoCase(Buffer, B^, Size) = crEqual;
        if R then
          begin
            SetPosition(P);
            Result := I;
            exit;
          end
        else
          begin
            Inc(I);
            SetPosition(P + I);
          end;
      end;
    SetPosition(P);
    Result := -1;
  finally
    if M then
      FreeMem(B);
  end;
end;

function AReaderEx.LocateStr(const S: String; const MaxOffset: Integer;
    const CaseSensitive: Boolean): Integer;
begin
  Result := LocateBuffer(Pointer(S)^, Length(S), MaxOffset, CaseSensitive);
end;

function AReaderEx.ExtractAll(const C: CharSet; const ExtractNonMatch: Boolean;
    const MaxCount: Integer): String;
var I : Integer;
begin
  I := Locate(C, not ExtractNonMatch, MaxCount);
  if I = -1 then
    if MaxCount = -1 then
      Result := GetToEOF
    else
      Result := ReadStr(MaxCount)
  else
    Result := ReadStr(I);
end;

const
  csNewLineNone    : CharSet = [];
  csNewLineCR      : CharSet = [#13];
  csNewLineLF      : CharSet = [#10];
  csNewLineEOF     : CharSet = [#26];
  csNewLineCRLF    : CharSet = [#10, #13];
  csNewLineCREOF   : CharSet = [#13, #26];
  csNewLineLFEOF   : CharSet = [#10, #26];
  csNewLineCRLFEOF : CharSet = [#10, #13, #26];

procedure FirstNewLineCharsFromEOLTypes(const EOLTypes: TReaderEOLTypes;
    var Chars: PCharSet);
begin
  if [eolCR, eolCRLF] * EOLTypes <> [] then
    if [eolLF, eolLFCR] * EOLTypes <> [] then
      if eolEOFAtEOF in EOLTypes then
        Chars := @csNewLineCRLFEOF else
        Chars := @csNewLineCRLF
    else
      if eolEOFAtEOF in EOLTypes then
        Chars := @csNewLineCREOF else
        Chars := @csNewLineCR
  else
    if [eolLF, eolLFCR] * EOLTypes <> [] then
      if eolEOFAtEOF in EOLTypes then
        Chars := @csNewLineLFEOF else
        Chars := @csNewLineLF
  else
    if eolEOFAtEOF in EOLTypes then
      Chars := @csNewLineEOF
    else
      Chars := @csNewLineNone;
end;

function AReaderEx.SkipLineTerminator(const EOLTypes: TReaderEOLTypes): Integer;
var C, D : Char;
    R    : Boolean;
begin
  C := Char(ReadByte);
  if ((C = #10) and ([eolLFCR, eolLF] * EOLTypes = [eolLF])) or
     ((C = #13) and ([eolCRLF, eolCR] * EOLTypes = [eolCR])) then
    begin
      Result := 1;
      exit;
    end;
  if not (((C = #10) and (eolLFCR in EOLTypes)) or
          ((C = #13) and (eolCRLF in EOLTypes))) then
    begin
      SetPosition(GetPosition - 1);
      Result := 0;
      exit;
    end;
  R := EOF;
  if (C = #26) and (eolEOFAtEOF in EOLTypes) and R then
    begin
      Result := 1;
      exit;
    end;
  if R then
    begin
      if ((C = #10) and (eolLF in EOLTypes)) or
         ((C = #13) and (eolCR in EOLTypes)) then
        begin
          Result := 1;
          exit;
        end;
      SetPosition(GetPosition - 1);
      Result := 0;
      exit;
    end;
  D := Char(ReadByte);
  if ((C = #13) and (D = #10) and (eolCRLF in EOLTypes)) or
     ((C = #10) and (D = #13) and (eolLFCR in EOLTypes)) then
    begin
      Result := 2;
      exit;
    end;
  if ((C = #13) and (eolCR in EOLTypes)) or
     ((C = #10) and (eolLF in EOLTypes)) then
    begin
      SetPosition(GetPosition - 1);
      Result := 1;
      exit;
    end;
  SetPosition(GetPosition - 2);
  Result := 0;
end;

function AReaderEx.ExtractLine(const MaxLineLength: Integer;
    const EOLTypes: TReaderEOLTypes): String;
var NewLineChars : PCharSet;
    Fin : Boolean;
begin
  if EOLTypes = [] then
    begin
      Result := '';
      exit;
    end;
  if EOLTypes = [eolEOF] then
    begin
      Result := GetToEOF;
      exit;
    end;
  FirstNewLineCharsFromEOLTypes(EOLTypes, NewLineChars);
  Result := '';
  Repeat
    Result := Result + ExtractAll(NewLineChars^, True, MaxLineLength);
    if EOF then
      if eolEOF in EOLTypes then
        exit
      else
        RaiseReadError;
    Fin := (MaxLineLength >= 0) and (Length(Result) >= MaxLineLength);
    if not Fin then
      begin
        Fin := SkipLineTerminator(EOLTypes) > 0;
        if not Fin then
          Result := Result + Char(ReadByte);
      end;
  Until Fin;
end;

function AReaderEx.SkipLine(const MaxLineLength: Integer;
    const EOLTypes: TReaderEOLTypes): Boolean;
var NewLineChars : PCharSet;
    I    : Integer;
    P, Q : Int64;
    Fin  : Boolean;
begin
  if EOLTypes = [] then
    begin
      Result := False;
      exit;
    end;
  Result := True;
  if EOLTypes = [eolEOF] then
    begin
      Position := Size;
      exit;
    end;
  FirstNewLineCharsFromEOLTypes(EOLTypes, NewLineChars);
  Fin := False;
  Repeat
    I := Locate(NewLineChars^, False, MaxLineLength);
    if I < 0 then
      if MaxLineLength < 0 then
        begin
          Position := Size;
          exit;
        end
      else
        begin
          P := Position + MaxLineLength;
          Q := Size;
          if P > Q then
            P := Q;
          Position := P;
          exit;
        end
    else
      begin
        Skip(I);
        if EOF then
          exit;
        Fin := SkipLineTerminator(EOLTypes) > 0;
        if not Fin then
          SkipByte;
      end;
  Until Fin;
end;



{                                                                              }
{ TMemoryReader                                                                }
{   For Size < 0 the memory reader assumes no size limit.                      }
{                                                                              }
constructor TMemoryReader.Create(const Data: Pointer; const Size: Integer);
begin
  inherited Create;
  SetData(Data, Size);
end;

procedure TMemoryReader.SetData(const Data: Pointer; const Size: Integer);
begin
  FData := Data;
  FSize := Size;
  FPos := 0;
end;

function TMemoryReader.GetPosition: Int64;
begin
  Result := FPos;
end;

procedure TMemoryReader.SetPosition(const Position: Int64);
var S : Integer;
begin
  S := FSize;
  if (Position < 0) or ((S >= 0) and (Position > S)) then
    RaiseSeekError;
  FPos := Integer(Position);
end;

function TMemoryReader.GetSize: Int64;
begin
  Result := FSize;
end;

function TMemoryReader.Read(var Buffer; const Size: Integer): Integer;
var L, S, I : Integer;
    P       : PByte;
begin
  I := FPos;
  S := FSize;
  if (Size <= 0) or ((S >= 0) and (I >= S)) then
    begin
      Result := 0;
      exit;
    end;
  if S < 0 then
    L := Size
  else
    begin
      L := S - I;
      if Size < L then
        L := Size;
    end;
  P := FData;
  Inc(P, I);
  MoveMem(P^, Buffer, L);
  Result := L;
  Inc(FPos, L);
end;

function TMemoryReader.EOF: Boolean;
var S : Integer;
begin
  S := FSize;
  if S < 0 then
    Result := False
  else
    Result := FPos >= S;
end;

function TMemoryReader.ReadByte: Byte;
var I, S : Integer;
    P    : PByte;
begin
  I := FPos;
  S := FSize;
  if (S >= 0) and (I >= S) then
    RaiseReadError;
  P := FData;
  Inc(P, I);
  Result := P^;
  Inc(FPos);
end;

function TMemoryReader.ReadLongInt: LongInt;
var I, S : Integer;
begin
  I := FPos;
  S := FSize;
  if (S >= 0) and (I + Sizeof(LongInt) > S) then
    RaiseReadError;
  Result := PLongInt(@PChar(FData)[I])^;
  Inc(FPos, Sizeof(LongInt));
end;

function TMemoryReader.ReadInt64: Int64;
var I, S : Integer;
begin
  I := FPos;
  S := FSize;
  if (S >= 0) and (I + Sizeof(Int64) > S) then
    RaiseReadError;
  Result := PInt64(@PChar(FData)[I])^;
  Inc(FPos, Sizeof(Int64));
end;

function TMemoryReader.PeekByte: Byte;
var I, S : Integer;
    P    : PByte;
begin
  I := FPos;
  S := FSize;
  if (S >= 0) and (I >= S) then
    RaiseReadError;
  P := FData;
  Inc(P, I);
  Result := P^;
end;

function TMemoryReader.Match(const Buffer; const Size: Integer;
    const CaseSensitive: Boolean): Integer;
var L, S : Integer;
    P    : PByte;
    R    : Boolean;
begin
  S := FSize;
  if S < 0 then
    L := Size
  else
    begin
      L := S - FPos;
      if L > Size then
        L := Size;
    end;
  if L <= 0 then
    begin
      Result := -1;
      exit;
    end;
  P := FData;
  Inc(P, FPos);
  if CaseSensitive then
    R := CompareMem(Buffer, P^, L) else
    R := CompareMemNoCase(Buffer, P^, L) = crEqual;
  if R then
    Result := L else
    Result := -1;
end;

procedure TMemoryReader.Skip(const Count: Integer);
var S, I : Integer;
begin
  if Count <= 0 then
    exit;
  S := FSize;
  if S < 0 then
    Inc(FPos, Count)
  else
    begin
      I := FPos + Count;
      if I > S then
        RaiseSeekError;
      FPos := I;
    end;
end;



{                                                                              }
{ TStringReader                                                                }
{                                                                              }
constructor TStringReader.Create(const Data: String);
begin
  FDataString := Data;
  inherited Create(Pointer(FDataString), Length(FDataString));
end;

procedure TStringReader.SetDataString(const S: String);
begin
  FDataString := S;
  SetData(Pointer(FDataString), Length(FDataString));
end;

function TStringReader.GetAsString: String;
begin
  Result := FDataString;
end;



{                                                                              }
{ TFileReader                                                                  }
{                                                                              }
constructor TFileReader.Create(const FileName: String;
    const AccessHint: TFileReaderAccessHint);
{$IFDEF OS_WIN32}
var F : LongWord;
{$ENDIF}
begin
  inherited Create;
  {$IFDEF OS_WIN32}
  F := FILE_ATTRIBUTE_NORMAL;
  Case AccessHint of
    frahNone             : ;
    frahRandomAccess     : F := F or FILE_FLAG_RANDOM_ACCESS;
    frahSequentialAccess : F := F or FILE_FLAG_SEQUENTIAL_SCAN;
  end;
  FHandle := Integer(Windows.CreateFile(PChar(FileName), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, F, 0));
  {$ELSE}
  FHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  {$ENDIF}
  if FHandle = -1 then
    raise EFileReader.Create('File open error');
  FHandleOwner := True;
end;

constructor TFileReader.Create(const FileHandle: Integer; const HandleOwner: Boolean);
begin
  inherited Create;
  FHandle := FileHandle;
  FHandleOwner := HandleOwner;
end;

destructor TFileReader.Destroy;
begin
  if FHandleOwner and (FHandle <> -1) and (FHandle <> 0) then
    FileClose(FHandle);
  inherited Destroy;
end;

function TFileReader.GetPosition: Int64;
begin
  Result := FileSeek(FHandle, Int64(0), 1);
  if Result = -1 then
    raise EFileReader.Create('File error');
end;

procedure TFileReader.SetPosition(const Position: Int64);
begin
  if FileSeek(FHandle, Position, 0) = -1 then
    raise EFileReader.Create('File seek error');
end;

function TFileReader.GetSize: Int64;
var I : Int64;
begin
  I := GetPosition;
  Result := FileSeek(FHandle, Int64(0), 2);
  SetPosition(I);
  if Result = -1 then
    raise EFileReader.Create('File error');
end;

function TFileReader.Read(var Buffer; const Size: Integer): Integer;
var I : Integer;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  I := FileRead(FHandle, Buffer, Size);
  if I <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Result := I;
end;

function TFileReader.EOF: Boolean;
begin
  Result := GetPosition >= GetSize;
end;



{ ReadFileToStr                                                                }
function ReadFileToStr(const FileName: String): String;
var F : TFileReader;
begin
  F := TFileReader.Create(FileName);
  try
    Result := F.GetAsString;
  finally
    F.Free;
  end;
end;



{                                                                              }
{ AReaderProxy                                                                 }
{                                                                              }
constructor AReaderProxy.Create(const Reader: AReaderEx; const ReaderOwner: Boolean);
begin
  Assert(Assigned(Reader));
  inherited Create;
  FReader := Reader;
  FReaderOwner := ReaderOwner;
end;

destructor AReaderProxy.Destroy;
begin
  if FReaderOwner then
    FreeAndNil(FReader);
  inherited Destroy;
end;

function AReaderProxy.Read(var Buffer; const Size: Integer): Integer;
begin
  Result := FReader.Read(Buffer, Size);
end;

function AReaderProxy.EOF: Boolean;
begin
  Result := FReader.EOF;
end;

function AReaderProxy.GetPosition: Int64;
begin
  Result := FReader.GetPosition;
end;

procedure AReaderProxy.SetPosition(const Position: Int64);
begin
  FReader.SetPosition(Position);
end;

function AReaderProxy.GetSize: Int64;
begin
  Result := FReader.GetSize;
end;



{                                                                              }
{ TReaderProxy                                                                 }
{                                                                              }
constructor TReaderProxy.Create(const Reader: AReaderEx; const ReaderOwner: Boolean;
    const Size: Int64);
begin
  inherited Create(Reader, ReaderOwner);
  FOffset := Reader.GetPosition;
  FSize := Size;
end;

function TReaderProxy.GetPosition: Int64;
begin
  Result := FReader.GetPosition - FOffset;
end;

procedure TReaderProxy.SetPosition(const Position: Int64);
begin
  if Position < 0 then
    raise EReader.Create('Seek error');
  if (FSize >= 0) and (Position > FOffset + FSize) then
    raise EReader.Create('Seek error');
  FReader.SetPosition(FOffset + Position);
end;

function TReaderProxy.GetSize: Int64;
begin
  Result := FReader.GetSize;
  if Result >= FOffset then
    Dec(Result, FOffset)
  else
    Result := -1;
  if (FSize >= 0) and (FSize < Result) then
    Result := FSize;
end;

function TReaderProxy.EOF: Boolean;
begin
  Result := FReader.EOF;
  if Result or (FSize < 0) then
    exit;
  Result := FReader.Position >= FOffset + FSize;
end;

function TReaderProxy.Read(var Buffer; const Size: Integer): Integer;
var L : Integer;
    M : Int64;
begin
  L := Size;
  if FSize >= 0 then
    begin
      M := FSize - (FReader.Position - FOffset);
      if M < L then
        L := Integer(M);
    end;
  if L <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Result := FReader.Read(Buffer, L);
end;



{                                                                              }
{ TBufferedReader                                                              }
{                                                                              }
constructor TBufferedReader.Create(const Reader: AReaderEx; const BufferSize: Integer;
    const ReaderOwner: Boolean);
begin
  inherited Create(Reader, ReaderOwner);
  FBufferSize := BufferSize;
  GetMem(FBuffer, BufferSize);
  FPos := Reader.GetPosition;
end;

destructor TBufferedReader.Destroy;
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer);
  inherited Destroy;
end;

function TBufferedReader.GetPosition: Int64;
begin
  Result := FPos;
end;

function TBufferedReader.GetSize: Int64;
begin
  Result := FReader.GetSize;
end;

procedure TBufferedReader.SetPosition(const Position: Int64);
var B, C : Int64;
begin
  B := Position - FPos;
  if B = 0 then
    exit;
  C := B + FBufPos;
  if (C >= 0) and (C <= FBufUsed) then
    begin
      Inc(FBufPos, Integer(B));
      FPos := Position;
      exit;
    end;
  FReader.SetPosition(Position);
  FPos := Position;
  FBufPos := 0;
  FBufUsed := 0;
end;

procedure TBufferedReader.Skip(const Count: Integer);
var I : Integer;
    P : Int64;
begin
  if Count < 0 then
    raise EReader.Create('Seek error');
  if Count = 0 then
    exit;
  I := FBufUsed - FBufPos;
  if I >= Count then
    begin
      Inc(FBufPos, Count);
      Inc(FPos, Count);
      exit;
    end;
  P := GetPosition + Count;
  FReader.SetPosition(P);
  FPos := P;
  FBufPos := 0;
  FBufUsed := 0;
end;

// Internal function FillBuf
// Returns True if buffer was only partially filled
function TBufferedReader.FillBuf: Boolean;
var P : PByte;
    L, N : Integer;
begin
  L := FBufferSize - FBufUsed;
  if L <= 0 then
    begin
      Result := False;
      exit;
    end;
  P := FBuffer;
  Inc(P, FBufPos);
  N := FReader.Read(P^, L);
  Inc(FBufUsed, N);
  Result := N < L;
end;

function TBufferedReader.Read(var Buffer; const Size: Integer): Integer;
var L, M : Integer;
    P, Q : PByte;
    R : Boolean;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Q := @Buffer;
  M := Size;
  R := False;
  Repeat
    L := FBufUsed - FBufPos;
    if L > M then
      L := M;
    if L > 0 then
      begin
        P := FBuffer;
        Inc(P, FBufPos);
        MoveMem(P^, Q^, L);
        Inc(FBufPos, L);
        Inc(FPos, L);
        Dec(M, L);
        if M = 0 then
          begin
            Result := Size;
            exit;
          end;
        Inc(Q, L);
      end;
    FBufPos := 0;
    FBufUsed := 0;
    if R then
      begin
        Result := Size - M;
        exit;
      end;
    R := FillBuf;
  Until False;
end;

function TBufferedReader.EOF: Boolean;
begin
  if FBufUsed > FBufPos then
    Result := False else
    Result := FReader.EOF;
end;

procedure TBufferedReader.InvalidateBuffer;
begin
  FReader.SetPosition(FPos);
  FBufPos := 0;
  FBufUsed := 0;
end;

// Internal function BufferByte
// Fills buffer with at least one character, otherwise raises an exception
procedure TBufferedReader.BufferByte;
var I : Integer;
begin
  I := FBufUsed;
  if FBufPos < I then
    exit;
  if I >= FBufferSize then
    begin
      FBufPos := 0;
      FBufUsed := 0;
    end;
  FillBuf;
  if FBufPos >= FBufUsed then
    RaiseReadError;
end;

function TBufferedReader.ReadByte: Byte;
var P : PByte;
begin
  BufferByte;
  P := FBuffer;
  Inc(P, FBufPos);
  Result := P^;
  Inc(FBufPos);
  Inc(FPos);
end;

function TBufferedReader.PeekByte: Byte;
var P : PByte;
begin
  BufferByte;
  P := FBuffer;
  Inc(P, FBufPos);
  Result := P^;
end;

function TBufferedReader.PosBuf(const C: CharSet; const LocateNonMatch: Boolean;
    const MaxOffset: Integer): Integer;
var P : PChar;
    L : Integer;
begin
  P := FBuffer;
  L := FBufPos;
  Inc(P, L);
  Result := 0;
  While (L < FBufUsed) and ((MaxOffset < 0) or (Result <= MaxOffset)) do
    if (P^ in C) xor LocateNonMatch then
      exit else
      begin
        Inc(P);
        Inc(L);
        Inc(Result);
      end;
  Result := -1;
end;

function TBufferedReader.Locate(const C: CharSet; const LocateNonMatch: Boolean;
    const MaxOffset: Integer): Integer;
var I, J, M, K : Integer;
    P : Int64;
    R : Boolean;
begin
  P := GetPosition;
  M := MaxOffset;
  J := 0;
  R := False;
  Repeat
    K := FBufUsed - FBufPos;
    if K > 0 then
      begin
        I := PosBuf(C, LocateNonMatch, M);
        if I >= 0 then
          begin
            SetPosition(P);
            Result := J + I;
            exit;
          end;
      end;
    if R then
      begin
        SetPosition(P);
        Result := -1;
        exit;
      end;
    Inc(J, K);
    Inc(FPos, K);
    FBufPos := 0;
    FBufUsed := 0;
    if M >= 0 then
      begin
        Dec(M, K);
        if M < 0 then
          begin
            SetPosition(P);
            Result := -1;
            exit;
          end;
      end;
    R := FillBuf;
  Until False;
end;



{                                                                              }
{ TSplitBufferedReader                                                         }
{                                                                              }
constructor TSplitBufferedReader.Create(const Reader: AReaderEx; const BufferSize: Integer;
    const ReaderOwner: Boolean);
var I : Integer;
begin
  inherited Create(Reader, ReaderOwner);
  FBufferSize := BufferSize;
  For I := 0 to 1 do
    GetMem(FBuffer[I], BufferSize);
  FPos := Reader.GetPosition;
end;

destructor TSplitBufferedReader.Destroy;
var I : Integer;
begin
  For I := 0 to 1 do
    if Assigned(FBuffer[I]) then
      FreeMem(FBuffer[I]);
  inherited Destroy;
end;

function TSplitBufferedReader.GetSize: Int64;
begin
  Result := FReader.GetSize;
end;

function TSplitBufferedReader.GetPosition: Int64;
begin
  Result := FPos;
end;

// Internal function BufferStart used by SetPosition
// Returns the relative offset of the first buffered byte
function TSplitBufferedReader.BufferStart: Integer;
begin
  Result := -FBufPos;
  if FBufNr = 1 then
    Dec(Result, FBufUsed[0]);
end;

// Internal function BufferRemaining used by SetPosition
// Returns the length of the remaining buffered data
function TSplitBufferedReader.BufferRemaining: Integer;
begin
  Result := FBufUsed[FBufNr] - FBufPos;
  if FBufNr = 0 then
    Inc(Result, FBufUsed[1]);
end;

procedure TSplitBufferedReader.SetPosition(const Position: Int64);
var D : Int64;
begin
  D := Position - FPos;
  if D = 0 then
    exit;
  if (D >= BufferStart) and (D <= BufferRemaining) then
    begin
      Inc(FBufPos, D);
      if (FBufNr = 1) and (FBufPos < 0) then // Set position from Buf1 to Buf0
        begin
          Inc(FBufPos, FBufUsed[0]);
          FBufNr := 0;
        end else
      if (FBufNr = 0) and (FBufPos > FBufUsed[0]) then // Set position from Buf0 to Buf1
        begin
          Dec(FBufPos, FBufUsed[0]);
          FBufNr := 1;
        end;
      FPos := Position;
      exit;
    end;
  FReader.SetPosition(Position);
  FPos := Position;
  FBufNr := 0;
  FBufPos := 0;
  FBufUsed[0] := 0;
  FBufUsed[1] := 0;
end;

procedure TSplitBufferedReader.InvalidateBuffer;
begin
  FReader.SetPosition(FPos);
  FBufNr := 0;
  FBufPos := 0;
  FBufUsed[0] := 0;
  FBufUsed[1] := 0;
end;

// Internal function MoveBuf used by Read
// Moves remaining data from Buffer[BufNr]^[BufPos] to Dest
// Returns True if complete (Remaining=0)
function TSplitBufferedReader.MoveBuf(var Dest: PByte; var Remaining: Integer): Boolean;
var P : PByte;
    L, R, N, O : Integer;
begin
  N := FBufNr;
  O := FBufPos;
  L := FBufUsed[N] - O;
  if L <= 0 then
    begin
      Result := False;
      exit;
    end;
  P := FBuffer[N];
  Inc(P, O);
  R := Remaining;
  if R < L then
    L := R;
  MoveMem(P^, Dest^, L);
  Inc(Dest, L);
  Inc(FBufPos, L);
  Dec(R, L);
  if R <= 0 then
    Result := True else
    Result := False;
  Remaining := R;
end;

// Internal function FillBuf used by Read
// Fill Buffer[BufNr]^[BufPos] with up to BufferSize bytes and moves
// the read data to Dest
// Returns True if complete (incomplete Read or Remaining=0)
function TSplitBufferedReader.FillBuf(var Dest: PByte; var Remaining: Integer): Boolean;
var P : PByte;
    I, L, N : Integer;
begin
  N := FBufNr;
  I := FBufUsed[N];
  L := FBufferSize - I;
  if L <= 0 then
    begin
      Result := False;
      exit;
    end;
  P := FBuffer[N];
  Inc(P, I);
  I := FReader.Read(P^, L);
  if I > 0 then
    begin
      Inc(FBufUsed[N], I);
      if MoveBuf(Dest, Remaining) then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := I < L;
end;

function TSplitBufferedReader.Read(var Buffer; const Size: Integer): Integer;
var Dest : PByte;
    Remaining : Integer;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  Dest := @Buffer;
  Remaining := Size;
  Repeat
    if MoveBuf(Dest, Remaining) then
      begin
        Result := Size;
        Inc(FPos, Size);
        exit;
      end;
    if FillBuf(Dest, Remaining) then
      begin
        Result := Size - Remaining;
        Inc(FPos, Result);
        exit;
      end;
    if FBufNr = 0 then
      FBufNr := 1 else
      begin
        Swap(FBuffer[0], FBuffer[1]);
        FBufUsed[0] := FBufUsed[1];
        FBufUsed[1] := 0;
      end;
    FBufPos := 0;
  Until False;
end;

function TSplitBufferedReader.EOF: Boolean;
begin
  if FBufUsed[FBufNr] > FBufPos then
    Result := False else
    if FBufNr = 1 then
      Result := FReader.EOF else
      if FBufUsed[1] > 0 then
        Result := False else
        Result := FReader.EOF;
end;



{                                                                              }
{ TBufferedFileReader                                                          }
{                                                                              }
constructor TBufferedFileReader.Create(const FileName: String; const BufferSize: Integer);
begin
  inherited Create(TFileReader.Create(FileName), BufferSize, True);
end;

constructor TBufferedFileReader.Create(const FileHandle: Integer;
    const HandleOwner: Boolean; const BufferSize: Integer);
begin
  inherited Create(TFileReader.Create(FileHandle, HandleOwner), BufferSize, True);
end;



{                                                                              }
{ TSplitBufferedFileReader                                                     }
{                                                                              }
constructor TSplitBufferedFileReader.Create(const FileName: String; const BufferSize: Integer);
begin
  inherited Create(TFileReader.Create(FileName), BufferSize, True);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure TestReader(const Reader: AReaderEx; const FreeReader: Boolean);
begin
  try
    Reader.Position := 0;
    Assert(not Reader.EOF,                                 'Reader.EOF');
    Assert(Reader.Size = 26,                               'Reader.Size');
    Assert(Reader.PeekStr(0) = '',                         'Reader.PeekStr');
    Assert(Reader.PeekStr(-1) = '',                        'Reader.PeekStr');
    Assert(Reader.PeekStr(2) = '01',                       'Reader.PeekStr');
    Assert(Char(Reader.PeekByte) = '0',                    'Reader.PeekByte');
    Assert(Char(Reader.ReadByte) = '0',                    'Reader.ReadByte');
    Assert(Char(Reader.PeekByte) = '1',                    'Reader.PeekByte');
    Assert(Char(Reader.ReadByte) = '1',                    'Reader.ReadByte');
    Assert(Reader.ReadStr(0) = '',                         'Reader.ReadStr');
    Assert(Reader.ReadStr(-1) = '',                        'Reader.ReadStr');
    Assert(Reader.ReadStr(1) = '2',                        'Reader.ReadStr');
    Assert(Reader.MatchChar('3'),                          'Reader.MatchChar');
    Assert(Reader.MatchStr('3', True),                     'Reader.MatchStr');
    Assert(Reader.MatchStr('345', True),                   'Reader.MatchStr');
    Assert(not Reader.MatchStr('35', True),                'Reader.MatchStr');
    Assert(not Reader.MatchStr('4', True),                 'Reader.MatchStr');
    Assert(not Reader.MatchStr('', True),                  'Reader.MatchStr');
    Assert(Reader.ReadStr(2) = '34',                       'Reader.ReadStr');
    Assert(Reader.PeekStr(3) = '567',                      'Reader.PeekStr');
    Assert(Reader.Locate('5', False, 0) = 0,               'Reader.Locate');
    Assert(Reader.Locate('8', False, -1) = 3,              'Reader.Locate');
    Assert(Reader.Locate('8', False, 3) = 3,               'Reader.Locate');
    Assert(Reader.Locate('8', False, 2) = -1,              'Reader.Locate');
    Assert(Reader.Locate('8', False, 4) = 3,               'Reader.Locate');
    Assert(Reader.Locate('0', False, -1) = -1,             'Reader.Locate');
    Assert(Reader.Locate(['8'], False, -1) = 3,            'Reader.Locate');
    Assert(Reader.Locate(['8'], False, 3) = 3,             'Reader.Locate');
    Assert(Reader.Locate(['8'], False, 2) = -1,            'Reader.Locate');
    Assert(Reader.Locate(['0'], False, -1) = -1,           'Reader.Locate');
    Assert(Reader.LocateStr('8', -1, True) = 3,            'Reader.LocateStr');
    Assert(Reader.LocateStr('8', 3, True) = 3,             'Reader.LocateStr');
    Assert(Reader.LocateStr('8', 2, True) = -1,            'Reader.LocateStr');
    Assert(Reader.LocateStr('89', -1, True) = 3,           'Reader.LocateStr');
    Assert(Reader.LocateStr('0', -1, True) = -1,           'Reader.LocateStr');
    Assert(not Reader.EOF,                                 'Reader.EOF');
    Assert(Reader.Position = 5,                            'Reader.Position');
    Reader.Position := 7;
    Reader.SkipByte;
    Assert(Reader.Position = 8,                            'Reader.Position');
    Reader.Skip(2);
    Assert(Reader.Position = 10,                           'Reader.Position');
    Assert(not Reader.EOF,                                 'Reader.EOF');
    Assert(Reader.MatchStr('abcd', False),                 'Reader.MatchStr');
    Assert(not Reader.MatchStr('abcd', True),              'Reader.MatchStr');
    Assert(Reader.LocateStr('d', -1, True) = 3,            'Reader.LocateStr');
    Assert(Reader.LocateStr('d', 3, False) = 3,            'Reader.LocateStr');
    Assert(Reader.LocateStr('D', -1, True) = -1,           'Reader.LocateStr');
    Assert(Reader.LocateStr('D', -1, False) = 3,           'Reader.LocateStr');
    Assert(Reader.SkipAll('X', False, -1) = 0,             'Reader.SkipAll');
    Assert(Reader.SkipAll('A', False, -1) = 1,             'Reader.SkipAll');
    Assert(Reader.SkipAll(['b', 'C'], False, -1) = 2,      'Reader.SkipAll');
    Assert(Reader.SkipAll(['d'], False, 0) = 0,            'Reader.SkipAll');
    Assert(Reader.ExtractAll(['d', 'E'], False, 1) = 'd',  'Reader.ExtractAll');
    Assert(Reader.ExtractAll(['*'], True, 1) = 'E',        'Reader.ExtractAll');
    Assert(Reader.ReadStr(2) = '*.',                       'Reader.ReadStr');
    Assert(Reader.ExtractAll(['X'], False, 1) = 'X',       'Reader.ExtractAll');
    Assert(Reader.ExtractAll(['X'], False, -1) = 'XX',     'Reader.ExtractAll');
    Assert(Reader.ExtractAll(['X', '*'], True, 1) = 'Y',   'Reader.ExtractAll');
    Assert(Reader.ExtractAll(['X', '*'], True, -1) = 'YYY','Reader.ExtractAll');
    Assert(Reader.ExtractAll(['X'], False, -1) = '',       'Reader.ExtractAll');
    Assert(Reader.ExtractAll(['X'], True, -1) = '*.',      'Reader.ExtractAll');
    Assert(Reader.EOF,                                     'Reader.EOF');
    Assert(Reader.Position = 26,                           'Reader.Position');
    Reader.Position := Reader.Position - 2;
    Assert(Reader.PeekStr(3) = '*.',                       'Reader.PeekStr');
    Assert(Reader.ReadStr(3) = '*.',                       'Reader.ReadStr');
  finally
    if FreeReader then
      Reader.Free;
  end;
end;

procedure TestLineReader(const Reader: AReaderEx; const FreeReader: Boolean);
begin
  try
    Reader.Position := 0;
    Assert(not Reader.EOF,                    'Reader.EOF');
    Assert(Reader.ExtractLine = '1',          'Reader.ExtractLine');
    Assert(Reader.ExtractLine = '23',         'Reader.ExtractLine');
    Assert(Reader.ExtractLine = '',           'Reader.ExtractLine');
    Assert(Reader.ExtractLine = '4',          'Reader.ExtractLine');
    Assert(Reader.ExtractLine = '5',          'Reader.ExtractLine');
    Assert(Reader.ExtractLine = '6',          'Reader.ExtractLine');
    Assert(Reader.EOF,                        'Reader.EOF');

    Reader.Position := 0;
    Assert(Reader.ExtractLine(-1, [eolCRLF, eolEOF]) = '1', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCRLF, eolEOF]) = '23'#13#13'4'#10'5'#10#13'6', 'Reader.ExtractLine');
    Assert(Reader.EOF, 'Reader.EOF');

    Reader.Position := 0;
    Assert(Reader.ExtractLine(-1, [eolCR, eolEOF]) = '1', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolEOF]) = #10'23', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolEOF]) = '', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolEOF]) = '4'#10'5'#10, 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolEOF]) = '6', 'Reader.ExtractLine');
    Assert(Reader.EOF, 'Reader.EOF');

    Reader.Position := 0;
    Assert(Reader.ExtractLine(-1, [eolCR, eolCRLF, eolEOF]) = '1', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolCRLF, eolEOF]) = '23', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolCRLF, eolEOF]) = '', 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolCRLF, eolEOF]) = '4'#10'5'#10, 'Reader.ExtractLine');
    Assert(Reader.ExtractLine(-1, [eolCR, eolCRLF, eolEOF]) = '6', 'Reader.ExtractLine');
    Assert(Reader.EOF, 'Reader.EOF');

    Reader.Position := 0;
    Assert(Reader.SkipLine(-1, [eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.SkipLine(-1, [eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.EOF, 'Reader.EOF');

    Reader.Position := 0;
    Assert(Reader.SkipLine(-1, [eolCR, eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.SkipLine(-1, [eolCR, eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.SkipLine(-1, [eolCR, eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.SkipLine(-1, [eolCR, eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.SkipLine(-1, [eolCR, eolCRLF, eolEOF]), 'Reader.SkipLine');
    Assert(Reader.EOF, 'Reader.EOF');
  finally
    if FreeReader then
      Reader.Free;
  end;
end;

type
  TUnsizedStringReader = class(TStringReader)
  protected
    function  GetSize: Int64; override;
  end;

function TUnsizedStringReader.GetSize: Int64;
begin
  Result := -1;
end;

procedure TestUnsizedReader(const Data: String);
var S : TUnsizedStringReader;
    T : String;
begin
  S := TUnsizedStringReader.Create(Data);
  try
    T := S.GetToEOF;
    Assert(T = Data,     'UnsizedReader.GetToEOF');
    Assert(S.EOF,        'UnsizedReader.EOF');
  finally
    S.Free;
  end;
end;

procedure SelfTest;
var S : TStringReader;
    I : Integer;
    T : String;
begin
  S := TStringReader.Create('0123456789AbCdE*.XXXYYYY*.');
  try
    TestReader(TReaderProxy.Create(S, False, -1), True);
    TestReader(S, False);
    TestReader(TBufferedReader.Create(S, 128, False), True);
    For I := 1 to 16 do
      TestReader(TBufferedReader.Create(S, I, False), True);
    TestReader(TSplitBufferedReader.Create(S, 128, False), True);
    For I := 1 to 16 do
      TestReader(TSplitBufferedReader.Create(S, I, False), True);
  finally
    S.Free;
  end;

  S := TStringReader.Create('1'#13#10'23'#13#13'4'#10'5'#10#13'6');
  try
    TestLineReader(TReaderProxy.Create(S, False, -1), True);
    For I := 1 to 32 do
      TestLineReader(TBufferedReader.Create(S, I, False), True);
    For I := 1 to 32 do
      TestLineReader(TSplitBufferedReader.Create(S, I, False), True);
    TestLineReader(S, False);
  finally
    S.Free;
  end;

  TestUnsizedReader('');
  TestUnsizedReader('A');
  TestUnsizedReader('ABC');
  T := '';
  For I := 1 to 1000 do
    T := T + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  TestUnsizedReader(T);
end;



end.

