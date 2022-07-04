{$INCLUDE ..\cDefines.inc}
unit cWriters;

{                                                                              }
{                              Writers v3.03                                   }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cWriters.pas                     }
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
{   12/05/2002  3.01  Created cWriters unit from cStreams.                     }
{                     AWriter, TFileWriter.                                    }
{   29/03/2003  3.02  Added TStringWriter.                                     }
{   21/02/2004  3.03  Added TWideStringWriter.                                 }
{                                                                              }

interface

uses
  { Delphi }
  Windows,
  SysUtils;



{                                                                              }
{ AWriter                                                                      }
{   Writer abstract base class.                                                }
{                                                                              }
type
  AWriter = class
  protected
    function  GetPosition: Int64; virtual; abstract;
    procedure SetPosition(const Position: Int64); virtual; abstract;
    function  GetSize: Int64; virtual; abstract;
    procedure SetSize(const Size: Int64); virtual; abstract;

  public
    function  Write(const Buffer; const Size: Integer): Integer; virtual; abstract;

    property  Position: Int64 read GetPosition write SetPosition;
    property  Size: Int64 read GetSize write SetSize;
  end;
  EWriter = class(Exception);



{                                                                              }
{ AWriterEx                                                                    }
{   Base class for Writer implementations. AWriteEx extends AWriter with       }
{   commonly used functions.                                                   }
{                                                                              }
{   All methods in AWriterEx are implemented using calls to the abstract       }
{   methods in AWriter. Writer implementations can override the virtual        }
{   methods in AWriterEx with more efficient versions.                         }
{                                                                              }
type
  TWriterNewLineType = (nlCR, nlLF, nlCRLF, nlLFCR);
  AWriterEx = class(AWriter)
  protected
    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); overload; override;

    procedure SetAsString(const S: String); virtual;
    procedure SetAsWideString(const S: WideString); virtual;

  public
    procedure RaiseWriteError;

    procedure Append;
    procedure Truncate; virtual;
    procedure Clear; virtual;

    property  AsString: String write SetAsString;
    property  AsWideString: WideString write SetAsWideString;

    procedure WriteBuffer(const Buffer; const Size: Integer);
    procedure WriteStr(const Buffer: String); virtual;
    procedure WriteWideStr(const Buffer: WideString); virtual;

    procedure WriteByte(const V: Byte); virtual;
    procedure WriteWord(const V: Word); virtual;
    procedure WriteLongWord(const V: LongWord);
    procedure WriteLongInt(const V: LongInt);
    procedure WriteInt64(const V: Int64);
    procedure WriteSingle(const V: Single);
    procedure WriteDouble(const V: Double);
    procedure WriteExtended(const V: Extended);
    procedure WritePackedString(const V: String);
    procedure WritePackedStringArray(const V: Array of String);
    procedure WritePackedWideString(const V: WideString);

    procedure WriteBufLine(const Buffer; const Size: Integer;
              const NewLineType: TWriterNewLineType = nlCRLF);
    procedure WriteLine(const S: String;
              const NewLineType: TWriterNewLineType = nlCRLF);
  end;



{                                                                              }
{ TFileWriter                                                                  }
{   Writer implementation for a file.                                          }
{                                                                              }
type
  TFileWriterOpenMode = (fwomOpen,              // Open existing
                         fwomTruncate,          // Open existing and truncate
                         fwomCreate,            // Always create
                         fwomCreateIfNotExist); // Create if not exist else open existing
  TFileWriterAccessHint = (
      fwahNone,
      fwahRandomAccess,
      fwahSequentialAccess);
  TFileWriterOptions = Set of (
      fwoWriteThrough);
  TFileWriter = class(AWriterEx)
  protected
    FFileName    : String;
    FHandle      : Integer;
    FHandleOwner : Boolean;
    FFileCreated : Boolean;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); override;

  public
    constructor Create(const FileName: String;
                const OpenMode: TFileWriterOpenMode = fwomCreateIfNotExist;
                const Options: TFileWriterOptions = [];
                const AccessHint: TFileWriterAccessHint = fwahNone); overload;
    constructor Create(const FileHandle: Integer; const HandleOwner: Boolean); overload;
    destructor Destroy; override;

    property  Handle: Integer read FHandle;
    property  HandleOwner: Boolean read FHandleOwner;
    property  FileCreated: Boolean read FFileCreated;

    function  Write(const Buffer; const Size: Integer): Integer; override;

    procedure Flush;
    procedure DeleteFile;
  end;
  EFileWriter = class(EWriter);

procedure WriteStrToFile(const FileName: String; const S: String;
          const OpenMode: TFileWriterOpenMode = fwomCreate);
procedure AppendStrToFile(const FileName: String; const S: String);

procedure WriteWideStrToFile(const FileName: String; const S: WideString;
          const OpenMode: TFileWriterOpenMode = fwomCreate);
procedure AppendWideStrToFile(const FileName: String; const S: WideString);



{                                                                              }
{ TStringWriter                                                                }
{   Writer implementation for a dynamic string.                                }
{                                                                              }
type
  TStringWriter = class(AWriterEx)
  protected
    FData : String;
    FSize : Integer;
    FPos  : Integer;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); overload; override;
    procedure SetSize(const Size: Integer); overload;
    function  GetAsString: String;
    procedure SetAsString(const S: String); override;

  public
    property  DataString: String read FData;
    property  DataSize: Integer read FSize;
    property  AsString: String read GetAsString write SetAsString;

    function  Write(const Buffer; const Size: Integer): Integer; override;

    procedure WriteStr(const Buffer: String); override;
    procedure WriteByte(const V: Byte); override;
  end;



{                                                                              }
{ TWideStringWriter                                                            }
{   Writer implementation for a wide string.                                   }
{                                                                              }
type
  TWideStringWriter = class(AWriterEx)
  protected
    FData : WideString;
    FSize : Integer;
    FPos  : Integer;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); overload; override;
    procedure SetSize(const Size: Integer); overload;
    function  GetAsWideString: WideString;
    procedure SetAsWideString(const S: WideString); override;

  public
    property  DataString: WideString read FData;
    property  DataSize: Integer read FSize;
    property  AsWideString: WideString read GetAsWideString write SetAsWideString;

    function  Write(const Buffer; const Size: Integer): Integer; override;
    procedure WriteStr(const Buffer: String); override;
    procedure WriteWideStr(const Buffer: WideString); override;
    procedure WriteByte(const V: Byte); override;
    procedure WriteWord(const V: Word); override;
  end;



{                                                                              }
{ TOutputWriter                                                                }
{   Writer implementation for standard system output.                          }
{                                                                              }
type
  TOutputWriter = class(AWriterEx)
  public
    function  Write(const Buffer; const Size: Integer): Integer; override;
  end;



implementation



{                                                                              }
{ AWriterEx                                                                    }
{                                                                              }
procedure AWriterEx.RaiseWriteError;
begin
  raise EWriter.Create('Write error');
end;

function AWriterEx.GetPosition: Int64;
begin
  raise EWriter.Create('Abstract error: GetPosition');
end;

procedure AWriterEx.SetPosition(const Position: Int64);
begin
  raise EWriter.Create('Abstract error: SetPosition');
end;

function AWriterEx.GetSize: Int64;
begin
  raise EWriter.Create('Abstract error: GetSize');
end;

procedure AWriterEx.SetSize(const Size: Int64);
begin
  raise EWriter.Create('Abstract error: SetSize');
end;

procedure AWriterEx.Append;
begin
  Position := Size;
end;

procedure AWriterEx.Truncate;
begin
  Size := Position;
end;

procedure AWriterEx.Clear;
begin
  Size := 0;
end;

procedure AWriterEx.WriteBuffer(const Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if Write(Buffer, Size) <> Size then
    RaiseWriteError;
end;

procedure AWriterEx.WriteStr(const Buffer: String);
begin
  WriteBuffer(Pointer(Buffer)^, Length(Buffer));
end;

procedure AWriterEx.WriteWideStr(const Buffer: WideString);
begin
  WriteBuffer(Pointer(Buffer)^, Length(Buffer) * Sizeof(WideChar));
end;

procedure AWriterEx.SetAsString(const S: String);
begin
  Position := 0;
  WriteStr(S);
  Truncate;
end;

procedure AWriterEx.SetAsWideString(const S: WideString);
begin
  Position := 0;
  WriteWideStr(S);
  Truncate;
end;

procedure AWriterEx.WriteByte(const V: Byte);
begin
  WriteBuffer(V, Sizeof(Byte));
end;

procedure AWriterEx.WriteWord(const V: Word);
begin
  WriteBuffer(V, Sizeof(Word));
end;

procedure AWriterEx.WriteLongWord(const V: LongWord);
begin
  WriteBuffer(V, Sizeof(LongWord));
end;

procedure AWriterEx.WriteLongInt(const V: LongInt);
begin
  WriteBuffer(V, Sizeof(LongInt));
end;

procedure AWriterEx.WriteInt64(const V: Int64);
begin
  WriteBuffer(V, Sizeof(Int64));
end;

procedure AWriterEx.WriteSingle(const V: Single);
begin
  WriteBuffer(V, Sizeof(Single));
end;

procedure AWriterEx.WriteDouble(const V: Double);
begin
  WriteBuffer(V, Sizeof(Double));
end;

procedure AWriterEx.WriteExtended(const V: Extended);
begin
  WriteBuffer(V, Sizeof(Extended));
end;

procedure AWriterEx.WritePackedString(const V: String);
begin
  WriteLongInt(Length(V));
  WriteStr(V);
end;

procedure AWriterEx.WritePackedStringArray(const V: Array of String);
var I, L : Integer;
begin
  L := Length(V);
  WriteLongInt(L);
  For I := 0 to L - 1 do
    WritePackedString(V[I]);
end;

procedure AWriterEx.WritePackedWideString(const V: WideString);
begin
  WriteLongInt(Length(V));
  WriteWideStr(V);
end;

procedure AWriterEx.WriteBufLine(const Buffer; const Size: Integer;
    const NewLineType: TWriterNewLineType);
begin
  WriteBuffer(Buffer, Size);
  Case NewLineType of
    nlCR   : WriteByte(13);
    nlLF   : WriteByte(10);
    nlCRLF : WriteStr(#13#10);
    nlLFCR : WriteStr(#10#13);
  end;
end;

procedure AWriterEx.WriteLine(const S: String; const NewLineType: TWriterNewLineType);
begin
  WriteBufLine(Pointer(S)^, Length(S), NewLineType);
end;



{                                                                              }
{ TFileWriter                                                                  }
{                                                                              }

{$IFNDEF DELPHI6_UP}
procedure RaiseLastOSError;
begin
  {$IFDEF FREEPASCAL}
  raise Exception.Create('OS Error');
  {$ELSE}
  RaiseLastWin32Error;
  {$ENDIF}
end;
{$ENDIF}

constructor TFileWriter.Create(const FileName: String;
    const OpenMode: TFileWriterOpenMode; const Options: TFileWriterOptions;
    const AccessHint: TFileWriterAccessHint);
var CreateFile : Boolean;
    {$IFDEF OS_WIN32}
    F : LongWord;
    {$ENDIF}
begin
  inherited Create;
  FFileName := FileName;
  Case OpenMode of
    fwomCreate           : CreateFile := True;
    fwomCreateIfNotExist : CreateFile := not FileExists(FileName);
  else
    CreateFile := False;
  end;
  {$IFDEF OS_WIN32}
  F := FILE_ATTRIBUTE_NORMAL;
  Case AccessHint of
    fwahNone             : ;
    fwahRandomAccess     : F := F or FILE_FLAG_RANDOM_ACCESS;
    fwahSequentialAccess : F := F or FILE_FLAG_SEQUENTIAL_SCAN;
  end;
  if fwoWriteThrough in Options then
    F := F or FILE_FLAG_WRITE_THROUGH;
  if CreateFile then
    FHandle := Integer(Windows.CreateFile(PChar(FileName),
        GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, F, 0))
  else
    FHandle := Integer(Windows.CreateFile(PChar(FileName),
        GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, F, 0));
  {$ELSE}
  if CreateFile then
    FHandle := FileCreate(FileName)
  else
    FHandle := FileOpen(FileName, fmOpenReadWrite);
  {$ENDIF}
  if FHandle = -1 then
    RaiseLastOSError;
  FHandleOwner := True;
  FFileCreated := CreateFile;
  if OpenMode = fwomTruncate then
    if not SetEndOfFile(FHandle) then
      raise EFileWriter.Create('File truncate error');
end;

constructor TFileWriter.Create(const FileHandle: Integer; const HandleOwner: Boolean);
begin
  inherited Create;
  FHandle := FileHandle;
  FHandleOwner := HandleOwner;
end;

destructor TFileWriter.Destroy;
begin
  if FHandleOwner and (FHandle <> -1) and (FHandle <> 0) then
    FileClose(FHandle);
  inherited Destroy;
end;

function TFileWriter.GetPosition: Int64;
begin
  Result := FileSeek(FHandle, Int64(0), 1);
  if Result = -1 then
    raise EFileWriter.Create('File error');
end;

procedure TFileWriter.SetPosition(const Position: Int64);
begin
  if FileSeek(FHandle, Position, 0) = -1 then
    raise EFileWriter.Create('File seek error');
end;

function TFileWriter.GetSize: Int64;
var I : Int64;
begin
  I := GetPosition;
  Result := FileSeek(FHandle, Int64(0), 2);
  SetPosition(I);
  if Result = -1 then
    raise EFileWriter.Create('File error');
end;

procedure TFileWriter.SetSize(const Size: Int64);
begin
  SetPosition(Size);
  if not SetEndOfFile(FHandle) then
    raise EFileWriter.Create('File resize error');
end;

function TFileWriter.Write(const Buffer; const Size: Integer): Integer;
var I : Integer;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  I := FileWrite(FHandle, Buffer, Size);
  if I < 0 then
    RaiseLastOSError;
  Result := I;
end;

procedure TFileWriter.Flush;
begin
  if not FlushFileBuffers(FHandle) then
    RaiseLastOSError;
end;

procedure TFileWriter.DeleteFile;
begin
  if FFileName = '' then
    raise EFileWriter.Create('No filename');
  if (FHandle <> -1) and (FHandle <> 0) then
    FileClose(FHandle);
  FHandle := -1;
  SysUtils.DeleteFile(FFileName);
end;

procedure WriteStrToFile(const FileName: String; const S: String;
    const OpenMode: TFileWriterOpenMode);
var F : TFileWriter;
begin
  F := TFileWriter.Create(FileName, OpenMode);
  try
    F.SetAsString(S);
  finally
    F.Free;
  end;
end;

procedure AppendStrToFile(const FileName: String; const S: String);
var F : TFileWriter;
begin
  F := TFileWriter.Create(FileName, fwomCreateIfNotExist);
  try
    F.Append;
    F.WriteStr(S);
  finally
    F.Free;
  end;
end;

procedure WriteWideStrToFile(const FileName: String; const S: WideString;
    const OpenMode: TFileWriterOpenMode);
var F : TFileWriter;
begin
  F := TFileWriter.Create(FileName, OpenMode);
  try
    F.SetAsWideString(S);
  finally
    F.Free;
  end;
end;

procedure AppendWideStrToFile(const FileName: String; const S: WideString);
var F : TFileWriter;
begin
  F := TFileWriter.Create(FileName, fwomCreateIfNotExist);
  try
    F.Append;
    F.WriteWideStr(S);
  finally
    F.Free;
  end;
end;



{                                                                              }
{ TStringWriter                                                                }
{                                                                              }
function TStringWriter.GetPosition: Int64;
begin
  Result := FPos;
end;

procedure TStringWriter.SetPosition(const Position: Int64);
begin
  if (Position < 0) or (Position > High(Integer)) then
    raise EFileWriter.Create('Invalid position');
  FPos := Integer(Position);
end;

function TStringWriter.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TStringWriter.SetSize(const Size: Integer);
var L : Integer;
begin
  if Size = FSize then
    exit;
  L := Length(FData);
  if Size > L then
    begin
      // memory allocation strategy
      if L = 0 then        // first allocation is exactly as request
        L := Size else
      if Size < 16 then    // if grow to < 16 then allocate 16
        L := 16 else
        L := Size + (Size shr 2); // if grow to > 16 then pre-allocate 1/4
      SetLength(FData, L);
    end;
  FSize := Size;
end;

procedure TStringWriter.SetSize(const Size: Int64);
begin
  if Size > High(Integer) then
    raise EFileWriter.Create('Invalid size');
  SetSize(Integer(Size));
end;

function TStringWriter.GetAsString: String;
var L : Integer;
begin
  L := Length(FData);
  if L = FSize then
    Result := FData else
    Result := Copy(FData, 1, FSize);
end;

procedure TStringWriter.SetAsString(const S: String);
begin
  FData := S;
  FSize := Length(S);
  FPos := FSize;
end;

function TStringWriter.Write(const Buffer; const Size: Integer): Integer;
var I, J : Integer;
    P    : PChar;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  I := FPos;
  J := I + Size;
  if J > FSize then
    SetSize(J);
  P := Pointer(FData);
  Inc(P, I);
  Move(Buffer, P^, Size);
  Result := Size;
  FPos := J;
end;

procedure TStringWriter.WriteStr(const Buffer: String);
begin
  Write(Pointer(Buffer)^, Length(Buffer));
end;

procedure TStringWriter.WriteByte(const V: Byte);
var I, J : Integer;
    P    : PChar;
begin
  I := FPos;
  J := I + 1;
  if J > FSize then
    SetSize(J);
  P := Pointer(FData);
  Inc(P, I);
  PByte(P)^ := V;
  FPos := J;
end;



{                                                                              }
{ TWideStringWriter                                                            }
{                                                                              }
function TWideStringWriter.GetPosition: Int64;
begin
  Result := FPos;
end;

procedure TWideStringWriter.SetPosition(const Position: Int64);
begin
  if (Position < 0) or (Position > High(Integer)) then
    raise EFileWriter.Create('Invalid position');
  FPos := Integer(Position);
end;

function TWideStringWriter.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TWideStringWriter.SetSize(const Size: Integer);
var L : Integer;
begin
  if Size = FSize then
    exit;
  L := Length(FData) * Sizeof(WideChar);
  if Size > L then
    begin
      // memory allocation strategy
      if L = 0 then        // first allocation is exactly as request
        L := Size else
      if Size < 16 then    // if grow to < 16 then allocate 16
        L := 16 else
        L := Size + (Size shr 2); // if grow to > 16 then pre-allocate 1/4
      SetLength(FData, (L + 1) div Sizeof(WideChar));
    end;
  FSize := Size;
end;

procedure TWideStringWriter.SetSize(const Size: Int64);
begin
  if Size > High(Integer) then
    raise EFileWriter.Create('Invalid size');
  SetSize(Integer(Size));
end;

function TWideStringWriter.GetAsWideString: WideString;
var L : Integer;
begin
  L := Length(FData) * Sizeof(WideChar);
  if L = FSize then
    Result := FData else
    Result := Copy(FData, 1, FSize div Sizeof(WideChar));
end;

procedure TWideStringWriter.SetAsWideString(const S: WideString);
begin
  FData := S;
  FSize := Length(S) * Sizeof(WideChar);
  FPos := FSize;
end;

function TWideStringWriter.Write(const Buffer; const Size: Integer): Integer;
var I, J : Integer;
    P    : PChar;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  I := FPos;
  J := I + Size;
  if J > FSize then
    SetSize(J);
  P := Pointer(FData);
  Inc(P, I);
  Move(Buffer, P^, Size);
  Result := Size;
  FPos := J;
end;

procedure TWideStringWriter.WriteStr(const Buffer: String);
begin
  Write(Pointer(Buffer)^, Length(Buffer));
end;

procedure TWideStringWriter.WriteWideStr(const Buffer: WideString);
begin
  Write(Pointer(Buffer)^, Length(Buffer) * Sizeof(WideChar));
end;

procedure TWideStringWriter.WriteByte(const V: Byte);
var I, J : Integer;
    P    : PChar;
begin
  I := FPos;
  J := I + 1;
  if J > FSize then
    SetSize(J);
  P := Pointer(FData);
  Inc(P, I);
  PByte(P)^ := V;
  FPos := J;
end;

procedure TWideStringWriter.WriteWord(const V: Word);
var I, J : Integer;
    P    : PChar;
begin
  I := FPos;
  J := I + 2;
  if J > FSize then
    SetSize(J);
  P := Pointer(FData);
  Inc(P, I);
  PWord(P)^ := V;
  FPos := J;
end;



{                                                                              }
{ TOutputWriter                                                                }
{                                                                              }
function TOutputWriter.Write(const Buffer; const Size: Integer): Integer;
var I : Integer;
    P : PByte;
begin
  if Size <= 0 then
    begin
      Result := 0;
      exit;
    end;
  P := @Buffer;
  For I := 1 to Size do
    begin
      System.Write(Char(P^));
      Inc(P);
    end;
  Result := Size;
end;



end.

