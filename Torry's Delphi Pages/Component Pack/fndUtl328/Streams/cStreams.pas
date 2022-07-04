{$INCLUDE ..\cDefines.inc}
unit cStreams;

{                                                                              }
{                               Streams v3.08                                  }
{                                                                              }
{             This unit is copyright © 1999-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cStreams.pas                     }
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
{   08/02/2000  1.02  AStreamEx.                                               }
{   08/05/2000  1.03  ATRecordStream.                                          }
{   01/06/2000  1.04  TFixedLenRecordStreamer.                                 }
{   29/05/2002  3.05  Created cReaders and cWriters units from cStreams.       }
{   03/08/2002  3.06  Moved TVarSizeAllocator to unit cVarAllocator.           }
{   18/08/2002  3.07  Added TReaderWriter as AStream.                          }
{   17/03/2003  3.08  Added new file open mode: fsomCreateOnWrite              }
{                                                                              }

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cReaders,
  cWriters;



{                                                                              }
{ AStream                                                                      }
{   Abstract base class for streams.                                           }
{                                                                              }
type
  AStream = class;
  AStreamCopyProgressEvent = procedure (const Source, Destination: AStream;
      const BytesCopied: Int64; var Abort: Boolean) of object;
  AStream = class
  protected
    FOnCopyProgress : AStreamCopyProgressEvent;

    function  GetPosition: Int64; virtual; abstract;
    procedure SetPosition(const Position: Int64); virtual; abstract;
    function  GetSize: Int64; virtual; abstract;
    procedure SetSize(const Size: Int64); virtual; abstract;
    function  GetReader: AReaderEx; virtual; abstract;
    function  GetWriter: AWriterEx; virtual; abstract;

    procedure TriggerCopyProgressEvent(const Source, Destination: AStream;
              const BytesCopied: Int64; var Abort: Boolean); virtual;

  public
    function  Read(var Buffer; const Size: Integer): Integer; virtual; abstract;
    function  Write(const Buffer; const Size: Integer): Integer; virtual; abstract;

    property  Position: Int64 read GetPosition write SetPosition;
    property  Size: Int64 read GetSize write SetSize;
    function  EOF: Boolean; virtual;
    procedure Truncate; virtual;

    property  Reader: AReaderEx read GetReader;
    property  Writer: AWriterEx read GetWriter;

    procedure ReadBuffer(var Buffer; const Size: Integer);
    function  ReadByte: Byte;
    function  ReadStr(const Size: Integer): String;
    procedure WriteBuffer(const Buffer; const Size: Integer);
    procedure WriteStr(const S: String);

    procedure Assign(const Source: TObject); virtual;
    function  WriteTo(const Destination: AStream; const BlockSize: Integer = 0;
              const Count: Int64 = -1): Int64;

    property  OnCopyProgress: AStreamCopyProgressEvent read FOnCopyProgress write FOnCopyProgress;
  end;
  EStream = class(Exception);
  EStreamOperationAborted = class(EStream)
    constructor Create;
  end;



{                                                                              }
{ Stream proxies                                                               }
{                                                                              }
type
  { TStreamReaderProxy                                                         }
  TStreamReaderProxy = class(AReaderEx)
  protected
    FStream : AStream;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;

  public
    constructor Create(const Stream: AStream);
    property  Stream: AStream read FStream;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;
  end;

  { TStreamWriterProxy                                                         }
  TStreamWriterProxy = class (AWriterEx)
  protected
    FStream : AStream;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); override;

  public
    constructor Create(const Stream: AStream);
    property  Stream: AStream read FStream;

    function  Write(const Buffer; const Size: Integer): Integer; override;
  end;



{                                                                              }
{ Stream functions                                                             }
{                                                                              }
type
  TCopyProgressProcedure = procedure (const Source, Destination: AStream;
      const BytesCopied: Int64; var Abort: Boolean);
  TCopyDataEvent = procedure (const Offset: Int64; const Data: Pointer;
      const DataSize: Integer) of object;

function  CopyStream(const Source, Destination: AStream;
          const SourceOffset: Int64 = 0; const DestinationOffset: Int64 = 0;
          const BlockSize: Integer = 0; const Count: Int64 = -1;
          const ProgressCallback: TCopyProgressProcedure = nil;
          const CopyFromBack: Boolean = False): Int64; overload;

function  CopyStream(const Source: AReaderEx; const Destination: AWriterEx;
          const BlockSize: Integer = 0;
          const CopyDataEvent: TCopyDataEvent = nil): Int64; overload;

procedure DeleteStreamRange(const Stream: AStream; const Position, Count: Int64;
          const ProgressCallback: TCopyProgressProcedure = nil);

procedure InsertStreamRange(const Stream: AStream; const Position, Count: Int64;
          const ProgressCallback: TCopyProgressProcedure = nil);



{                                                                              }
{ TReaderWriter                                                                }
{   Composition of a Reader and a Writer as a Stream.                          }
{                                                                              }
type
  TReaderWriter = class(AStream)
  protected
    FReader      : AReaderEx;
    FWriter      : AWriterEx;
    FReaderOwner : Boolean;
    FWriterOwner : Boolean;

    procedure RaiseNoReaderError;
    procedure RaiseNoWriterError;

    function  GetPosition: Int64; override;
    procedure SetPosition(const Position: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize(const Size: Int64); override;
    function  GetReader: AReaderEx; override;
    function  GetWriter: AWriterEx; override;

  public
    constructor Create(const Reader: AReaderEx; const Writer: AWriterEx;
                const ReaderOwner: Boolean = True; const WriterOwner: Boolean = True);
    destructor Destroy; override;

    property  Reader: AReaderEx read FReader;
    property  Writer: AWriterEx read FWriter;
    property  ReaderOwner: Boolean read FReaderOwner write FReaderOwner;
    property  WriterOwner: Boolean read FWriterOwner write FWriterOwner;

    function  Read(var Buffer; const Size: Integer): Integer; override;
    function  Write(const Buffer; const Size: Integer): Integer; override;
    function  EOF: Boolean; override;
    procedure Truncate; override;
  end;
  EReaderWriter = class (Exception);



{                                                                              }
{ TFileStream                                                                  }
{   Stream implementation for a file.                                          }
{                                                                              }
type
  TFileStreamOpenMode = (
      fsomRead,
      fsomReadWrite,
      fsomCreate,
      fsomCreateIfNotExist,
      fsomCreateOnWrite);
  TFileStreamAccessHint = (
      fsahNone,
      fsahRandomAccess,
      fsahSequentialAccess);
  TFileStreamOptions = Set of (
      fsoWriteThrough);
  TFileStream = class(TReaderWriter)
  protected
    FFileName   : String;
    FOpenMode   : TFileStreamOpenMode;
    FOptions    : TFileStreamOptions;
    FAccessHint : TFileStreamAccessHint;

    procedure SetPosition(const Position: Int64); override;
    procedure SetSize(const Size: Int64); override;
    function  GetReader: AReaderEx; override;
    function  GetWriter: AWriterEx; override;

    function  GetFileHandle: Integer;
    function  GetFileCreated: Boolean;
    procedure EnsureCreateOnWrite;

  public
    constructor Create(const FileName: String;
                const OpenMode: TFileStreamOpenMode;
                const Options: TFileStreamOptions = [];
                const AccessHint: TFileStreamAccessHint = fsahNone); overload;
    constructor Create(const FileHandle: Integer; const HandleOwner: Boolean); overload;

    property  FileName: String read FFileName;
    property  FileHandle: Integer read GetFileHandle;
    property  FileCreated: Boolean read GetFileCreated;
    procedure DeleteFile;

    function  Write(const Buffer; const Size: Integer): Integer; override;
  end;
  EFileStream = class(EStream);



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation



{                                                                              }
{ EStreamOperationAborted                                                      }
{                                                                              }
constructor EStreamOperationAborted.Create;
begin
  inherited Create('Stream operation aborted');
end;



{                                                                              }
{ TStreamReaderProxy                                                           }
{                                                                              }
constructor TStreamReaderProxy.Create(const Stream: AStream);
begin
  inherited Create;
  Assert(Assigned(Stream));
  FStream := Stream;
end;

function TStreamReaderProxy.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

procedure TStreamReaderProxy.SetPosition(const Position: Int64);
begin
  FStream.Position := Position;
end;

function TStreamReaderProxy.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TStreamReaderProxy.Read(var Buffer; const Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size)
end;

function TStreamReaderProxy.EOF: Boolean;
begin
  Result := FStream.EOF;
end;



{                                                                              }
{ TStreamWriterProxy                                                           }
{                                                                              }
constructor TStreamWriterProxy.Create(const Stream: AStream);
begin
  inherited Create;
  Assert(Assigned(Stream));
  FStream := Stream;
end;

function TStreamWriterProxy.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

procedure TStreamWriterProxy.SetPosition(const Position: Int64);
begin
  FStream.Position := Position;
end;

function TStreamWriterProxy.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TStreamWriterProxy.SetSize(const Size: Int64);
begin
  FStream.Size := Size;
end;

function TStreamWriterProxy.Write(const Buffer; const Size: Integer): Integer;
begin
  Result := FStream.Write(Buffer, Size)
end;



{                                                                              }
{ CopyStream                                                                   }
{                                                                              }
const
  DefaultBlockSize = 2048;

function CopyStream(const Source, Destination: AStream; const SourceOffset: Int64;
    const DestinationOffset: Int64; const BlockSize: Integer; const Count: Int64;
    const ProgressCallback: TCopyProgressProcedure; const CopyFromBack: Boolean): Int64;
var Buf     : Pointer;
    L, I, C : Integer;
    R, S, D : Int64;
    A       : Boolean;
begin
  if not Assigned(Source) then
    raise EStream.Create('Invalid source');
  if not Assigned(Destination) then
    raise EStream.Create('Invalid destination');
  S := SourceOffset;
  D := DestinationOffset;
  if (S < 0) or (D < 0) then
    raise EStream.Create('Invalid offset');
  if (Source = Destination) and (Count < 0) and (S < D) then
    raise EStream.Create('Invalid parameters');
  A := False;
  if Assigned(ProgressCallback) then
    begin
      ProgressCallback(Source, Destination, 0, A);
      if A then
        raise EStreamOperationAborted.Create;
    end;
  Result := 0;
  R := Count;
  if R = 0 then
    exit;
  L := BlockSize;
  if L <= 0 then
    L := DefaultBlockSize;
  if (R > 0) and (R < L) then
    L := Integer(R);
  if CopyFromBack then
    begin
      if R < 0 then
        raise EStream.Create('Invalid count');
      Inc(S, R - L);
      Inc(D, R - L);
    end;
  GetMem(Buf, L);
  try
    While not Source.EOF and (R <> 0) do
      begin
        C := L;
        if (R > 0) and (R < C) then
          C := Integer(R);
        if CopyFromBack then
          Source.Position := S;
        I := Source.Read(Buf^, C);
        if (I <= 0) and not Source.EOF then
          raise EStream.Create('Stream read error');
        if CopyFromBack then
          Destination.Position := D;
        Destination.WriteBuffer(Buf^, I);
        Inc(Result, I);
        if R > 0 then
          Dec(R, I);
        if CopyFromBack then
          begin
            Dec(S, I);
            Dec(D, I);
          end
        else
          begin
            Inc(S, I);
            Inc(D, I);
          end;
        if Assigned(ProgressCallback) then
          begin
            ProgressCallback(Source, Destination, Result, A);
            if A then
              raise EStreamOperationAborted.Create;
          end;
      end;
  finally
    FreeMem(Buf);
  end;
end;

function CopyStream(const Source: AReaderEx; const Destination: AWriterEx;
    const BlockSize: Integer; const CopyDataEvent: TCopyDataEvent): Int64;
var Buf  : Pointer;
    L, I : Integer;
begin
  if not Assigned(Source) then
    raise EStream.Create('Invalid source');
  if not Assigned(Destination) then
    raise EStream.Create('Invalid destination');
  L := BlockSize;
  if L <= 0 then
    L := DefaultBlockSize;
  Result := 0;
  GetMem(Buf, L);
  try
    While not Source.EOF do
      begin
        I := Source.Read(Buf^, L);
        if (I = 0) and not Source.EOF then
          Source.RaiseReadError;
        Destination.WriteBuffer(Buf^, I);
        if Assigned(CopyDataEvent) then
          CopyDataEvent(Result, Buf, I);
        Inc(Result, I);
      end;
  finally
    FreeMem(Buf);
  end;
end;

procedure DeleteStreamRange(const Stream: AStream; const Position, Count: Int64;
    const ProgressCallback: TCopyProgressProcedure);
begin
  if Count <= 0 then
    exit;
  if CopyStream(Stream, Stream, Position + Count, Position, 0, Count,
      ProgressCallback, False) <> Count then
    raise EStream.Create('Copy error');
end;

procedure InsertStreamRange(const Stream: AStream; const Position, Count: Int64;
    const ProgressCallback: TCopyProgressProcedure);
begin
  if Count <= 0 then
    exit;
  if CopyStream(Stream, Stream, Position, Position + Count, 0, Count,
      ProgressCallback, True) <> Count then
    raise EStream.Create('Copy error');
end;



{                                                                              }
{ AStream                                                                      }
{                                                                              }
function AStream.EOF: Boolean;
begin
  Result := Position >= Size;
end;

procedure AStream.Truncate;
begin
  Size := Position;
end;

procedure AStream.ReadBuffer(var Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if Read(Buffer, Size) <> Size then
    raise EStream.Create('Read error');
end;

function AStream.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;

function AStream.ReadStr(const Size: Integer): String;
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

procedure AStream.WriteBuffer(const Buffer; const Size: Integer);
begin
  if Size <= 0 then
    exit;
  if Write(Buffer, Size) <> Size then
    raise EStream.Create('Write error');
end;

procedure AStream.WriteStr(const S: String);
begin
  WriteBuffer(Pointer(S)^, Length(S));
end;

procedure AStreamCopyCallback(const Source, Destination: AStream;
    const BytesCopied: Int64; var Abort: Boolean);
begin
  Assert(Assigned(Source) and Assigned(Destination) and not Abort, 'Assigned(Source) and Assigned(Destination) and not Abort');
  Source.TriggerCopyProgressEvent(Source, Destination, BytesCopied, Abort);
  if Abort then
    exit;
  Destination.TriggerCopyProgressEvent(Source, Destination, BytesCopied, Abort);
end;

procedure AStream.TriggerCopyProgressEvent(const Source, Destination: AStream;
    const BytesCopied: Int64; var Abort: Boolean);
begin
  if Assigned(FOnCopyProgress) then
    FOnCopyProgress(Source, Destination, BytesCopied, Abort);
end;

procedure AStream.Assign(const Source: TObject);
begin
  if not Assigned(Source) then
    raise EStream.Create('Invalid source');
  if Source is AStream then
    Size := CopyStream(AStream(Source), self, 0, 0, 0, -1, AStreamCopyCallback, False) else
    raise EStream.Create('Assign not defined for source type');
end;

function AStream.WriteTo(const Destination: AStream; const BlockSize: Integer;
    const Count: Int64): Int64;
begin
  Result := CopyStream(self, Destination, Position, Destination.Position,
      BlockSize, Count, AStreamCopyCallback, False);
end;



{                                                                              }
{ TReaderWriter                                                                }
{                                                                              }
constructor TReaderWriter.Create(const Reader: AReaderEx; const Writer: AWriterEx;
    const ReaderOwner: Boolean; const WriterOwner: Boolean);
begin
  inherited Create;
  FReader := Reader;
  FReaderOwner := ReaderOwner;
  FWriter := Writer;
  FWriterOwner := WriterOwner;
end;

destructor TReaderWriter.Destroy;
begin
  if FReaderOwner then
    FReader.Free;
  FReader := nil;
  if FWriterOwner then
    FWriter.Free;
  FWriter := nil;
  inherited Destroy;
end;

procedure TReaderWriter.RaiseNoReaderError;
begin
  raise EReaderWriter.Create('No reader');
end;

procedure TReaderWriter.RaiseNoWriterError;
begin
  raise EReaderWriter.Create('No writer');
end;

function TReaderWriter.GetReader: AReaderEx;
begin
  Result := FReader;
end;

function TReaderWriter.GetWriter: AWriterEx;
begin
  Result := FWriter;
end;

function TReaderWriter.GetPosition: Int64;
begin
  if Assigned(FReader) then
    Result := FReader.Position else
  if Assigned(FWriter) then
    Result := FWriter.Position else
    Result := 0;
end;

procedure TReaderWriter.SetPosition(const Position: Int64);
begin
  if Assigned(FReader) then
    FReader.Position := Position;
  if Assigned(FWriter) then
    FWriter.Position := Position;
end;

function TReaderWriter.GetSize: Int64;
begin
  if Assigned(FWriter) then
    Result := FWriter.Size else
  if Assigned(FReader) then
    Result := FReader.Size else
    Result := 0;
end;

procedure TReaderWriter.SetSize(const Size: Int64);
begin
  if not Assigned(FWriter) then
    RaiseNoWriterError;
  FWriter.Size := Size;
end;

function TReaderWriter.Read(var Buffer; const Size: Integer): Integer;
begin
  if not Assigned(FReader) then
    RaiseNoReaderError;
  Result := FReader.Read(Buffer, Size);
end;

function TReaderWriter.Write(const Buffer; const Size: Integer): Integer;
begin
  if not Assigned(FWriter) then
    RaiseNoWriterError;
  Result := FWriter.Write(Buffer, Size);
end;

function TReaderWriter.EOF: Boolean;
begin
  if Assigned(FReader) then
    Result := FReader.EOF else
  if Assigned(FWriter) then
    Result := FWriter.Position >= FWriter.Size else
    Result := True;
end;

procedure TReaderWriter.Truncate;
begin
  if not Assigned(FWriter) then
    RaiseNoWriterError;
  FWriter.Truncate;
end;



{                                                                              }
{ TFileStream                                                                  }
{                                                                              }
const
  WriterModes: Array[TFileStreamOpenMode] of TFileWriterOpenMode =
      (fwomOpen, fwomOpen, fwomCreate, fwomCreateIfNotExist, fwomOpen);
  ReaderAccessHints: Array[TFileStreamAccessHint] of TFileReaderAccessHint =
      (frahNone, frahRandomAccess, frahSequentialAccess);
  WriterAccessHints: Array[TFileStreamAccessHint] of TFileWriterAccessHint =
      (fwahNone, fwahRandomAccess, fwahSequentialAccess);

constructor TFileStream.Create(const FileName: String;
    const OpenMode: TFileStreamOpenMode; const Options: TFileStreamOptions;
    const AccessHint: TFileStreamAccessHint);
var W : TFileWriter;
    R : AReaderEx;
    T : TFileWriterOptions;
begin
  FFileName := FileName;
  FOpenMode := OpenMode;
  FOptions := Options;
  FAccessHint := AccessHint;
  R := nil;
  W := nil;
  T := [];
  if fsoWriteThrough in Options then
    Include(T, fwoWriteThrough);
  Case OpenMode of
    fsomRead :
      R := TFileReader.Create(FileName, ReaderAccessHints[AccessHint]);
    fsomCreateOnWrite :
      try
        W := TFileWriter.Create(FileName, fwomOpen, T,
            WriterAccessHints[AccessHint]);
      except
        W := nil;
      end;
    else
      W := TFileWriter.Create(FileName, WriterModes[OpenMode], T,
          WriterAccessHints[AccessHint]);
  end;
  if Assigned(W) then
    try
      R := TFileReader.Create(W.Handle, False);
    except
      W.Free;
      raise;
    end;
  inherited Create(R, W, True, True);
end;

constructor TFileStream.Create(const FileHandle: Integer; const HandleOwner: Boolean);
var W : TFileWriter;
    R : TFileReader;
begin
  W := TFileWriter.Create(FileHandle, HandleOwner);
  try
    R := TFileReader.Create(FileHandle, False);
  except
    W.Free;
    raise;
  end;
  inherited Create(R, W, True, True);
end;

function TFileStream.GetFileHandle: Integer;
begin
  Assert(Assigned(FReader), 'Assigned(FReader)');
  Result := TFileReader(FReader).Handle;
end;

function TFileStream.GetFileCreated: Boolean;
begin
  Result := Assigned(FWriter) and TFileWriter(FWriter).FileCreated;
end;

procedure TFileStream.DeleteFile;
begin
  if FFileName = '' then
    raise EFileStream.Create('No filename');
  SysUtils.DeleteFile(FFileName);
end;

procedure TFileStream.EnsureCreateOnWrite;
var T : TFileWriterOptions;
begin
  T := [];
  if fsoWriteThrough in FOptions then
    Include(T, fwoWriteThrough);
  FWriter := TFileWriter.Create(FileName, fwomCreateIfNotExist, T,
      WriterAccessHints[FAccessHint]);
  FReader := TFileReader.Create(TFileWriter(FWriter).Handle, False);
end;

procedure TFileStream.SetPosition(const Position: Int64);
begin
  if (FOpenMode = fsomCreateOnWrite) and not Assigned(FWriter) and
     (Position > 0) then
    EnsureCreateOnWrite;
  if Assigned(FWriter) then
    FWriter.Position := Position else
  if Assigned(FReader) then
    FReader.Position := Position;
end;

procedure TFileStream.SetSize(const Size: Int64);
begin
  if (FOpenMode = fsomCreateOnWrite) and not Assigned(FWriter) then
    EnsureCreateOnWrite;
  inherited SetSize(Size);
end;

function TFileStream.GetReader: AReaderEx;
begin
  if (FOpenMode = fsomCreateOnWrite) and not Assigned(FWriter) then
    EnsureCreateOnWrite;
  Result := FReader;
end;

function TFileStream.GetWriter: AWriterEx;
begin
  if (FOpenMode = fsomCreateOnWrite) and not Assigned(FWriter) then
    EnsureCreateOnWrite;
  Result := FWriter;
end;

function TFileStream.Write(const Buffer; const Size: Integer): Integer;
begin
  if (FOpenMode = fsomCreateOnWrite) and not Assigned(FWriter) then
    EnsureCreateOnWrite;
  Result := inherited Write(Buffer, Size);
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
begin
end;



end.

