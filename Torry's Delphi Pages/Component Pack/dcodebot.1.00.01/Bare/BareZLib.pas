
(********************************************************)
(*                                                      *)
(*  Bare Object Library @ www.codebot.org/delphi        *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BareZLib;

interface

{$I BARE.INC}

uses
  Windows, ShellAPI, {$IFDEF BARE}BareUtils{$ELSE} SysUtils, Classes{$ENDIF};

const
  foCopy = FO_COPY;
  foDelete = FO_DELETE;
  foMove = FO_MOVE;
  foRename = FO_RENAME;

type
  TOperationOption = (ooAllowUndo, ooConfirmMouse, ooFileOnly, ooMultiDestFile,
    ooNoConfirmation, ooNoConfirmMkDir, ooRenameCollision, ooSilent,
    ooSimpleProgress);
  TOperationOptions = set of TOperationOption;

function FileOperation(const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooNoConfirmation, ooSilent]): Boolean;

function GetTempFileName: string;

procedure CompressFile(const SourceFile, DestFile: string);
procedure DecompressFile(const SourceFile, DestFile: string);
procedure CompressFolder(const Folder, FileName: string);
procedure DecompressFolder(const FileName, Folder: string);

type
  TAlloc = function (AppData: Pointer; Items, Size: Integer): Pointer; register;
  TFree = procedure (AppData, Block: Pointer); register;

  TZStreamRec = packed record
    NextInput: PChar;       // next input byte
    AvailInput: Integer;    // number of bytes available at NextInput
    TotalInput: Integer;    // total nb of input bytes read so far
    NextOutput: PChar;      // next output byte should be put here
    AvailOutput: Integer;   // remaining free space at NextOutput
    TotalOutput: Integer;   // total nb of bytes output so far
    Msg: PChar;             // last error message, NULL if no error
    Internal: Pointer;      // not visible by applications
    ZAlloc: TAlloc;         // used to allocate the Internal state
    ZFree: TFree;           // used to free the Internal state
    AppData: Pointer;       // private data object passed to ZAlloc and ZFree
    data_type: Integer;     //  best guess about the data type: ascii or binary
    Adler: Integer;         // adler32 value of the uncompressed data
    Reserved: Integer;      // Reserved for future use
  end;

{ TCustomZlibStream }

  TCustomZlibStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: Integer;
    FOnProgress: TNotifyEvent;
    FZRec: TZStreamRec;
    FBuffer: array [Word] of Char;
  protected
    constructor Create(Stream: TStream);
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

{ TCompressionStream compresses data on the fly as data is written to it, and
  stores the compressed data to another stream.

  TCompressionStream is write-only and strictly sequential. Reading from the
  stream will raise an exception. Using Seek to move the stream pointer
  will raise an exception.

  Output data is cached Internally, written to the output stream only when
  the Internal output buffer is full.  All pending output data is flushed
  when the stream is destroyed.

  The Position property returns the number of uncompressed bytes of
  data that have been written to the stream so far.

  CompressionRate returns the on-the-fly percentage by which the original
  data has been compressed:  (1 - (CompressedBytes / UncompressedBytes)) * 100
  If raw data size = 100 and compressed data size = 25, the CompressionRate
  is 75%

  The OnProgress event is called each time the output buffer is filled and
  written to the output stream.  This is useful for updating a progress
  indicator when you are writing a large chunk of data to the compression
  stream in a single call. }

  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TCompressionStream = class(TCustomZlibStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(CompressionLevel: TCompressionLevel; Dest: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

{ TDecompressionStream decompresses data on the fly as data is read from it.

  Compressed data comes from a separate source stream.  TDecompressionStream
  is read-only and unidirectional; you can seek forward in the stream, but not
  backwards.  The special case of setting the stream position to zero is
  allowed.  Seeking forward decompresses data until the requested position in
  the uncompressed data has been reached.  Seeking backwards, seeking relative
  to the end of the stream, requesting the size of the stream, and writing to
  the stream will raise an exception.

  The Position property returns the number of bytes of uncompressed data that
  have been read from the stream so far.

  The OnProgress event is called each time the internal input buffer of
  compressed data is exhausted and the next block is read from the input stream.
  This is useful for updating a progress indicator when you are reading a
  large chunk of data from the decompression stream in a single call. }

  TDecompressionStream = class(TCustomZlibStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property OnProgress;
  end;

{ CompressBuf compresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf   }

procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
  out OutBuf: Pointer; out OutBytes: Integer);

{ DecompressBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
       OutEstimate = zero, or est. size of the decompressed data
  Out: OutBuf = ptr to newly allocated buffer containing decompressed data
       OutBytes = number of bytes in OutBuf }

procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
 OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);

{ DecompressToUserBuf decompresses data, buffer to buffer, in one call.
   In: InBuf = ptr to compressed data
       InBytes = number of bytes in InBuf
  Out: OutBuf = ptr to user-allocated buffer to contain decompressed data
       BufSize = number of bytes in OutBuf   }

procedure DecompressToUserBuf(const InBuf: Pointer; InBytes: Integer;
  const OutBuf: Pointer; BufSize: Integer);

const
  ZLib_Version = '1.0.4';

type
  EZlibError = class(Exception);
  ECompressionError = class(EZlibError);
  EDecompressionError = class(EZlibError);

implementation

function FileOperation(const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooNoConfirmation, ooSilent]): Boolean;
const
  Flags: array[TOperationOption] of Integer = (FOF_ALLOWUNDO, FOF_CONFIRMMOUSE,
    FOF_FILESONLY, FOF_MULTIDESTFILES, FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR,
    FOF_RENAMEONCOLLISION, FOF_SILENT, FOF_SIMPLEPROGRESS);
var
  FileOpStruct: TSHFileOpStruct;
  LastMode: Integer;
  I: TOperationOption;
begin
  Result := False;
  LastMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  with FileOpStruct do
  try
    Wnd := 0;
    wFunc := Operation;
    pFrom := PChar(Source);
    pTo := PChar(Dest);
    fFlags := 0;
    for I := Low(TOperationOption) to High(TOperationOption) do
      if I in Options then
        fFlags := fFlags or Flags[I];
    fAnyOperationsAborted := True;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    if SHFileOperation(FileOpStruct) = 0 then
      Result := not fAnyOperationsAborted;
  finally
    SetErrorMode(LastMode);
  end
end;

function GetTempFileName: string;
var
  TempPath: string;
begin
  SetLength(TempPath, MAX_PATH);
  GetTempPath(MAX_PATH, PChar(TempPath));
  SetLength(TempPath, StrLen(PChar(TempPath)));
  if TempPath[Length(TempPath)] <> '\' then
    TempPath := TempPath + '\';
  SetLength(Result, MAX_PATH);
  Windows.GetTempFileName(PChar(TempPath), '~TM', 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  DeleteFile(Result);
end;

const
  faEndOfList = $8000;

function CreateDir(const Dir: string): Boolean;
begin
  Result := CreateDirectory(PChar(Dir), nil);
end;

function RemoveDir(const Dir: string): Boolean;
begin
  Result := RemoveDirectory(PChar(Dir));
end;

procedure CompressFile(const SourceFile, DestFile: string);
var
  SourceStream: TStream;
  DestStream: TStream;
  ZLibStream: TStream;
  Bytes: Integer;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead);
  try
    Bytes := SourceStream.Size;
    DestStream := TFileStream.Create(DestFile, fmCreate);
    try
      ZLibStream := TCompressionStream.Create(clMax, DestStream);
      try
        ZLibStream.Write(Bytes, SizeOf(Bytes));
        ZLibStream.CopyFrom(SourceStream, Bytes);
      finally
        ZLibStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure DecompressFile(const SourceFile, DestFile: string);
var
  SourceStream: TStream;
  ZLibStream: TStream;
  DestStream: TStream;
  Bytes: Integer;
begin
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead);
  try
    ZLibStream := TDecompressionStream.Create(SourceStream);
    try
      DestStream := TFileStream.Create(DestFile, fmCreate);
      try
        ZLibStream.Read(Bytes, SizeOf(Bytes));
        DestStream.CopyFrom(ZLibStream, Bytes);
      finally
        DestStream.Free;
      end;
    finally
      ZLibStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure WriteString(Stream: TStream; const S: string);
var
  I: Integer;
begin
  I := Length(S);
  Stream.Write(I, SizeOf(I));
  if I > 0 then
    Stream.Write(PChar(S)^, Length(S));
end;

function ReadString(Stream: TStream): string;
var
  I: Integer;
begin
  Stream.Read(I, SizeOf(I));
  if I > 0 then
  begin
    SetLength(Result, I);
    if I > 0 then
      Stream.Read(PChar(Result)^, I);
  end
  else
    Result := '';
end;

procedure WriteFile(Stream: TStream; const FileName: string);
var
  FileStream: TStream;
  Size: Integer;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Size := FileStream.Size;
    Stream.Write(Size, SizeOf(Size));
    Stream.CopyFrom(FileStream, Size);
  finally
    FileStream.Free;
  end;
end;

procedure ReadFile(Stream: TStream; const FileName: string);
var
  FileStream: TStream;
  Size: Integer;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Read(Size, SizeOf(Size));
    FileStream.CopyFrom(Stream, Size);
  finally
    FileStream.Free;
  end;
end;

procedure CompressFolder(const Folder, FileName: string);
var
  Stream: TStream;

  procedure Compress(const Directory: string);
  var
    SearchRec: TSearchRec;
    SearchResult: Integer;
    S: string;
    I: Integer;
  begin
    S := Directory;
    if S = '' then Exit;
    if S[Length(S)] <> '\' then
      S := S + '\';
    SearchResult := FindFirst(S + '*.*', faAnyFile, SearchRec);
    while SearchResult = 0 do
    begin
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
      begin
        SearchResult := FindNext(SearchRec);
        Continue;
      end;
      Stream.Write(SearchRec.Attr, SizeOf(SearchRec.Attr));
      WriteString(Stream, SearchRec.Name);
      if SearchRec.Attr and faDirectory = faDirectory then
      begin
        Compress(S + SearchRec.Name);
        I := faEndOfList;
        Stream.Write(I, SizeOf(I));
      end
      else
        WriteFile(Stream, S + SearchRec.Name);
      SearchResult := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  end;

var
  TempFile: string;
begin
  TempFile := GetTempFileName;
  Stream := TFileStream.Create(TempFile, fmCreate);
  try
    Compress(Folder);
  finally
    Stream.Free;
  end;
  CompressFile(TempFile, FileName);
  DeleteFile(TempFile);
end;

procedure DecompressFolder(const FileName, Folder: string);
var
  Stream: TStream;

  procedure Decompress(const Directory: string);
  var
    S: string;
    I: Integer;
  begin
    S := Directory;
    if S = '' then Exit;
    if S[Length(S)] <> '\' then
      S := S + '\';
    CreateDir(S);
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(I, SizeOf(I));
      if I and faEndOfList = faEndOfList then Break;
      if I and faDirectory = faDirectory then
        Decompress(S + ReadString(Stream))
      else
        ReadFile(Stream, S + ReadString(Stream));
    end;
  end;

var
  TempFile: string;
begin
  TempFile := GetTempFileName;
  DecompressFile(FileName, TempFile);
  Stream := TFileStream.Create(TempFile, fmOpenRead);
  try
    Decompress(Folder);
  finally
    Stream.Free;
  end;
  DeleteFile(TempFile);
end;

resourcestring
  STargetBufferTooSmall = 'ZLib error: target buffer may be too small';
  SInvalidStreamOp = 'Invalid stream operation';

const
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

{$L ZLIB\DEFLATE.OBJ}
{$L ZLIB\INFLATE.OBJ}
{$L ZLIB\INFTREES.OBJ}
{$L ZLIB\TREES.OBJ}
{$L ZLIB\ADLER32.OBJ}
{$L ZLIB\INFBLOCK.OBJ}
{$L ZLIB\INFCODES.OBJ}
{$L ZLIB\INFUTIL.OBJ}
{$L ZLIB\INFFAST.OBJ}

procedure _tr_init; external;
procedure _tr_tally; external;
procedure _tr_flush_block; external;
procedure _tr_align; external;
procedure _tr_stored_block; external;
procedure adler32; external;
procedure inflate_blocks_new; external;
procedure inflate_blocks; external;
procedure inflate_blocks_reset; external;
procedure inflate_blocks_free; external;
procedure inflate_set_dictionary; external;
procedure inflate_trees_bits; external;
procedure inflate_trees_dynamic; external;
procedure inflate_trees_fixed; external;
procedure inflate_trees_free; external;
procedure inflate_codes_new; external;
procedure inflate_codes; external;
procedure inflate_codes_free; external;
procedure _inflate_mask; external;
procedure inflate_flush; external;
procedure inflate_fast; external;

procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

// deflate compresses data

function deflateInit_(var Stream: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;
function deflate(var Stream: TZStreamRec; flush: Integer): Integer; external;
function deflateEnd(var Stream: TZStreamRec): Integer; external;

// inflate decompresses data

function inflateInit_(var Stream: TZStreamRec; version: PChar;
  recsize: Integer): Integer; external;
function inflate(var Stream: TZStreamRec; flush: Integer): Integer; external;
function inflateEnd(var Stream: TZStreamRec): Integer; external;
function inflateReset(var Stream: TZStreamRec): Integer; external;

function zlibAllocMem(AppData: Pointer; Items, Size: Integer): Pointer; register;
begin
//  GetMem(Result, Items*Size);
  Result := AllocMem(Items * Size);
end;

procedure zlibFreeMem(AppData, Block: Pointer); register;
begin
  FreeMem(Block);
end;

{function zlibCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EZlibError.Create('error');    //!!
end;}

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create('error'); //!!
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create('error');  //!!
end;

procedure CompressBuf(const InBuf: Pointer; InBytes: Integer;
                      out OutBuf: Pointer; out OutBytes: Integer);
var
  Stream: TZStreamRec;
  P: Pointer;
begin
  FillChar(Stream, SizeOf(Stream), 0);
  Stream.ZAlloc := zlibAllocMem;
  Stream.ZFree := zlibFreeMem;
  OutBytes := ((InBytes + (InBytes div 10) + 12) + 255) and not 255;
  GetMem(OutBuf, OutBytes);
  try
    Stream.NextInput := InBuf;
    Stream.AvailInput := InBytes;
    Stream.NextOutput := OutBuf;
    Stream.AvailOutput := OutBytes;
    CCheck(deflateInit_(Stream, Z_BEST_COMPRESSION, zlib_version, SizeOf(Stream)));
    try
      while CCheck(deflate(Stream, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, 256);
        ReallocMem(OutBuf, OutBytes);
        Stream.NextOutput := PChar(Integer(OutBuf) + (Integer(Stream.NextOutput) - Integer(P)));
        Stream.AvailOutput := 256;
      end;
    finally
      CCheck(deflateEnd(Stream));
    end;
    ReallocMem(OutBuf, Stream.TotalOutput);
    OutBytes := Stream.TotalOutput;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);
var
  Stream: TZStreamRec;
  P: Pointer;
  BufInc: Integer;
begin
  FillChar(Stream, SizeOf(Stream), 0);
  Stream.ZAlloc := zlibAllocMem;
  Stream.ZFree := zlibFreeMem;
  BufInc := (InBytes + 255) and not 255;
  if OutEstimate = 0 then
    OutBytes := BufInc
  else
    OutBytes := OutEstimate;
  GetMem(OutBuf, OutBytes);
  try
    Stream.NextInput := InBuf;
    Stream.AvailInput := InBytes;
    Stream.NextOutput := OutBuf;
    Stream.AvailOutput := OutBytes;
    DCheck(inflateInit_(Stream, zlib_version, SizeOf(Stream)));
    try
      while DCheck(inflate(Stream, Z_FINISH)) <> Z_STREAM_END do
      begin
        P := OutBuf;
        Inc(OutBytes, BufInc);
        ReallocMem(OutBuf, OutBytes);
        Stream.NextOutput := PChar(Integer(OutBuf) + (Integer(Stream.NextOutput) - Integer(P)));
        Stream.AvailOutput := BufInc;
      end;
    finally
      DCheck(inflateEnd(Stream));
    end;
    ReallocMem(OutBuf, Stream.TotalOutput);
    OutBytes := Stream.TotalOutput;
  except
    FreeMem(OutBuf);
    raise
  end;
end;

procedure DecompressToUserBuf(const InBuf: Pointer; InBytes: Integer;
  const OutBuf: Pointer; BufSize: Integer);
var
  Stream: TZStreamRec;
begin
  FillChar(Stream, SizeOf(Stream), 0);
  Stream.ZAlloc := zlibAllocMem;
  Stream.ZFree := zlibFreeMem;
  Stream.NextInput := InBuf;
  Stream.AvailInput := InBytes;
  Stream.NextOutput := OutBuf;
  Stream.AvailOutput := BufSize;
  DCheck(inflateInit_(Stream, zlib_version, SizeOf(Stream)));
  try
    if DCheck(inflate(Stream, Z_FINISH)) <> Z_STREAM_END then
      raise EZlibError.Create(STargetBufferTooSmall);
  finally
    DCheck(inflateEnd(Stream));
  end;
end;

{ TCustomZlibStream }

constructor TCustomZLibStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStreamPos := Stream.Position;
  FZRec.ZAlloc := zlibAllocMem;
  FZRec.ZFree := zlibFreeMem;
end;

procedure TCustomZLibStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;

{ TCompressionStream }

constructor TCompressionStream.Create(CompressionLevel: TCompressionLevel;
  Dest: TStream);
const
  Levels: array [TCompressionLevel] of ShortInt =
    (Z_NO_COMPRESSION, Z_BEST_SPEED, Z_DEFAULT_COMPRESSION, Z_BEST_COMPRESSION);
begin
  inherited Create(Dest);
  FZRec.NextOutput := FBuffer;
  FZRec.AvailOutput := SizeOf(FBuffer);
  CCheck(deflateInit_(FZRec, Levels[CompressionLevel], zlib_version, SizeOf(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.NextInput := nil;
  FZRec.AvailInput := 0;
  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
    while (CCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END)
      and (FZRec.AvailOutput = 0) do
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FZRec.NextOutput := FBuffer;
      FZRec.AvailOutput := SizeOf(FBuffer);
    end;
    if FZRec.AvailOutput < SizeOf(FBuffer) then
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZRec.AvailOutput);
  finally
    deflateEnd(FZRec);
  end;
  inherited Destroy;
end;

function TCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise ECompressionError.Create(sInvalidStreamOp);
end;

function TCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FZRec.NextInput := @Buffer;
  FZRec.AvailInput := Count;
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
  while (FZRec.AvailInput > 0) do
  begin
    CCheck(deflate(FZRec, 0));
    if FZRec.AvailOutput = 0 then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FZRec.NextOutput := FBuffer;
      FZRec.AvailOutput := SizeOf(FBuffer);
      FStreamPos := FStream.Position;
      Progress(Self);
    end;
  end;
  Result := Count;
end;

function TCompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then
    Result := FZRec.TotalInput
  else
    raise ECompressionError.Create(SInvalidStreamOp);
end;

function TCompressionStream.GetCompressionRate: Single;
begin
  if FZRec.TotalInput = 0 then
    Result := 0
  else
    Result := (1.0 - (FZRec.TotalOutput / FZRec.TotalInput)) * 100.0;
end;

{ TDecompressionStream }

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZRec.NextInput := FBuffer;
  FZRec.AvailInput := 0;
  DCheck(inflateInit_(FZRec, zlib_version, SizeOf(FZRec)));
end;

destructor TDecompressionStream.Destroy;
begin
  FStream.Seek(-FZRec.AvailInput, 1);
  inflateEnd(FZRec);
  inherited Destroy;
end;

function TDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  FZRec.NextOutput := @Buffer;
  FZRec.AvailOutput := Count;
  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;
  while (FZRec.AvailOutput > 0) do
  begin
    if FZRec.AvailInput = 0 then
    begin
      FZRec.AvailInput := FStream.Read(FBuffer, SizeOf(FBuffer));
      FZRec.NextInput := FBuffer;
      FStreamPos := FStream.Position;
      Progress(Self);
    end;
    CCheck(inflate(FZRec, 0));
  end;
  Result := Count;
end;

function TDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EDecompressionError.Create(SInvalidStreamOp);
end;

function TDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  I: Integer;
  Buf: array [0..4095] of Char;
begin
  if (Offset = 0) and (Origin = soFromBeginning) then
  begin
    DCheck(inflateReset(FZRec));
    FZRec.NextInput := FBuffer;
    FZRec.AvailInput := 0;
    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ( (Offset >= 0) and (Origin = soFromCurrent)) or
          ( ((Offset - FZRec.TotalOutput) > 0) and (Origin = soFromBeginning)) then
  begin
    if Origin = soFromBeginning then Dec(Offset, FZRec.TotalOutput);
    if Offset > 0 then
    begin
      for I := 1 to Offset div SizeOf(Buf) do
        ReadBuffer(Buf, SizeOf(Buf));
      ReadBuffer(Buf, Offset mod SizeOf(Buf));
    end;
  end
  else
    raise EDecompressionError.Create(SInvalidStreamOp);
  Result := FZRec.TotalOutput;
end;

end.

