unit TBXGraphicsZLIB;

interface

uses
  SysUtils, Classes;

type
  TAlloc = function(AppData: Pointer; Items, Size: Integer): Pointer;
  TFree = procedure(AppData, Block: Pointer);
  TCompressionLevel = (clNone, clFastest, clDefault, clMax);

  TZStreamRec = packed record
    next_in: PChar;       // next input byte
    avail_in: Integer;    // number of bytes available at next_in
    total_in: Integer;    // total nb of input bytes read so far
    next_out: PChar;      // next output byte should be put here
    avail_out: Integer;   // remaining free space at next_out
    total_out: Integer;   // total nb of bytes output so far
    msg: PChar;           // last error message, NULL if no error
    internal: Pointer;    // not visible by applications
    zalloc: TAlloc;       // used to allocate the internal state
    zfree: TFree;         // used to free the internal state
    AppData: Pointer;     // private data object passed to zalloc and zfree
    data_type: Integer;   //  best guess about the data type: ascii or binary
    adler: Integer;       // adler32 value of the uncompressed data
    reserved: Integer;    // reserved for future use
  end;

const
  zlib_version = '1.2.3';
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

  _z_errmsg: array [0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    'ok',                   // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

type
  EZlibError = class(Exception);

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar; recsize: Integer): Integer;
function deflate(var strm: TZStreamRec; flush: Integer): Integer;
function deflateEnd(var strm: TZStreamRec): Integer;
function inflateInit_(var strm: TZStreamRec; version: PChar; recsize: Integer): Integer;
function inflate(var strm: TZStreamRec; flush: Integer): Integer;
function inflateEnd(var strm: TZStreamRec): Integer;
function inflateReset(var strm: TZStreamRec): Integer;

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
procedure zcfree(opaque, block: Pointer);
function CRC32(C: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;

function ZLibCheck(Code: Integer): Integer;

type
  TCustomZlibStream = class
  private
    FStrm: TStream;
    FStrmPos: Integer;
    FZRec: TZStreamRec;
    FBuffer: array [Word] of Char;
  protected
    constructor Create(Strm: TStream);
  end;

  TCompressionStream = class(TCustomZlibStream)
  public
    constructor Create(Dest: TStream);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint;
  end;

  TDecompressionStream = class(TCustomZlibStream)
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint;
  end;


implementation


{$L zlib\deflate.obj}
{$L zlib\inflate.obj}
{$L zlib\inftrees.obj}
{$L zlib\infback.obj}
{$L zlib\inffast.obj}
{$L zlib\trees.obj}
{$L zlib\compress.obj}
{$L zlib\adler32.obj}


function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar; recsize: Integer): Integer; external;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function deflateEnd(var strm: TZStreamRec): Integer; external;
function inflateInit_(var strm: TZStreamRec; version: PChar; recsize: Integer): Integer; external;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function inflateEnd(var strm: TZStreamRec): Integer; external;
function inflateReset(var strm: TZStreamRec): Integer; external;


procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

var
  crc_table: array [0..255] of Cardinal;

type
  PByteArray = ^TByteArray;
  TByteArray = array [0..MaxInt div 2] of Byte;

procedure make_crc_table;
const
  P = $EDB88320;
var
  C: Cardinal;
  N, K : Integer;
begin
  for N := 0 to 255 do
  begin
    C := N;
    for K := 0 to 7 do
      if C and 1 <> 0 then C := P xor (C shr 1) else C := C shr 1;
    crc_table[N] := C;
  end;
end;

function CRC32(C: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;
begin
  if (Len = 0) or (Buffer = nil) then Result := 0
  else
  begin
    C := C xor $FFFFFFFF;
    while Len >= 8 do
    begin
      { unroll the loop }
      C := crc_table[(C xor PByteArray(Buffer)[0]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[1]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[2]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[3]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[4]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[5]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[6]) and $FF] xor (C shr 8);
      C := crc_table[(C xor PByteArray(Buffer)[7]) and $FF] xor (C shr 8);
      Inc(Buffer, 8);
      Dec(Len, 8);
    end;
    while Len > 0 do
    begin
      { calc the rest of the values }
      C := crc_table[(C xor Buffer^) and $FF] xor (C shr 8);
      Inc(Buffer);
      Dec(Len);
    end;
    Result := C xor $FFFFFFFF;
  end;
end;

function ZLibCheck(Code: Integer): Integer;
begin
  if Code < 0 then
    raise EZLibError.Create('Compression error: ' + _z_errmsg[2 - Code]);
  Result := Code;
end;

{ TCustomZlibStream }

constructor TCustomZlibStream.Create(Strm: TStream);
begin
  inherited Create;
  FStrm := Strm;
  FStrmPos := Strm.Position;
end;

{ TCompressionStream }

constructor TCompressionStream.Create(Dest: TStream);
begin
  inherited Create(Dest);
  FZRec.next_out := FBuffer;
  FZRec.avail_out := SizeOf(FBuffer);
  ZLibCheck(deflateInit_(FZRec, Z_BEST_COMPRESSION, zlib_version, sizeof(FZRec)));
end;

destructor TCompressionStream.Destroy;
begin
  FZRec.next_in := nil;
  FZRec.avail_in := 0;
  try
    if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
    while (ZLibCheck(deflate(FZRec, Z_FINISH)) <> Z_STREAM_END) and (FZRec.avail_out = 0) do
    begin
      FStrm.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := SizeOf(FBuffer);
    end;
    if FZRec.avail_out < SizeOf(FBuffer) then
      FStrm.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZRec.avail_out);
  finally
    deflateEnd(FZRec);
  end;
  inherited;
end;

function TCompressionStream.Write(const Buffer; Count: Integer): Longint;
begin
  FZRec.next_in := @Buffer;
  FZRec.avail_in := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while FZRec.avail_in > 0 do
  begin
    ZLibCheck(deflate(FZRec, 0));
    if FZRec.avail_out = 0 then
    begin
      FStrm.WriteBuffer(FBuffer, SizeOf(FBuffer));
      FZRec.next_out := FBuffer;
      FZRec.avail_out := SizeOf(FBuffer);
      FStrmPos := FStrm.Position;
    end;
  end;
  Result := Count;
end;

{ TDecompressionStream }

constructor TDecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FZRec.next_in := FBuffer;
  FZRec.avail_in := 0;
  ZLibCheck(inflateInit_(FZRec, zlib_version, sizeof(FZRec)));
end;

destructor TDecompressionStream.Destroy;
begin
  FStrm.Seek(-FZRec.avail_in, 1);
  inflateEnd(FZRec);
  inherited;
end;

function TDecompressionStream.Read(var Buffer; Count: Integer): Longint;
begin
  FZRec.next_out := @Buffer;
  FZRec.avail_out := Count;
  if FStrm.Position <> FStrmPos then FStrm.Position := FStrmPos;
  while FZRec.avail_out > 0 do
  begin
    if FZRec.avail_in = 0 then
    begin
      FZRec.avail_in := FStrm.Read(FBuffer, SizeOf(FBuffer));
      if FZRec.avail_in = 0 then
      begin
        Result := Count - FZRec.avail_out;
        Exit;
      end;
	    FZRec.next_in := FBuffer;
      FStrmPos := FStrm.Position;
    end;
    ZLibCheck(inflate(FZRec, 0));
  end;
  Result := Count;
end;

initialization
  make_crc_table;
  
end.
