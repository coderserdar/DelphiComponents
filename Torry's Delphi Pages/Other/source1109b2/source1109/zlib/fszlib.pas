{*****************************************************************************
*  fszlib.pas (zlib 1.2.3)                                                   *
*                                                                            *
*  copyright (c) 2005 Krzysztof Winnicki modified for FSSQL (fssql.com)      *
*  copyright (c) 2002-2005 Roberto Della Pasqua (www.dellapasqua.com)        *
*  copyright (c) 2000-2002 base2 technologies (www.base2ti.com)              *
*  copyright (c) 1997 Borland International (www.borland.com)                *
*                                                                            *
*  revision history                                                          *
*    2005.02.03  updated with latest zlib 1.2.2, thanks to Fabio Dell'Aria   *
*                (www.eurekalog.com) for provide me the compiled objects     *
*                zlib is compiled without crc32-compressBound                *
*    2003.12.18  updated with latest zlib 1.2.1 (see www.zlib.org)           *
*                obj's compiled with fastest speed optimizations (bcc 5.6.4) *
*                (hint:see basm newsgroup about a Move RTL fast replacement) *
*                Thanks to Cosmin Truta for the pascal zlib reference        *
*                                                                            *
*    2002.11.02  ZSendToBrowser: deflate algorithm for HTTP1.1 compression   *
*    2002.10.24  ZFastCompressString and ZFastDecompressString:300% faster   *
*    2002.10.15  recompiled zlib 1.1.4 c sources with speed optimizations    *
*                (and targeting 686+ cpu) and changes to accomodate Borland  *
*                standards  (C++ v5.6 compiler)                              *
*    2002.10.15  optimized move mem for not aligned structures  (strings,etc)*
*    2002.10.15  little changes to avoid system unique string calls          *
*                                                                            *
*    2002.03.15  updated to zlib version 1.1.4                               *
*    2001.11.27  enhanced TfsZDecompressionStream.Read to adjust source        *
*                  stream position upon end of compression data              *
*                fixed endless loop in TfsZDecompressionStream.Read when       *
*                  destination count was greater than uncompressed data      *
*    2001.10.26  renamed unit to integrate "nicely" with delphi 6            *
*    2000.11.24  added soFromEnd condition to TfsZDecompressionStream.Seek     *
*                added fsZCompressStream and fsZDecompressStream                 *
*    2000.06.13  optimized, fixed, rewrote, and enhanced the zlib.pas unit   *
*                  included on the delphi cd (zlib version 1.1.3)            *
*                                                                            *
*  acknowledgements                                                          *
*    erik turner    Z*Stream routines                                        *
*    david bennion  finding the nastly little endless loop quirk with the    *
*                     TfsZDecompressionStream.Read method                      *
*    burak kalayci  informing me about the zlib 1.1.4 update                 *
*****************************************************************************}

unit fszlib;

interface

uses
  Windows,
  Sysutils,
  Classes;

const
  ZLIB_VERSION = '1.2.3';

type
  TZAlloc = function(opaque: Pointer; items, size: Integer): Pointer;
  TZFree = procedure(opaque, block: Pointer);
  TfsZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  {** fsTZStreamRec ***********************************************************}

  fsTZStreamRec = packed record
    next_in: PChar; // next input byte
    avail_in: Longint; // number of bytes available at next_in
    total_in: Longint; // total nb of input bytes read so far
    next_out: PChar; // next output byte should be put here
    avail_out: Longint; // remaining free space at next_out
    total_out: Longint; // total nb of bytes output so far
    msg: PChar; // last error message, NULL if no error
    state: Pointer; // not visible by applications
    zalloc: TZAlloc; // used to allocate the internal state
    zfree: TZFree; // used to free the internal state
    opaque: Pointer; // private data object passed to zalloc and zfree
    data_type: Integer; // best guess about the data type: ascii or binary
    adler: Longint; // adler32 value of the uncompressed data
    reserved: Longint; // reserved for future use
  end;

  {** fsTCustomZStream ********************************************************}

  fsTCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: Integer;
    FOnProgress: TNotifyEvent;
    FZStream: fsTZStreamRec;
    FBuffer: array[Word] of Char;
  protected
    constructor Create(stream: TStream);
    procedure DoProgress; dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  {** TfsZCompressionStream ***************************************************}

  TfsZCompressionStream = class(fsTCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream; compressionLevel: TfsZCompressionLevel = zcDefault);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;
    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  {** TfsZDecompressionStream *************************************************}

  TfsZDecompressionStream = class(fsTCustomZStream)
  public
    constructor Create(source: TStream);
    destructor Destroy; override;
    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;
    property OnProgress;
  end;

{** zlib public routines ****************************************************}

{*****************************************************************************
*  fsZCompressBuff                                                                 *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer  = pointer to uncompressed data                                *
*    inSize    = size of inBuffer (bytes)                                    *
*    outBuffer = pointer (unallocated)                                       *
*    level     = compression level                                           *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to compressed data (allocated)                      *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure fsZCompressBuff(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TfsZCompressionLevel = zcDefault);

{*****************************************************************************
*  fsZDecompressBuff                                                               *
*                                                                            *
*  pre-conditions                                                            *
*    inBuffer    = pointer to compressed data                                *
*    inSize      = size of inBuffer (bytes)                                  *
*    outBuffer   = pointer (unallocated)                                     *
*    outEstimate = estimated size of uncompressed data (bytes)               *
*                                                                            *
*  post-conditions                                                           *
*    outBuffer = pointer to decompressed data (allocated)                    *
*    outSize   = size of outBuffer (bytes)                                   *
*****************************************************************************}

procedure fsZDecompressBuff(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer = 0);

{** string routines *********************************************************}

function fsZCompressStr(const s: string; level: TfsZCompressionLevel = zcDefault): string;

function fsZDecompressStr(const s: string): string;

{** stream routines *********************************************************}

procedure fsZCompressStream(inStream, outStream: TStream;
  level: TfsZCompressionLevel = zcDefault);

procedure fsZDecompressStream(inStream, outStream: TStream);

{** export routines ********************************************************}

function adler32(adler: LongInt; const buf: PChar; len: Integer): LongInt;
procedure MoveI32(const Source; var Dest; Count: Integer);
procedure ZFastCompressString(var s: string; level: TfsZCompressionLevel);
procedure ZFastDecompressString(var s: string);
procedure ZSendToBrowser(var s: string);
function deflateInit_(var strm: fsTZStreamRec; level: Integer; version: PChar; recsize: Integer): Integer;
function DeflateInit2_(var strm: fsTZStreamRec; level: integer; method: integer; windowBits: integer; memLevel: integer; strategy: integer; version: PChar; recsize: integer): integer;
function deflate(var strm: fsTZStreamRec; flush: Integer): Integer;
function deflateEnd(var strm: fsTZStreamRec): Integer;
function inflateInit_(var strm: fsTZStreamRec; version: PChar; recsize: Integer): Integer;
function inflateInit2_(var strm: fsTZStreamRec; windowBits: integer;  version: PChar; recsize: integer): integer;
function inflate(var strm: fsTZStreamRec; flush: Integer): Integer;
function inflateEnd(var strm: fsTZStreamRec): Integer;
function inflateReset(var strm: fsTZStreamRec): Integer; 

type
  fsEZLibError = class(Exception);
  fsEZCompressionError = class(fsEZLibError);
  fsEZDecompressionError = class(fsEZLibError);

implementation

{** link zlib 1.2.3 **************************************************************}
{** bcc32 flags: -c -6 -O2 -Ve -X- -pr -a8 -b -d -k- -vi -tWM -r -RT- -DFASTEST **}

{$L adler32.obj}
{$L deflate.obj}
{$L infback.obj}
{$L inffast.obj}
{$L inflate.obj}
{$L inftrees.obj}
{$L compress.obj}
{$L crc32.obj}
{$L trees.obj}
{*****************************************************************************
*  note: do not reorder the above -- doing so will result in external        *
*  functions being undefined                                                 *
*****************************************************************************}

const
  {** flush constants *******************************************************}

  Z_NO_FLUSH = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH = 2;
  Z_FULL_FLUSH = 3;
  Z_FINISH = 4;

  {** return codes **********************************************************}

  Z_OK = 0;
  Z_STREAM_END = 1;
  Z_NEED_DICT = 2;
  Z_ERRNO = (-1);
  Z_STREAM_ERROR = (-2);
  Z_DATA_ERROR = (-3);
  Z_MEM_ERROR = (-4);
  Z_BUF_ERROR = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION = 0;
  Z_BEST_SPEED = 1;
  Z_BEST_COMPRESSION = 9;
  Z_DEFAULT_COMPRESSION = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED = 1;
  Z_HUFFMAN_ONLY = 2;
  Z_DEFAULT_STRATEGY = 0;

  {** data types ************************************************************}

  Z_BINARY = 0;
  Z_ASCII = 1;
  Z_UNKNOWN = 2;

  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** return code messages **************************************************}

  _z_errmsg: array[0..9] of PChar = (
    'need dictionary', // Z_NEED_DICT      (2)
    'stream end', // Z_STREAM_END     (1)
    '', // Z_OK             (0)
    'file error', // Z_ERRNO          (-1)
    'stream error', // Z_STREAM_ERROR   (-2)
    'data error', // Z_DATA_ERROR     (-3)
    'insufficient memory', // Z_MEM_ERROR      (-4)
    'buffer error', // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
    );

  ZLevels: array[TfsZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

  SZInvalid = 'Invalid ZStream operation!';

{*********************** Peter Morris not aligned move **********************}

procedure MoveI32(const Source; var Dest; Count: Integer); register;
asm
        cmp   ECX,0
        Je    @JustQuit
        push  ESI
        push  EDI
        mov   ESI, EAX
        mov   EDI, EDX
    @Loop:
	Mov   AL, [ESI]
        Inc   ESI
        mov   [EDI], AL
        Inc   EDI
        Dec   ECX
        Jnz   @Loop
        pop   EDI
        pop   ESI
    @JustQuit:
end;
{****************************************************************************}

{** deflate routines ********************************************************}

function deflateInit_(var strm: fsTZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;

function DeflateInit2_(var strm: fsTZStreamRec; level: integer; method: integer; windowBits: integer;
  memLevel: integer; strategy: integer; version: PChar; recsize: integer): integer; external;

function deflate(var strm: fsTZStreamRec; flush: Integer): Integer;
  external;

function deflateEnd(var strm: fsTZStreamRec): Integer; external;

{** inflate routines ********************************************************}

function inflateInit_(var strm: fsTZStreamRec; version: PChar;
  recsize: Integer): Integer; external;

function inflateInit2_(var strm: fsTZStreamRec; windowBits: integer;
  version: PChar; recsize: integer): integer; external;

function inflate(var strm: fsTZStreamRec; flush: Integer): Integer;
  external;

function inflateEnd(var strm: fsTZStreamRec): Integer; external;

function inflateReset(var strm: fsTZStreamRec): Integer; external;

{** utility routines  *******************************************************}

function adler32; external;
//function crc32; external;
//function compressBound; external;

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result, items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^, count, b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

function _malloc(Size: Integer): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure _free(Block: Pointer); cdecl;
begin
  FreeMem(Block);
end;

{** custom zlib routines ****************************************************}

function DeflateInit(var stream: fsTZStreamRec; level: Integer): Integer;
begin
  result := DeflateInit_(stream, level, ZLIB_VERSION, SizeOf(fsTZStreamRec));
end;

function DeflateInit2(var stream: fsTZStreamRec; level, method, windowBits,
  memLevel, strategy: Integer): Integer;
begin
  result := DeflateInit2_(stream, level, method, windowBits, memLevel, strategy, ZLIB_VERSION, SizeOf(fsTZStreamRec));
end;

function InflateInit(var stream: fsTZStreamRec): Integer;
begin
  result := InflateInit_(stream, ZLIB_VERSION, SizeOf(fsTZStreamRec));
end;

function InflateInit2(var stream: fsTZStreamRec; windowBits: Integer): Integer;
begin
  result := InflateInit2_(stream, windowBits, ZLIB_VERSION, SizeOf(fsTZStreamRec));
end;

{****************************************************************************}

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise fsEZCompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise fsEZDecompressionError.Create(_z_errmsg[2 - code]);
  end;
end;

procedure fsZCompressBuff(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TfsZCompressionLevel);
const
  delta = 256;
var
  zstream: fsTZStreamRec;
begin
  FillChar(zstream, SizeOf(fsTZStreamRec), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZCompress2(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
var
  zstream: fsTZStreamRec;
begin
  FillChar(zstream, SizeOf(fsTZStreamRec), 0);

  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZCompressCheck(DeflateInit2(zstream, 1, 8, -15, 9, 0));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure fsZDecompressBuff(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: fsTZStreamRec;
  delta: Integer;
begin
  FillChar(zstream, SizeOf(fsTZStreamRec), 0);

  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer, outSize);

  try
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;
    zstream.next_out := outBuffer;
    zstream.avail_out := outSize;

    ZDecompressCheck(InflateInit(zstream));

    try
      while ZDecompressCheck(inflate(zstream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize, delta);
        ReallocMem(outBuffer, outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer, zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

{** string routines *********************************************************}

function fsZCompressStr(const s: string; level: TfsZCompressionLevel): string;
var
  buffer: Pointer;
  size: Integer;
begin
  fsZCompressBuff(PChar(s), Length(s), buffer, size, level);
  SetLength(result, size);
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

procedure ZFastCompressString(var s: string; level: TfsZCompressionLevel);
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  fsZCompressBuff(pointer(s), length(s), outBuf, outBytes, level);
  SetLength(s, outBytes);
  MoveI32(pointer(outBuf)^, pointer(s)^, outBytes);
  FreeMem(outBuf);
end;

procedure ZFastDecompressString(var s: string);
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  fsZDecompressBuff(pointer(s), Length(s), outBuf, outBytes);
  SetLength(s, outBytes);
  MoveI32(pointer(outBuf)^, pointer(s)^, outBytes);
  FreeMem(outBuf);
end;

procedure ZSendToBrowser(var s: string);
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  ZCompress2(pointer(s), length(s), outBuf, outBytes);
  SetLength(s, outBytes);
  Move(pointer(outBuf)^, pointer(s)^, outBytes);
  FreeMem(outBuf);
end;

function fsZDecompressStr(const s: string): string;
var
  buffer: Pointer;
  size: Integer;
begin
  fsZDecompressBuff(PChar(s), Length(s), buffer, size);
  SetLength(result, size);
  Move(buffer^, pointer(result)^, size);
  FreeMem(buffer);
end;

{** stream routines *********************************************************}

procedure fsZCompressStream(inStream, outStream: TStream;
  level: TfsZCompressionLevel);
const
  bufferSize = 32768;
var
  zstream: fsTZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of Char;
  outBuffer: array[0..bufferSize - 1] of Char;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(fsTZStreamRec), 0);

  ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(deflate(zstream, Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(deflate(zstream, Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(deflateEnd(zstream));
end;

procedure fsZDecompressStream(inStream, outStream: TStream);
const
  bufferSize = 32768;
var
  zstream: fsTZStreamRec;
  zresult: Integer;
  inBuffer: array[0..bufferSize - 1] of Char;
  outBuffer: array[0..bufferSize - 1] of Char;
  inSize: Integer;
  outSize: Integer;
begin
  FillChar(zstream, SizeOf(fsTZStreamRec), 0);

  ZCompressCheck(InflateInit(zstream));

  inSize := inStream.Read(inBuffer, bufferSize);

  while inSize > 0 do
  begin
    zstream.next_in := inBuffer;
    zstream.avail_in := inSize;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      ZCompressCheck(inflate(zstream, Z_NO_FLUSH));

      // outSize := zstream.next_out - outBuffer;
      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zstream.avail_in = 0) and (zstream.avail_out > 0);

    inSize := inStream.Read(inBuffer, bufferSize);
  end;

  repeat
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := ZCompressCheck(inflate(zstream, Z_FINISH));

    // outSize := zstream.next_out - outBuffer;
    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  until (zresult = Z_STREAM_END) and (zstream.avail_out > 0);

  ZCompressCheck(inflateEnd(zstream));
end;

{** fsTCustomZStream **********************************************************}

constructor fsTCustomZStream.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
  FStreamPos := stream.Position;
end;

procedure fsTCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then FOnProgress(Self);
end;

{** TfsZCompressionStream *****************************************************}

constructor TfsZCompressionStream.Create(dest: TStream;
  compressionLevel: TfsZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := FBuffer;
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(DeflateInit(FZStream, ZLevels[compressionLevel]));
end;

destructor TfsZCompressionStream.Destroy;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

    while ZCompressCheck(deflate(FZStream, Z_FINISH)) <> Z_STREAM_END do
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    deflateEnd(FZStream);
  end;

  inherited Destroy;
end;

function TfsZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise fsEZCompressionError.Create(SZInvalid);
end;

function TfsZCompressionStream.Write(const buffer; count: Longint): Longint;
begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(deflate(FZStream, Z_NO_FLUSH));

    if FZStream.avail_out = 0 then
    begin
      FStream.WriteBuffer(FBuffer, SizeOf(FBuffer));

      FZStream.next_out := FBuffer;
      FZStream.avail_out := SizeOf(FBuffer);

      FStreamPos := FStream.Position;

      DoProgress;
    end;
  end;

  result := Count;
end;

function TfsZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else raise fsEZCompressionError.Create(SZInvalid);
end;

function TfsZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then result := 0
  else result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{** TfsZDecompressionStream ***************************************************}

constructor TfsZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);
  FZStream.next_in := FBuffer;
  FZStream.avail_in := 0;
  ZDecompressCheck(InflateInit(FZStream));
end;

destructor TfsZDecompressionStream.Destroy;
begin
  inflateEnd(FZStream);
  inherited Destroy;
end;

function TfsZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := count;

  if FStream.Position <> FStreamPos then FStream.Position := FStreamPos;

  zresult := Z_OK;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := FStream.Read(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        result := count - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := FBuffer;
      FStreamPos := FStream.Position;

      DoProgress;
    end;

    zresult := ZDecompressCheck(inflate(FZStream, Z_NO_FLUSH));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    FStream.Position := FStream.Position - FZStream.avail_in;
    FStreamPos := FStream.Position;

    FZStream.avail_in := 0;
  end;

  result := count - FZStream.avail_out;
end;

function TfsZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise fsEZDecompressionError.Create(SZInvalid);
end;

function TfsZDecompressionStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  buf: array[0..8191] of Char;
  i: Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(inflateReset(FZStream));

    FZStream.next_in := FBuffer;
    FZStream.avail_in := 0;

    FStream.Position := 0;
    FStreamPos := 0;
  end
  else if ((offset >= 0) and (origin = soFromCurrent)) or
    (((offset - FZStream.total_out) > 0) and (origin = soFromBeginning)) then
  begin
    if origin = soFromBeginning then Dec(offset, FZStream.total_out);

    if offset > 0 then
    begin
      for i := 1 to offset div SizeOf(buf) do ReadBuffer(buf, SizeOf(buf));
      ReadBuffer(buf, offset mod SizeOf(buf));
    end;
  end
  else if (offset = 0) and (origin = soFromEnd) then
  begin
    while Read(buf, SizeOf(buf)) > 0 do ;
  end
  else raise fsEZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

end.

