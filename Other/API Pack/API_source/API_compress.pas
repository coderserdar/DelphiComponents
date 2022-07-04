unit API_compress;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.01, 08012008, ari pikivirta
//  * added more premade functions under the visible component class
//  * example made better for easier understanding (not using class though)
//

interface

uses
  Windows, Messages, SysUtils, Classes, API_base;

type
  TAPI_compress = class(TAPI_Custom_Component)
  private
  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
  published
    // all functions result ratio
    function  CompressStream(inpStream, outStream: TStream): double;
    function  DecompressStream(inpStream, outStream: TStream): double;
    function  CompressFile(Source, Target: string): double;
    function  DecompressFile(Source, Target: string): double;
    function  CompressStringlist(Source: tstringlist; TargetFilename: string): double;
    function  DecompressStringlist(SourceFilename: string; var Target: tstringlist): double;
  end;

procedure Register;

//------------------------------------------------------------------------------
// compress and decompress streams (ZLib)
//------------------------------------------------------------------------------
procedure CompressStream(inpStream, outStream: TStream);
procedure DecompressStream(inpStream, outStream: TStream);

implementation

{$include 'inc\CompilerVersions.INC'}
{$R *.RES}

uses
  ZLib; // stream compress

const
  versioninfo = 'r1.01/ari.pikivirta(at)kolumbus.fi';

//------------------------------------------------------------------------------
constructor tAPI_compress.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:=versioninfo;
end;

//------------------------------------------------------------------------------
destructor tAPI_compress.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_Compress.CompressStream(inpStream, outStream: TStream): double;
begin
  api_compress.compressstream(inpstream, outstream);
  result:= 100 * (inpstream.size - outstream.size) / inpstream.size;
end;

//------------------------------------------------------------------------------
function TAPI_Compress.DecompressStream(inpStream, outStream: TStream): double;
begin
  api_compress.decompressstream(inpstream, outstream);
  result:= 100 * (inpstream.size - outstream.size) / inpstream.size;
end;

//------------------------------------------------------------------------------
function TAPI_Compress.CompressFile(Source, Target: string): double;
var
  fs_in, fs_out: tfilestream;
begin
  fs_in:= tfilestream.create(source, fmopenread, fmsharedenywrite);
  try
    fs_out:= tfilestream.create(target, fmcreate, fmshareexclusive);
    try
      result:= CompressStream(fs_in, fs_out);
      //result:= (fs_in.size - fs_out.size)/fs_in.size * 100;
    finally
      fs_out.free;
    end;
  finally
    fs_in.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Compress.DecompressFile(Source, Target: string): double;
var
  fs_in, fs_out: tfilestream;
begin
  fs_in:= tfilestream.create(source, fmopenread, fmsharedenywrite);
  try
    fs_out:= tfilestream.create(target, fmcreate, fmshareexclusive);
    try
      result:= DecompressStream(fs_in, fs_out);
    finally
      fs_out.free;
    end;
  finally
    fs_in.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Compress.CompressStringlist(Source: tstringlist; TargetFilename: string): double;
var
  ms_in: tmemorystream;
  fs_out: tfilestream;
begin
  ms_in:= tmemorystream.create;
  try
    ms_in.clear;
    source.SaveToStream(ms_in);
    fs_out:= tfilestream.create(targetFilename, fmcreate, fmshareexclusive);
    try
      result:= compressstream(ms_in, fs_out);
    finally
      fs_out.free;
    end;
  finally
    ms_in.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_compress.DecompressStringlist(SourceFilename: string; var Target: tstringlist): double;
var
  fs_in: tfilestream;
  ms_out: tmemorystream;
begin
  fs_in:= tfilestream.create(sourcefilename, fmopenread, fmsharedenywrite);
  try
    fs_in.seek(int64(0), sofrombeginning);
    ms_out:= tmemorystream.create;
    try
      ms_out.clear;
      result:= decompressstream(fs_in, ms_out);
      target.LoadFromStream(ms_out);
    finally
      ms_out.free;
    end;
  finally
    fs_in.free;
  end;
end;

//------------------------------------------------------------------------------
procedure CompressStream(inpStream, outStream: TStream);
var
  InpBuf, OutBuf: Pointer;
  InpBytes, OutBytes: Integer;
begin
  InpBuf:= nil;
  OutBuf:= nil;
  try
    GetMem(InpBuf, inpStream.Size);
    inpStream.Position := 0;
    InpBytes := inpStream.Read(InpBuf^, inpStream.Size);
    {$IFDEF DELPHI2009UP}
    zCompress(InpBuf, InpBytes, OutBuf, OutBytes);
    {$ELSE}
    CompressBuf(InpBuf, InpBytes, OutBuf, OutBytes);
    {$ENDIF}
    outStream.Write(OutBuf^, OutBytes);
  finally
    if InpBuf <> nil then FreeMem(InpBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  end;
end;

//------------------------------------------------------------------------------
procedure DecompressStream(inpStream, outStream: TStream);
var
  InpBuf, OutBuf: Pointer;
  OutBytes, sz: Integer;
begin
  InpBuf:= nil;
  OutBuf:= nil;
  sz:= inpStream.Size - inpStream.Position;
  if sz > 0 then
  try
    GetMem(InpBuf, sz);
    inpStream.Read(InpBuf^, sz);
    {$IFDEF DELPHI2009UP}
    zDecompress(InpBuf, sz, OutBuf, OutBytes);
    {$ELSE}
    Decompressbuf(InpBuf, sz, 0, OutBuf, OutBytes);
    {$ENDIF}
    outStream.Write(OutBuf^, OutBytes);
  finally
    if InpBuf <> nil then FreeMem(InpBuf);
    if OutBuf <> nil then FreeMem(OutBuf);
  end;
  outStream.Position := 0;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_compress]);
end;

end.
