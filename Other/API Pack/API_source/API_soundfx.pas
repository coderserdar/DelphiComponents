unit API_soundfx;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Classes, Messages;

const
  TAPI_SOUNDFXMAXBUFFER = 1;

type
  TAPI_soundfx = class(TComponent)
  private
    fversion: string;
    fmscount: integer;
    fms: array[0..TAPI_SOUNDFXMAXBUFFER] of tmemorystream;
    procedure dummys(s: string);
  protected
  public
    constructor Create(AOWner: tcomponent); override;
    destructor  Destroy; override;
    procedure   PlaySound(FileName: string); overload;
    procedure   PlaySound(stream: tstream); overload;
  published
    property Version: string read fversion write dummys stored false;
  end;

procedure Register;

implementation

{$r *.res}

uses
  MMSystem;

//------------------------------------------------------------------------------
constructor TAPI_soundfx.Create(AOWner: TComponent);
var
  i: integer;
begin
  inherited create(aowner);
  fversion:= 'r1.00/ari.pikivirta@kolumbus.fi';
  for i:=0 to TAPI_SOUNDFXMAXBUFFER-1 do
  begin
    fms[i]:= tmemorystream.create;
    fms[i].clear;
  end;
  fmscount:= 0;
end;

//------------------------------------------------------------------------------
destructor TAPI_soundfx.Destroy;
var
  i: integer;
begin
  for i:=0 to TAPI_SOUNDFXMAXBUFFER-1 do
    fms[i].free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_soundfx.dummys(s: string);
begin
  // this function does nothing.
end;

//------------------------------------------------------------------------------
procedure TAPI_soundfx.PlaySound(stream: tstream);
begin
  fmscount:= fmscount + 1;
  if fmscount>TAPI_SOUNDFXMAXBUFFER-1 then fmscount:= 0;
  fms[fmscount].CopyFrom(stream, stream.size);
  fms[fmscount].Seek(0,0);
  mmsystem.playsound(fms[fmscount].memory, 0, SND_ASYNC or SND_MEMORY);
end;

//------------------------------------------------------------------------------
procedure TAPI_soundfx.playsound(filename: string);
var
  fs: tfilestream;
begin
  if fileexists( filename ) then
  begin
    fs:= tfilestream.Create(filename, fmOpenRead);
    try
      PlaySound(fs);
    finally
      fs.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_soundfx]);
end;

end.
