unit API_twincatIO;

// API_twincatIO unit to handle beckhoff's twincat IO communication easily.
// note, that using this unit will need you to have TCatIoDrv.dll library
// to be installed in your computer (and found ini %path% variable)
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
  sysutils, classes, dialogs, forms, syncobjs, windows;

type
  tbuffer = array[0..255] of byte;
  pbuffer = ^tbuffer;

  tciothread = class(tthread)
  private
    cycletime: integer;
    procedure readinputs;
    procedure writeoutputs;
  protected
    procedure execute; override;
  public
  end;

var
  inputs: tbuffer;
  outputs: tbuffer;

// additional
procedure setbit (var fbyte: byte; bitnr:byte; state: boolean);
function  bitisset (fbyte, bitnr:byte): boolean;

// basic stuff
procedure tcio_init;
procedure tcio_kill;
procedure tcio_defaults;
function  tcio_open(var txt: string; task, inlength, outlength: integer; timeout: integer): boolean;
function  tcio_isopen: boolean;
function  tcio_close: boolean;

// direct io access
function  tcio_readinputs(var inputimage: tbuffer): boolean;
function  tcio_writeoutputs(outputimage: tbuffer): boolean;

// io thread
function  tcio_start(cycletimeMS: integer): boolean;
function  tcio_isrunning: boolean;
function  tcio_stop: boolean;
function  tcio_reset: boolean;

implementation

uses
  syncobjs;

var
  _iothread: tciothread;
  _ioopen: boolean;
  _iorun: boolean;
  _ioport: integer;
  _inimageptr: pbuffer;
  _outimageptr: pbuffer;
  _iolock: tcriticalsection;

//------------------------------------------------------------------------------
// these functions need TCatIODrv.dll to be present at some of windows %path%
// variable folders.
function TCatIoOpen: longint; stdcall; external 'TCatIoDrv.dll' name '_TCatIoOpen@0';
function TCatIoClose: longint; stdcall; external 'TCatIoDrv.dll' name '_TCatIoClose@0';
function TCatIoInputUpdate(nPort: WORD): longint; stdcall; external 'TCatIoDrv.dll' name '_TCatIoInputUpdate@4';
function TCatIoOutputUpdate(nPort: WORD): longint; stdcall; external 'TCatIoDrv.dll' name  '_TCatIoOutputUpdate@4';
function TCatIoGetInputPtr(nPort: WORD; var pInput: Pointer; nSize: longint): longint; stdcall; external 'TCatIoDrv.dll' name  '_TCatIoGetInputPtr@12';
function TCatIoGetOutputPtr(nPort: WORD; var pOutput: Pointer; nSize: longint ): longint; stdcall; external 'TCatIoDrv.dll' name  '_TCatIoGetOutputPtr@12';
function TCatIoReset(): longint; stdcall; external 'TCatIoDrv.dll' name '_TCatIoReset@0';
function TCatIoGetCpuTime(var pCpuTime: TFileTime ): longint; stdcall; external 'TCatIoDrv.dll' name  '_TCatIoGetCpuTime@4';
function TCatIoGetCpuCounter(var pCpuCount: int64 ): longint; stdcall; external 'TCatIoDrv.dll' name  '_TCatIoGetCpuCounter@4';

//------------------------------------------------------------------------------
// sets specified bit of fbyte to have state specified
procedure setbit (var fbyte: byte; bitnr:byte; state: boolean);
begin
  if state then
    fbyte:=fbyte or (1 shl Bitnr) else
    fbyte:=fbyte and ($FF xor (1 shl Bitnr));
end;

//------------------------------------------------------------------------------
// returns true if specified bit of byte is high
function bitisset (fbyte, bitnr:byte): boolean;
begin
  result:=(fbyte and (1 shl Bitnr))<>0;
end;

//------------------------------------------------------------------------------
procedure tcio_init;
begin
  // init some variables
  _ioopen:= false;
  _iorun:= false;
  _iolock:= tcriticalsection.Create;
end;

//------------------------------------------------------------------------------
procedure tcio_kill;
begin
  // do deinitialization stuff
  if tcio_isrunning then
    tcio_stop;
  if tcio_isopen then
    tcio_close;
  _iolock.Free;
end;

//------------------------------------------------------------------------------
function tcio_open(var txt: string; task, inlength, outlength: integer; timeout: integer): boolean;
var
  res: integer;
  waitstart: tdatetime;
  ok: boolean;
begin
  result:= false;
  txt:= 'Ok';

  // init tc io mapping
  if _ioopen then
  begin
    txt:= 'io already open';
    exit;
  end;

  // open twincat
  res:= tcatioopen;
  if res<>0 then
  begin
    // failed to open tcat io
    txt:= 'failed to open tcat io';
    exit;
  end;

  _ioport:= task;

  // setup timeout
  waitstart:= false;
  ok:= false;
  while (not ok) and (millisecondspan(waitstart, now)<timeout) do
  begin

    // get pointers
    if (tcatiogetinputptr(_ioport, pointer( _inimageptr ), inlength) = 0) and
      (tcatiogetoutputptr(_ioport, pointer( _outimageptr ), outlength) = 0) then
    begin
      // succesfull
      _ioopen:= true;
      txt:= 'twincat io opened succesfully.';
      ok:= true;
    end else
    begin
      // failed
      txt:= 'failed to assign pointers for input and output images.';
      TCatIoClose;
      sleep(5);
    end;

  end;

  result:= ok;
end;

//------------------------------------------------------------------------------
function  tcio_start(cycletimeMS: integer): boolean;
begin
  // start thread and io
  result:= false;
  if (_ioopen) and (not _iorun) then
  begin
    _iothread:= tciothread.Create(true);
    _iothread.cycletime:= cycletimeMS;
    _iothread.Priority:= tpnormal;
    _iothread.FreeOnTerminate:= true;
    _iothread.Resume;
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function tcio_isopen: boolean;
begin
  result:= _ioopen;
end;

//------------------------------------------------------------------------------
function tcio_isrunning: boolean;
begin
  result:= _iorun;
end;

//------------------------------------------------------------------------------
function tcio_stop: boolean;
begin
  // stop thread and io
  result:= false;
  if _iorun then
  begin
    _iothread.terminate;
    while (_iorun) do sleep(1);
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function tcio_reset: boolean;
begin
  result:= false;
  if _ioopen then
  begin
    TCatIoReset;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
function tcio_close: boolean;
begin
  result:= false;
  // free twincat io if open
  if _ioopen then
  begin
    _ioopen:= false;
    TCatIoClose;
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
procedure tciothread.execute;
begin
  _iorun:= true;

  while (not terminated) do
  begin
    sleep( cycletime );
    synchronize( readinputs );
    synchronize( writeoutputs );
  end;

  _iorun:= false;
end;

//------------------------------------------------------------------------------
procedure tciothread.readinputs;
begin
  tcio_readinputs( inputs );
end;

//------------------------------------------------------------------------------
procedure tciothread.writeoutputs;
begin
  tcio_writeoutputs( outputs );
end;

//------------------------------------------------------------------------------
function tcio_readinputs(var inputimage: tbuffer): boolean;
begin
  result:= false;
  if not _ioopen then exit;
  if (tcatioinputupdate(_ioport)=0) then
  begin
    inputimage:= _inimageptr^;
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function  tcio_writeoutputs(outputimage: tbuffer): boolean;
begin
  result:= false;
  if not _ioopen then exit;
  _outimageptr^:=  outputimage;
  result:= (tcatiooutputupdate(_ioport) = 0);
end;

end.
