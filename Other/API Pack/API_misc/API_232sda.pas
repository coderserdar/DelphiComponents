unit API_232sda;

// component to control b&b 232sda12 module
// r1.00, ari pikivirta
// * first release, all basic functions included

interface

uses
  SysUtils, Classes, Windows;

const
  // analog channels
  SDA_CH1 = #0;
  SDA_CH2 = #1;
  SDA_CH3 = #2;
  SDA_CH4 = #3;
  SDA_CH5 = #4;
  SDA_CH6 = #5;
  SDA_CH7 = #6;
  SDA_CH8 = #7;
  SDA_CH9 = #8;
  SDA_CH10 = #9;
  SDA_CH11 = #10;

  // communication strings
  SDA_READAD = #33+#48+#82+#65; // + number of channels > returns high&low bytes of each
  SDA_READDI = #33+#48+#82+#68; // returns 1 byte
  SDA_WRITEDI = #33+#48+#83+#79; // + states bit2, bit1, bit0

type
  tapi_232sdaerrorevent = procedure (sender: tobject; errormessage: string);

  Tport = (
    _None,
    _Com1,
    _Com2,
    _Com3,
    _Com4,
    _Com5,
    _Com6,
    _Com7,
    _Com8
  );

  Tbaudrate = (
    br110,
    br300,
    br600,
    br1200,
    br2400,
    br4800,
    br9600,
    br14400,
    br19200,
    br38400,
    br56000,
    br115200,
    br128000,
    br256000
  );

  Tparity = (
    pNone,
    pOdd,
    pEven,
    pMark
  );

  Tdatabits = (
    dbFour,
    dbFive,
    dbSix,
    dbSeven,
    dbEight
  );

  Tstopbits = (
    sbOne,
    sbOnePointFive,
    sbTwo
  );

  TAPI_232sda = class(TComponent)
  private
    fversion: string;
    flasterror: string;

    // 232sda12
    finputwaitcycles: word;
    fai: array[0..10] of word;
    fdi: array[0..2] of boolean;
    fdo: array[0..2] of boolean;

    // serial port
    fopen: boolean;
    fhandle: thandle;
    fport: tport;
    fbaudrate: tbaudrate;
    fparity: tparity;
    fdatabits: tdatabits;
    fstopbits: tstopbits;
    finbuffer: integer;
    foutbuffer: integer;
    freadtimeout: cardinal;
    fwritetimeout: cardinal;
    finque: integer;
    foutque: integer;

    fonerror: tapi_232sdaerrorevent;

    // dummies
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure dummyw(w: word);
    procedure dummyb(b: boolean);

    // 232sda12
    procedure setinputwaitcycles(w: word);
    procedure setoutput00(b: boolean);
    procedure setoutput01(b: boolean);
    procedure setoutput02(b: boolean);

    // serial port
    procedure setopen(b: boolean);
    procedure setport(p: tport);
    procedure setbaudrate(b: tbaudrate);
    procedure setparity(p: tparity);
    procedure setdatabits(d: tdatabits);
    procedure setstopbits(s: tstopbits);
    procedure setinbuffer (i: integer);
    procedure setoutbuffer (i: integer);
    procedure setreadtimeout (c: cardinal);
    procedure setwritetimeout (c: cardinal);
    function _open: boolean; // internal
    function _close: boolean; // internal
    function _read( var s: string; inputlength: word ): boolean;
    function _write( s: string ): boolean;
    procedure _refreshques;

  protected
  public

    constructor create (aowner: tcomponent); override;
    destructor destroy; override;

    // 232sda12
    function readwrite: boolean;

    // settings related
    function savesettings( fname: string ): boolean;
    function opensettings( fname: string ): boolean;

    procedure setbit (var fbyte: byte; bitnr:byte; state: boolean);
    function bitisset (fbyte, bitnr:byte): boolean;

  published

    // common
    property version: string read fversion write dummys;
    property lasterror: string read flasterror write dummys;

    // 232sda12
    property inputwaitcycles: word read finputwaitcycles write setinputwaitcycles;
    property ai00: word read fai[0] write dummyw;
    property ai01: word read fai[1] write dummyw;
    property ai02: word read fai[2] write dummyw;
    property ai03: word read fai[3] write dummyw;
    property ai04: word read fai[4] write dummyw;
    property ai05: word read fai[5] write dummyw;
    property ai06: word read fai[6] write dummyw;
    property ai07: word read fai[7] write dummyw;
    property ai08: word read fai[8] write dummyw;
    property ai09: word read fai[9] write dummyw;
    property ai10: word read fai[10] write dummyw;
    property di00: boolean read fdi[0] write dummyb;
    property di01: boolean read fdi[1] write dummyb;
    property di02: boolean read fdi[2] write dummyb;
    property do00: boolean read fdo[0] write setoutput00;
    property do01: boolean read fdo[1] write setoutput01;
    property do02: boolean read fdo[2] write setoutput02;

    // serial port
    property open: boolean read fopen write setopen;
    property port: tport read fport write setport;
    property baudrate: tbaudrate read fbaudrate write setbaudrate;
    property parity: tparity read fparity write setparity;
    property databits: tdatabits read fdatabits write setdatabits;
    property stopbits: tstopbits read fstopbits write setstopbits;
    property bufferR: integer read finbuffer write setinbuffer;
    property bufferW: integer read foutbuffer write setoutbuffer;
    property timeoutR: cardinal read freadtimeout write setreadtimeout;
    property timeoutW: cardinal read fwritetimeout write setwritetimeout;
    property queR: integer read finque write dummyi;
    property queW: integer read foutque write dummyi;

  end;

var
  comstat: pcomstat;

procedure Register;

implementation

const
  versioninfo: string = 'R1.00/ari.pikivirta@kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_232sda.create (aowner: tcomponent);
begin
  inherited create (aowner);

  // common
  fversion:=versioninfo;
  flasterror:='';

  // 232sda12
  finputwaitcycles:=1000;
  fillchar(fai, sizeof(fai), 0);
  fillchar(fdi, sizeof(fdi), 0);
  fillchar(fdo, sizeof(fdo), 0);

  // serial port
  fopen:=false;
  fhandle:=invalid_handle_value;
  fport:=_None;
  fbaudrate:=br9600;
  fparity:=peven;
  fdatabits:=dbeight;
  fstopbits:=sbone;
  finbuffer:=512;
  foutbuffer:=512;
  freadtimeout:=1000;
  fwritetimeout:=1000;
end;

//------------------------------------------------------------------------------
destructor TAPI_232sda.destroy;
begin
  // close port if left open
  if fopen then
    _close;

  // destroy class
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.dummys(s: string);
begin
  // do nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.dummyi(i: integer);
begin
  // do nothing..
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.dummyw(w: word);
begin
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.dummyb(b: boolean);
begin
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setoutput00(b: boolean);
begin
  fdo[0]:=b;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setoutput01(b: boolean);
begin
  fdo[1]:=b;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setinputwaitcycles(w: word);
begin
  if w<1 then exit;
  finputwaitcycles:=w;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setoutput02(b: boolean);
begin
  fdo[2]:=B;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setopen (b: boolean);
begin
  if b<>fopen then
  begin
    if fopen then
    begin
      // close port
      _close;
    end else
    begin
      // open port
      _open;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setport (p: tport);
begin
  if p<>fport then
  begin
    if fopen then
    begin
      flasterror:='Cannot change port while port is open.';
      exit;
    end;
    // change port
    fport:=p;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setbaudrate (b: tbaudrate);
begin
  if b<>fbaudrate then
  begin
    if fopen then
    begin
      flasterror:='Cannot change baudrate while port is open.';
      exit;
    end;
    // change baudrate
    fbaudrate:=b;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setparity (p: tparity);
begin
  if p<>fparity then
  begin
    if fopen then
    begin
      flasterror:='Cannot change parity while port is open.';
      exit;
    end;
    // change parity
    fparity:=p;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setdatabits (d: tdatabits);
begin
  if d<>fdatabits then
  begin
    if fopen then
    begin
      flasterror:='Cannot change databits while port is open.';
      exit;
    end;
    // change databits
    fdatabits:=d;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setstopbits (s: tstopbits);
begin
  if s<>fstopbits then
  begin
    if fopen then
    begin
      flasterror:='Cannot change stopbits while port is open.';
      exit;
    end;
    // change stop bits
    fstopbits:=s;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setreadtimeout(c: cardinal);
begin
  if c<>freadtimeout then
  begin
    if fopen then
    begin
      flasterror:='Cannot change rx timeout while port is open.';
      exit;
    end;
    // change read timeout
    freadtimeout:=c;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setwritetimeout(c: cardinal);
begin
  if c<>fwritetimeout then
  begin
    if fopen then
    begin
      flasterror:='Cannot change tx timeout while port is open.';
      exit;
    end;
    // set write timeout
    fwritetimeout:=c;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setinbuffer(i: integer);
begin
  if i<>finbuffer then
  begin
    if fopen then
    begin
      flasterror:='Cannot change rx buffer size while port is open.';
      exit;
    end;
    // change inbuffer size
    finbuffer:=i;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setoutbuffer(i: integer);
begin
  if i<>foutbuffer then
  begin
    if fopen then
    begin
      flasterror:='Cannot change tx buffer size while port is open.';
      exit;
    end;
    // change outbuffer size
    foutbuffer:=i;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_232sda._open: boolean;
var
  s: array[0..259] of Char; // for delphi 8
  commtimeouts: tcommtimeouts;
  dcb: _dcb;
  i: integer;
begin
  result:=false;

  // check if open
  if fopen then
  begin
    flasterror:='Port is already open.';
    exit;
  end;

  // fillchar replacement
  // for delphi 8
  for i:=0 to sizeof(s)-1 do
    s[i]:=#0;

  // get device name
  case fport of
    _none: exit;
    _com1: s:='COM1';
    _com2: s:='COM2';
    _com3: s:='COM3';
    _com4: s:='COM4';
    _com5: s:='COM5';
    _com6: s:='COM6';
    _com7: s:='COM7';
    _com8: s:='COM8';
    else
    begin
      flasterror:='Unable to locate selected com port.';
      exit;
    end;
  end;

  // open handle
  fhandle := CreateFile(
    s,
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0);

  // result
  if (fhandle=invalid_handle_value) then
  begin
    flasterror:='Failed to create com file.';
    exit;
  end;

  // set buffer sizes
  if not setupcomm(
    fhandle,
    foutbuffer,
    finbuffer) then
  begin
    flasterror:='Failed to setup comm buffers.';
    closehandle(fhandle);
    exit;
  end;

  // get dcb struct
  if not getcommstate(
    fhandle,
    dcb) then
  begin
    flasterror:='Failed to get comm state.';
    closehandle(fhandle);
    exit;
  end;

   // set baudrate
  case fbaudrate of
    br110: dcb.BaudRate:=CBR_110;
    br300: dcb.BaudRate:=CBR_300;
    br600: dcb.BaudRate:=CBR_600;
    br1200: dcb.BaudRate:=CBR_1200;
    br2400: dcb.BaudRate:=CBR_2400;
    br4800: dcb.BaudRate:=CBR_4800;
    br9600: dcb.BaudRate:=CBR_9600;
    br14400: dcb.BaudRate:=CBR_14400;
    br19200: dcb.BaudRate:=CBR_19200;
    br38400: dcb.BaudRate:=CBR_38400;
    br56000: dcb.baudrate:=CBR_56000;
    br115200: dcb.BaudRate:=CBR_115200;
    br128000: dcb.BaudRate:=CBR_128000;
    br256000: dcb.BaudRate:=CBR_256000;
  end;

  // set bytesize
  case fdatabits of
    dbFour: dcb.ByteSize:=4;
    dbFive: dcb.ByteSize:=5;
    dbSix: dcb.ByteSize:=6;
    dbSeven: dcb.ByteSize:=7;
    dbEight: dcb.ByteSize:=8;
  end;

  // set parity
  case fparity of
    pNone: dcb.parity:=NOPARITY;
    pOdd: dcb.parity:=ODDPARITY;
    pEven: dcb.parity:=EVENPARITY;
    pMark: dcb.parity:=MARKPARITY;
  end;

  // set stopbits
  case fstopbits of
    sbOne: dcb.StopBits:= ONESTOPBIT;
    sbOnePointFive: dcb.StopBits:= ONE5STOPBITS;
    sbTwo: dcb.StopBits:= TWOSTOPBITS;
  end;

  dcb.Flags:=1;
  {
    DWORD fBinary: 1;          // binary mode, no EOF check
    DWORD fParity: 1;          // enable parity checking
    DWORD fOutxCtsFlow:1;      // CTS output flow control
    DWORD fOutxDsrFlow:1;      // DSR output flow control
    DWORD fDtrControl:2;       // DTR flow control type
    DWORD fDsrSensitivity:1;   // DSR sensitivity
    DWORD fTXContinueOnXoff:1; // XOFF continues Tx
    DWORD fOutX: 1;            // XON/XOFF out flow control
    DWORD fInX: 1;             // XON/XOFF in flow control
    DWORD fErrorChar: 1;       // enable error replacement
    DWORD fNull: 1;            // enable null stripping
    DWORD fRtsControl:2;       // RTS flow control
    DWORD fAbortOnError:1;     // abort reads/writes on error
    DWORD fDummy2:17;          // reserved
  }

  // set dcb struct
  if not setcommstate(
    fhandle,
    DCB) then
  begin
    flasterror:='Failed to set comm state.';
    closehandle(fhandle);
    exit;
  end;

  // set timeouts
  commtimeouts.ReadIntervalTimeout:=0;
  commtimeouts.ReadTotalTimeoutMultiplier:=0;
  commtimeouts.ReadTotalTimeoutConstant:= freadtimeout;
  commtimeouts.WriteTotalTimeoutMultiplier:=0;
  commtimeouts.WriteTotalTimeoutConstant:= fwritetimeout;

  if not setcommtimeouts(
    fhandle,
    commtimeouts) then
  begin
    flasterror:='Failed to set comm timeouts.';
    closehandle(fhandle);
    exit;
  end;

  PurgeComm(fhandle, PURGE_TXABORT or PURGE_TXCLEAR);
  PurgeComm(fhandle, PURGE_RXABORT or PURGE_RXCLEAR);

  new(comstat);
  fopen:=true;
  result:=true;
  flasterror:='ok';
end;

//------------------------------------------------------------------------------
function TAPI_232sda._close: boolean;
begin
  result:=False;

  // check if open
  if not fopen then
  begin
    flasterror:='Port is not open.';
    exit;
  end;

  // close handle
  dispose(comstat);
  closehandle(fhandle);
  fopen:=false;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda._refreshques;
var
  errors: cardinal;
begin
  errors:=0;

  if not fopen then
  begin
    flasterror:='Port is not open (getrxdize).';
    exit;
  end;

  clearcommerror(fhandle, errors, comstat);

  case errors of
    CE_BREAK: flasterror:='The hardware detected a break condition.';
    CE_DNS: flasterror:='A parallel device is not selected.';
    CE_FRAME: flasterror:='The hardware detected a framing error.';
    CE_IOE: flasterror:='An I/O error occurred during communications with the device.';
    CE_MODE: flasterror:='The requested mode is not supported, or the hFile parameter is invalid. If this value is specified, it is the only valid error.';
    CE_OOP: flasterror:='A parallel device signaled that it is out of paper.';
    CE_OVERRUN: flasterror:='A character-buffer overrun has occurred. The next character is lost.¨';
    CE_PTO: flasterror:='A time-out occurred on a parallel device.';
    CE_RXOVER: flasterror:='An input buffer overflow has occurred. There is either no room in the input buffer, or a character was received after the end-of-file (EOF) character.';
    CE_RXPARITY: flasterror:='The hardware detected a parity error.';
    CE_TXFULL: flasterror:='The application tried to transmit a character, but the output buffer was full.';
  end;

  // refresh que values
  foutque:=comstat^.cbOutQue;
  finque:=comstat^.cbInQue;
end;

//------------------------------------------------------------------------------
function TAPI_232sda._write( s: string ): boolean;
var
  bw: dword;
  buf: array[0..300] of byte;
  len: cardinal;
  i: integer;
begin
  result:=False;

  // check if open
  if not fopen then
  begin
    flasterror:='Port is not open.';
    exit;
  end;

  len:=length(s);
  if len>300 then len:=300;

  for i:=1 to len do
  begin
    buf[i-1]:=ord(s[i]);
  end;

  // send to outbuffer
  if not writefile(
    fhandle,
    buf,
    len,
    bw,
    nil) then
  begin
    flasterror:='Failed to write string to port.';
    exit;
  end;

  _refreshques;

  // result
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_232sda._read( var s: string; inputlength: word ): boolean;
var
  br: dword;
  buf: pchar;
  i: integer;
  len: integer;
  ws: integer;
begin
  result:=False;
  s:='';

  // check if open
  if not fopen then
  begin
    flasterror:='Cannot read string while port is not open.';
    exit;
  end;

  len:= inputlength;

  ws:=0;
  while (ws<finputwaitcycles) and (finque<len) do
  begin
    _refreshques;
    ws:=ws+1;
  end;

  if ws>=finputwaitcycles then
  begin
    flasterror:='Wait cycle count exceeded.';
    exit;
  end;

  // read buffer
  if not readfile(
    fhandle,
    buf,
    len,
    br,
    nil) then
  begin
    flasterror:='Failed to read port.';
    exit;
  end;

  // copy buffer to string
  for i:=0 to br-1 do
    s:=s+buf[i];

  // result
  result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_232sda.setbit (var fbyte: byte; bitnr:byte; state: boolean);
begin
  if state then
    fbyte:=fbyte or (1 shl Bitnr) else
    fbyte:=fbyte and ($FF xor (1 shl Bitnr));
end;

//------------------------------------------------------------------------------
function TAPI_232sda.bitisset (fbyte, bitnr:byte): boolean;
begin
  result:=(fbyte and (1 shl Bitnr))<>0;
end;

//------------------------------------------------------------------------------
function TAPI_232sda.readwrite: boolean;
var
  s: string;
  outputstring: string;
  i: integer;
  b1, b2: byte;
begin
  result:=false;

  // readwrite analog channels
  if _write(SDA_READAD+#10) then
    if _read(s, 11*2) then
    try
      // parse input bytes
      for i:=0 to 10 do
      begin
        // get low and high bytes
        b1:=ord(s[i*2+1]);
        b2:=ord(s[i*2+2]);
        // combine bytes to one word
        fai[i]:=b2*256 + b1;
      end;
      result:=true;
    except
      // failed to parse input
    end;

  // readwrite digital inputs
  if result then
    if _write(SDA_READDI) then
      if _read(s, 1) then
      try
        // parse input byte
        b1:=ord(s[1]);
        fdi[0]:=bitisset(b1,0);
        fdi[1]:=bitisset(b1,1);
        fdi[2]:=bitisset(b1,2);
        result:=true;
      except
        // failed to parse byte
      end;

  // write digital outputs
  if result then
    if _write(SDA_WRITEDI+outputstring) then
      result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_232sda.savesettings( fname: string ): boolean;
var
  f: textfile;
begin
  result:=false;
  {$i-}
  assignfile(f,fname);
  rewrite(f);
  {$i+}
  if ioresult=0 then
  begin
    // serial port
    writeln(f,integer(fport));
    writeln(f,integer(fbaudrate));
    writeln(f,integer(fparity));
    writeln(f,integer(fdatabits));
    writeln(f,integer(fstopbits));
    writeln(f,finbuffer);
    writeln(f,foutbuffer);
    writeln(f,freadtimeout);
    writeln(f,fwritetimeout);

    closefile(f);
    result:=true;
  end else
  begin
    flasterror:='Failed to save settings.';
  end;
end;

//------------------------------------------------------------------------------

function TAPI_232sda.opensettings( fname: string ): boolean;
var
  f: textfile;
  i: integer;
begin
  result:=false;
  {$i-}
  assignfile(f,fname);
  reset(f);
  {$i+}
  if ioresult=0 then
  begin
    // serial port
    readln(f,i); fport:=tport(i);
    readln(f,i); fbaudrate:=tbaudrate(i);
    readln(f,i); fparity:=tparity(i);
    readln(f,i); fdatabits:=tdatabits(i);
    readln(f,i); fstopbits:=tstopbits(i);
    readln(f,finbuffer);
    readln(f,foutbuffer);
    readln(f,freadtimeout);
    readln(f,fwritetimeout);

    closefile(f);
    result:=true;
  end else
  begin
    flasterror:='Failed to open settings.';
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API', [TAPI_232sda]);
end;

end.
