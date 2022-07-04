unit API_rs232;

//------------------------------------------------------------------------------
// serial communication component with setup dialog imtegrated
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
// r1.03, 19122008, ari pikivirta
//  * changed to use api_232rw unit
//
// r1.02, 10082004, ari pikivirta
//  * added port opened events
//  * added port closed events
//  * added write and read buffer function (byte array)
//
// r1.01, ari pikivirta
//  * added OnError event
//
// r1.00, ari pikivirta

interface

uses
  Windows, SysUtils, Classes, Messages;

type
  Tport = (_None, _Com1, _Com2, _Com3, _Com4, _Com5, _Com6, _Com7, _Com8);
  Tbaudrate = (br110, br300, br600, br1200, br2400, br4800, br9600,
    br14400, br19200, br38400, br56000, br115200, br128000, br256000);
  Tparity = (pNone, pOdd, pEven, pMark);
  Tdatabits = (dbFour, dbFive, dbSix, dbSeven, dbEight);
  Tstopbits = (sbOne, sbOnePointFive, sbTwo);

type
  TAPI_rs232 = class(TComponent)
  private
    fversion: string;
    fonportopen: tnotifyevent;
    fafterportopen: tnotifyevent;
    fonportclose: tnotifyevent;
    fafterportclose: tnotifyevent;

    function  BaudrateToCardinal(i: tbaudrate): cardinal;
    function  CardinalToBaudrate(i: cardinal): tbaudrate;

    function  getopen: boolean;
    procedure setopen(b: boolean);
    function  getport: tport;
    procedure setport(p: tport);
    function  getbaudrate: tbaudrate;
    procedure setbaudrate(b: tbaudrate);
    function  getparity: tparity;
    procedure setparity(p: tparity);
    function  getdatabits: tdatabits;
    procedure setdatabits(d: tdatabits);
    function  getstopbits: tstopbits;
    procedure setstopbits(s: tstopbits);
    procedure dummys(s: string);

  protected
  public
    constructor Create (aowner: tcomponent); override;
    destructor Destroy; override;

    function Read(var s: string): boolean;
    function Write(Const s: string): boolean;

    function ReadBuf( var buf: array of byte ): boolean;
    function WriteBuf( buf: array of byte ): boolean;

    function SaveSettings( fname: string ): boolean;
    function OpenSettings( fname: string ): boolean;

  published
    property version: string read fversion write dummys stored false;
    property OnPortOpen: tnotifyevent read fonportopen write fonportopen;
    property AfterPortOpen: tnotifyevent read fafterportopen write fafterportopen;
    property OnPortClose: tnotifyevent read fonportclose write fonportclose;
    property AfterPortClose: tnotifyevent read fafterportclose write fafterportclose;

    property Open: boolean read getopen write setopen;
    property Port: tport read getport write setport;
    property Baudrate: tbaudrate read getbaudrate write setbaudrate;
    property Parity: tparity read getparity write setparity;
    property Databits: tdatabits read getdatabits write setdatabits;
    property Stopbits: tstopbits read getstopbits write setstopbits;
  end;

procedure Register;

implementation

{$R *.RES}

uses
  api_232rw;

const
  versioninfo: string = 'r1.03/ari.pikivirta@kolumbus.fi';

procedure TAPI_rs232.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_rs232.create (aowner: tcomponent);
begin
  inherited create (aowner);
  fversion:= versioninfo;
  com_init;
end;

//------------------------------------------------------------------------------
destructor TAPI_rs232.destroy;
begin
  com_close;
  com_kill;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function  TAPI_rs232.getopen: boolean;
begin
  result:= com_isopen;
end;

procedure TAPI_rs232.setopen (b: boolean);
begin
  if com_isopen then
  begin
    if assigned(fonportclose) then fonportclose(self);
    com_close;
    if assigned(fafterportclose) then fafterportclose(self);
  end;
  if b then
  begin
    if assigned(fonportopen) then fonportopen(self);
    com_open;
    if assigned(fafterportopen) then fafterportopen(self);
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_rs232.getport: tport;
begin
  result:= tport(api_232rw.com_getport);
end;

procedure TAPI_rs232.setport (p: tport);
begin
  com_setport(integer(p));
end;

//------------------------------------------------------------------------------
function TAPI_rs232.BaudrateToCardinal(i: tbaudrate): cardinal;
begin
  case i of
    br110: result:=     CBR_110;
    br300: result:=     CBR_300;
    br600: result:=     CBR_600;
    br1200: result:=    CBR_1200;
    br2400: result:=    CBR_2400;
    br4800: result:=    CBR_4800;
    br9600: result:=    CBR_9600;
    br14400: result:=   CBR_14400;
    br19200: result:=   CBR_19200;
    br38400: result:=   CBR_38400;
    br56000: result:=   CBR_56000;
    br115200: result:=  CBR_115200;
    br128000: result:=  CBR_128000;
    br256000: result:=  CBR_256000;
  else
    // erronous case!
    result:= 0;
  end;
end;

function TAPI_rs232.CardinalToBaudrate(i: cardinal): tbaudrate;
begin
  case i of
    CBR_110: result:=   br110;
    CBR_300: result:=   br300;
    CBR_600: result:=   br600;
    CBR_1200: result:=  br1200;
    CBR_2400: result:=  br2400;
    CBR_4800: result:=  br4800;
    CBR_9600: result:=  br9600;
    CBR_14400: result:= br14400;
    CBR_19200: result:= br19200;
    CBR_38400: result:= br38400;
    CBR_56000: result:= br56000;
    CBR_115200: result:=br115200;
    CBR_128000: result:=br128000;
    CBR_256000: result:=br256000;
  else
    // erronous case!
    result:= tbaudrate(0);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_rs232.getbaudrate: tbaudrate;
begin
  result:= CardinalToBaudrate(api_232rw.com_getbaudrate);
end;

procedure TAPI_rs232.setbaudrate (b: tbaudrate);
begin
  com_setbaudrate(BaudrateToCardinal(b));
end;

//------------------------------------------------------------------------------
function TAPI_rs232.getparity: tparity;
begin
  //Tparity = (pNone, pOdd, pEven, pMark);
  case com_getparity of
    NOPARITY: result:= pNone;
    ODDPARITY: result:= pOdd;
    EVENPARITY: result:= pEven;
    MARKPARITY: result:= pMark;
  else
    // erronous case!
    result:= pNone;
  end;
end;

procedure TAPI_rs232.setparity (p: tparity);
begin
  case p of
    pOdd: com_setparity(ODDPARITY);
    pEven: com_setparity(EVENPARITY);
    pMark: com_setparity(MARKPARITY);
  else
    // erronois or no parity
    com_setparity(NOPARITY);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_rs232.getdatabits: tdatabits;
begin
  //Tdatabits = (dbFour, dbFive, dbSix, dbSeven, dbEight);
  result:= tdatabits(com_getbytesize-4);
end;

procedure TAPI_rs232.setdatabits (d: tdatabits);
begin
  //Tdatabits = (dbFour, dbFive, dbSix, dbSeven, dbEight);
  api_232rw.com_setbytesize(integer(d)+4);
end;

//------------------------------------------------------------------------------
function TAPI_rs232.getstopbits: tstopbits;
begin
  // Tstopbits = (sbOne, sbOnePointFive, sbTwo);
  case com_getstopbits of
    windows.ONE5STOPBITS: result:= sbOnePointFive;
    windows.TWOSTOPBITS: result:= sbTwo;
  else
    result:= sbOne;
  end;
end;

procedure TAPI_rs232.setstopbits (s: tstopbits);
begin
  // Tstopbits = (sbOne, sbOnePointFive, sbTwo);
  case s of
    sbOnePointFive: com_setstopbits(windows.ONE5STOPBITS);
    sbTwo: com_setstopbits(windows.TWOSTOPBITS);
  else
    com_setstopbits(windows.ONESTOPBIT);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_rs232.write(Const s: string): boolean;
begin
  result:= api_232rw.com_write(ansistring(s));
end;

//------------------------------------------------------------------------------
function TAPI_rs232.read(var s: string): boolean;
begin
  result:= api_232rw.com_read(ansistring(s));
end;

//------------------------------------------------------------------------------
function TAPI_rs232.readbuf( var buf: array of byte ): boolean;
var
  br, i, pos: integer;
  cb: tcombuffer;
begin
  result:= api_232rw.com_readbuf( cb, br );

  if result then                      // if succesfully read something
  begin
    pos:= low(buf);                   // point to start of array
    for i:=0 to br-1 do               // copy buffers
    begin
      buf[pos]:= cb[i];
      pos:= pos + 1;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_rs232.writebuf( buf: array of byte ): boolean;
var
  cb: tcombuffer;
  len, i, pos: integer;
begin
  len:= high(buf)-low(buf);         // get size
  pos:= 0;
  for i:=low(buf) to high(buf) do   // copy buffer
  begin
    cb[pos]:= buf[i];               // to tcombuffer
    pos:= pos + 1;                  // inc position
  end;

  result:= api_232rw.com_writebuf(cb, len);
end;

//------------------------------------------------------------------------------
function TAPI_rs232.savesettings( fname: string ): boolean;
var
  f: textfile;
begin
  result:=false;
  {$i-}
  assignfile(f,fname);
  rewrite(f);
  {$i+}
  if ioresult=0 then
  try
    // serial port
    writeln(f,integer(getport));
    writeln(f,integer(getbaudrate));
    writeln(f,integer(getparity));
    writeln(f,integer(getdatabits));
    writeln(f,integer(getstopbits));
    result:=true;
  finally
    closefile(f);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_rs232.opensettings( fname: string ): boolean;
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
  try
    // serial port
    readln(f,i); setport( tport(i) );
    readln(f,i); setbaudrate( tbaudrate(i) );
    readln(f,i); setparity( tparity(i) );
    readln(f,i); setdatabits( tdatabits(i) );
    readln(f,i); setstopbits( tstopbits(i) );
    result:=true;
  finally
    closefile(f);
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_rs232]);
end;

end.
