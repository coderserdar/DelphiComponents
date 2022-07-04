unit API_labjack;

// component for interfacing labjack u12 io module (needs also the dll)
// r1.00, ari pikvirta
// * first release, all basic module functions included no automation yet
//   usage is through refresh_a for analogs, and refresh_d for digitals
//   analog inputs are only allowed as single ended (no support for gains)
// r1.01, ari pikivirta
// * added onerror event

interface

uses
  SysUtils, Classes;

type
  tlabjackerrorevent = procedure (sender: tobject; errormessage: string) of object;

  tlabjackmode_D = (
    _16i,               // 16 digital inputs
    _12i_4q,            // 12 digital inputs, 4 digital outputs
    _8i_8q,             // 8 digital inputs, 8 digital outputs
    _4i_12q,            // 4 digital inputs, 12 digital outputs
    _16q);              // 16 digital outputs

  tlabjackmode_IO = (
    _4i,                // 4 digital inputs
    _3i_1q,
    _2i_2q,
    _1i_3q,
    _4q);               // 4 digital outputs

{  tlabjackgain = (
    _20to20,                // +-20 volts
    _10to10,                // +-10 volts
    _5to5,                 // +-5 volts
    _4to4,                 // +-4 volts
    _2_5to2_5,               // +-2.5 volts
    _2to2,                 // +-2 volts
    _1_25to1_25,              // +-1.25 volts
    _1to1);                // +-1 volts}

type
  TAPI_labjack = class(TComponent)
  private
    // common properties
    fversion: string;
    flasterror: string;
    fonerror: tlabjackerrorevent;

    // labjack
    fmode_IO: tlabjackmode_IO;
    fmode_D: tlabjackmode_D;
{    fgain: tlabjackgain;}
    fai: array[0..7] of single;
    fao: array[0..1] of single;
    fd_IO: array[0..3] of boolean;
    fd_D: array[0..15] of boolean;

    // labjack
{    procedure setgain (gain: tlabjackgain);}
    procedure setmode_D (mode: tlabjackmode_D);
    procedure setmode_IO (mode: tlabjackmode_IO);

{    function parsegain (gain: tlabjackgain): integer;}
    function parsemode_d (mode: tlabjackmode_d): integer;
    function parsemode_io (mode: tlabjackmode_io): integer;

    procedure setao00 (value: single);
    procedure setao01 (value: single);

    procedure setio00 (b: boolean);
    procedure setio01 (b: boolean);
    procedure setio02 (b: boolean);
    procedure setio03 (b: boolean);

    procedure setd00 (b: boolean);
    procedure setd01 (b: boolean);
    procedure setd02 (b: boolean);
    procedure setd03 (b: boolean);
    procedure setd04 (b: boolean);
    procedure setd05 (b: boolean);
    procedure setd06 (b: boolean);
    procedure setd07 (b: boolean);
    procedure setd08 (b: boolean);
    procedure setd09 (b: boolean);
    procedure setd10 (b: boolean);
    procedure setd11 (b: boolean);
    procedure setd12 (b: boolean);
    procedure setd13 (b: boolean);
    procedure setd14 (b: boolean);
    procedure setd15 (b: boolean);

  protected
  public
    constructor create(aowner: tcomponent); override;
    destructor destroy; override;

    // refresh stuff
    procedure refresh_a;
    procedure refresh_d;

  published
    property version: string read fversion;
    property lasterror: string read flasterror;
    property onerror: tlabjackerrorevent read fonerror write fonerror;

    // labjack
{    property gain: tlabjackgain read fgain write setgain;}
    property mode_d: tlabjackmode_D read fmode_d write setmode_d;
    property mode_io: tlabjackmode_IO read fmode_io write setmode_io;

    property ai00: single read fai[0];
    property ai01: single read fai[1];
    property ai02: single read fai[2];
    property ai03: single read fai[3];
    property ai04: single read fai[4];
    property ai05: single read fai[5];
    property ai06: single read fai[6];
    property ai07: single read fai[7];
    property ao00: single read fao[0] write setao00;
    property ao01: single read fao[1] write setao01;

    property io00: boolean read fd_io[0] write setio00;
    property io01: boolean read fd_io[1] write setio01;
    property io02: boolean read fd_io[2] write setio02;
    property io03: boolean read fd_io[3] write setio03;

    property d00: boolean read fd_d[0] write setd00;
    property d01: boolean read fd_d[1] write setd01;
    property d02: boolean read fd_d[2] write setd02;
    property d03: boolean read fd_d[3] write setd03;
    property d04: boolean read fd_d[4] write setd04;
    property d05: boolean read fd_d[5] write setd05;
    property d06: boolean read fd_d[6] write setd06;
    property d07: boolean read fd_d[7] write setd07;
    property d08: boolean read fd_d[8] write setd08;
    property d09: boolean read fd_d[9] write setd09;
    property d10: boolean read fd_d[10] write setd10;
    property d11: boolean read fd_d[11] write setd11;
    property d12: boolean read fd_d[12] write setd12;
    property d13: boolean read fd_d[13] write setd13;
    property d14: boolean read fd_d[14] write setd14;
    property d15: boolean read fd_d[15] write setd15;
  end;

procedure Register;

implementation

uses
  u_labjack;

const
  VersionInformation: string = 'r1.00/ari.pikivirta@kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_labjack.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninformation;
//  fgain:=_20to20;
  fmode_io:=_4i;
  fmode_d:=_16i;
  flasterror:='';
end;

//------------------------------------------------------------------------------
destructor TAPI_labjack.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_labjack.setmode_d(mode: tlabjackmode_d);
begin
  fmode_d:=mode;
end;

//------------------------------------------------------------------------------
function TAPI_labjack.parsemode_d(mode: tlabjackmode_d): integer;
begin
  case mode of
    _16i: result:=0;        // 0000 0000 0000 0000
    _12i_4q: result:=15;    // 0000 0000 0000 1111
    _8i_8q: result:=255;    // 0000 0000 1111 1111
    _4i_12q: result:=4095;  // 0000 1111 1111 1111
    _16q: result:=65535;    // 1111 1111 1111 1111
    else result:=0;
  end
end;

//------------------------------------------------------------------------------
procedure TAPI_labjack.setmode_io(mode: tlabjackmode_io);
begin
  fmode_io:=mode;
end;

//------------------------------------------------------------------------------
function TAPI_labjack.parsemode_io(mode: tlabjackmode_io): integer;
begin
  case mode of
    _4i: result:=0;         // 0000
    _3i_1q: result:=1;      // 0001
    _2i_2q: result:=3;      // 0011
    _1i_3q: result:=7;      // 0111
    _4q: result:=15;        // 1111
    else
    begin
      flasterror:='Failed to parse io.';
      if assigned(fonerror) then
        fonerror(self, flasterror);
      result:=0;
    end;
  end
end;

{
//------------------------------------------------------------------------------
procedure TAPI_labjack.setgain(gain: tlabjackgain);
begin
  fgain:=fgain;
end;
}

//------------------------------------------------------------------------------
procedure TAPI_labjack.setao00(value: single);
begin
  fao[0]:=value;
end;

procedure TAPI_labjack.setao01(value: single);
begin
  fao[1]:=value;
end;

//------------------------------------------------------------------------------
procedure TAPI_labjack.setio00(b: boolean);
begin
  fd_io[0]:=b;
end;

procedure TAPI_labjack.setio01(b: boolean);
begin
  fd_io[1]:=b;
end;

procedure TAPI_labjack.setio02(b: boolean);
begin
  fd_io[2]:=b;
end;

procedure TAPI_labjack.setio03(b: boolean);
begin
  fd_io[3]:=b;
end;

//------------------------------------------------------------------------------
procedure TAPI_labjack.setd00(b: boolean);
begin
  fd_d[0]:=b;
end;

procedure TAPI_labjack.setd01(b: boolean);
begin
  fd_d[1]:=b;
end;

procedure TAPI_labjack.setd02(b: boolean);
begin
  fd_d[2]:=b;
end;

procedure TAPI_labjack.setd03(b: boolean);
begin
  fd_d[3]:=b;
end;

procedure TAPI_labjack.setd04(b: boolean);
begin
  fd_d[4]:=b;
end;

procedure TAPI_labjack.setd05(b: boolean);
begin
  fd_d[5]:=b;
end;

procedure TAPI_labjack.setd06(b: boolean);
begin
  fd_d[6]:=b;
end;

procedure TAPI_labjack.setd07(b: boolean);
begin
  fd_d[7]:=b;
end;

procedure TAPI_labjack.setd08(b: boolean);
begin
  fd_d[8]:=b;
end;

procedure TAPI_labjack.setd09(b: boolean);
begin
  fd_d[9]:=b;
end;

procedure TAPI_labjack.setd10(b: boolean);
begin
  fd_d[10]:=b;
end;

procedure TAPI_labjack.setd11(b: boolean);
begin
  fd_d[11]:=b;
end;

procedure TAPI_labjack.setd12(b: boolean);
begin
  fd_d[12]:=b;
end;

procedure TAPI_labjack.setd13(b: boolean);
begin
  fd_d[13]:=b;
end;

procedure TAPI_labjack.setd14(b: boolean);
begin
  fd_d[14]:=b;
end;

procedure TAPI_labjack.setd15(b: boolean);
begin
  fd_d[15]:=b;
end;

{
//------------------------------------------------------------------------------
function TAPI_labjack.parsegain(gain: tlabjackgain): integer;
begin
  case gain of
    _20to20: result:=1;
    _10to10: result:=2;
    _5to5: result:=3;
    _4to4: result:=4;
    _2_5to2_5: result:=5;
    _2to2: result:=8;
    _1_25to1_25: result:=10;
    _1to1: result:=20;
    else
    begin
      flasterror:='failed to parse gain.';
      result:=0;
    end;
  end
end;
}

//------------------------------------------------------------------------------
// ANALOG CHANNELS
procedure TAPI_labjack.refresh_a;
var
  retvalue: integer;
  idnum: integer;
  channel: integer;
  demo: integer;
  gain: integer;
  overvoltage: integer;
  analogout0: single;
  analogout1: single;
  voltage: single;
begin
  idnum:=-1;
  demo:=0;
  gain:=0; //parsegain(fgain);
  overvoltage:=0;
  voltage:=0;

  // read analog inputs
  for channel:=0 to 7 do
  begin
    retvalue:=EAnalogin(idnum,demo,channel,gain,overvoltage,voltage);

    if retvalue>0 then
    begin
      flasterror:='failed to read analog ch'+inttostr(channel)+' (err'+inttostr(retvalue)+')';
      if assigned(fonerror) then
        fonerror(self, flasterror);
    end else
    begin
      fai[channel]:=voltage;
    end;
  end;

  // write analog outputs
  analogout0:= fao[0];
  analogout1:= fao[1];
  retvalue:=eanalogout(idnum, demo, analogout0, analogout1);

  if retvalue>0 then
  begin
    flasterror:='failed to write analog channel (err'+inttostr(retvalue)+')';
    if assigned(fonerror) then
        fonerror(self, flasterror);
  end;
end;

//------------------------------------------------------------------------------
// DIGITAL CHANNELS
procedure TAPI_labjack.refresh_d;
var
  i: integer;
  idnum: integer;
  demo: integer;
  trisd: integer;
  trisio: integer;
  stated: integer;
  stateio: integer;
  updatedigital: integer;
  outputd: integer;
  retvalue: integer;

  procedure setbit (var fi: integer; bitnr:byte; state: boolean);
  begin
    if state then
      fi:=fi or (1 shl Bitnr) else
      fi:=fi and ($FFFFFFFF xor (1 shl Bitnr));
  end;

  function bitisset (fo: integer; bitnr:byte): boolean;
  begin
    result:=(fo and (1 shl Bitnr))<>0;
  end;

begin
  // set default values
  idnum:=-1;
  demo:=0;
  outputd:=0;
  stated:=0;
  updatedigital:=1;
  stateio:=0;
  trisd:=parsemode_d(fmode_d);
  trisio:=parsemode_io(fmode_io);

  // setup the output buffer
  for i:=0 to 15 do
  begin
    if i<4 then setbit(stateio, i, fd_io[i]);
    setbit(stated, i, fd_d[i]);
  end;

  // do the io stuff
  retvalue:=digitalio(idnum,demo,trisd,trisio,stated,stateio,updatedigital,outputd);

  if retvalue>0 then
  begin
    // error #retvalue occured
    if assigned(fonerror) then
        fonerror(self, 'Error reading or writing digital io.');
  end else
  begin
    // parse input states
    for i:=0 to 15 do
    begin
      if i<4 then fd_io[i]:=bitisset(stateio,i);
      fd_d[i]:=bitisset(stated,i);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('API', [TAPI_labjack]);
end;

end.
