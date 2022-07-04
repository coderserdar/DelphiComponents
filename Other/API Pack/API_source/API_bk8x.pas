unit API_bk8x;

//------------------------------------------------------------------------------
// API_bk8x component is component to easy up communicating with beckhoff
// BK8x00 bus couplers.
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
// r1.06, 12032008, ari pikivirta
//  * added threadsafe input & output buffers to the class for easier usage
//
// r1.05, 11022008, ari pikivirta
//  * added retry counter for the communication problems, reset on succesfull cycle
//  * added exception on thread event error
//
// r1.04, 28012008, ari pikivirta
//  * added expectedreadlength property
//  * added real read length + 1 into the normal read length expectation
//
// r1.03, 08102007, ari pikivirta
//  * added onopen and onclose events
//
// r1.02, 14082007, ari pikivirta
//  * added property to adjust readtimeout from the default 1s
//
// r1.01, 26072007, ari pikivirta
//  * added save and open setting functions
//
// r1.00, 23072007, ari pikivirta
//  * using API_232rw component/class for the communication
//  * self adjusting expected readlength (makes reading much faster)
//  * integrated thread for reading and writing io
//  * integrated cycletime calculation send as event parameter 
//
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, API_base, API_timer, syncobjs;

type
  // io buffer
  TBuffer = array[0..256] of byte;

  // events
  TAPI_BK8xThreadEvent = procedure(Sender: TThread; CycleMSec: double;
    Inputs: TBuffer; var Outputs: TBuffer) of object;
  TAPI_BK8xThreadError = procedure(Sender: TThread; ErrorMsg: string) of object;

  // thread
  TAPI_BK8xThread = class(TThread)
  private
    fevent: TAPI_BK8xThreadEvent;
    ferror: TAPI_BK8xThreadError;
    finterval: double;
    fexpectread: integer;
    fforceexpectread: integer;
    ftimer: TAPI_timer;
    fin, fout: tbuffer;
    ferrstr: string;
    fretrycount: integer;
    fretrycounter: integer;
    function ReadInputs: boolean;
    function WriteOutputs: boolean;
    procedure Event;
    procedure Error(Custom: string = '');
    procedure DoError;
  protected
    procedure Execute; override;
  end;

  // component
  TAPI_bk8x = class(TAPI_Custom_Component)
  private
    fthread:  TAPI_bk8xThread;
    fpriority: tthreadpriority;
    fevent:   TAPI_bk8xthreadevent;
    ferror:   TAPI_BK8xThreadError;
    fstart:   tdatetime;
    fonopen:  tnotifyevent;
    fonclose: tnotifyevent;
    fretrycount: integer;
    function  getport: integer;
    procedure setport(i: integer);
    function  getbaudrate: integer;
    procedure setbaudrate(i: integer);
    function  getopen: boolean;
    procedure setopen(b: boolean);
    function  getaddress: integer;
    procedure setaddress(i: integer);
    function  getmessageident: integer;
    procedure setmessageident(i: integer);
    function  getwritewords: integer;
    procedure setwritewords(i: integer);
    function  getreadtimeout: integer;
    procedure setreadtimeout(i: integer);
    procedure Error(Custom: string = '');
    function  getexpectedreadlength: integer;
    procedure setexpectedreadlength(i: integer);
  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    function  BytesRead: int64;
    function  BytesSent: int64;
    function  ReadQueue: int64;
    function  SendQueue: int64;
    function  StartTime: tdatetime;
    function  Inputs: tbuffer;
    procedure Outputs(outputs: tbuffer); overload;
    function  Outputs: tbuffer; overload;
    procedure SetBit (var OfByte: byte; BitIndex:byte; State: boolean);
    function  BitIsSet (OfByte, BitIndex:byte): boolean;
    function  SaveSettings(FileName: string; UseComponentName: boolean = true): boolean;
    function  OpenSettings(FileName: string; UseComponentName: boolean = true): boolean;
  published
    property Port: integer read getport write setport;
    property Baudrate: integer read getbaudrate write setbaudrate;
    property Open: boolean read getopen write setopen;
    property Address: integer read getaddress write setaddress;
    property MessageIdent: integer read getmessageident write setmessageident;
    property WriteLength: integer read getwritewords write setwritewords;
    property ThreadEvent: TAPI_BK8xThreadEvent read fevent write fevent;
    property ThreadPriority: tthreadpriority read fpriority write fpriority;
    property ThreadError: TAPI_BK8xThreadError read ferror write ferror;
    property ReadTimeout: integer read getreadtimeout write setreadtimeout;
    property OnClose: TNotifyEvent read fonclose write fonclose;
    property OnOpen: TNotifyEvent read fonopen write fonopen;
    property RetryCount: integer read fretrycount write fretrycount;
    property ExpectedReadLength: integer read getexpectedreadlength write setexpectedreadlength;
  end;

procedure Register;

implementation

{$R *.RES}

uses
  windows, api_232rw, math, inifiles;

type
  TAPI_BK8xData = record
    lock: tcriticalsection;
    address: integer;
    messageident: integer;
    writewords: integer;
    expectedreadlength: integer;
  end;

  // io
  TThreadIO = record
    lock: tcriticalsection;
    inputs: tbuffer;
    outputs: tbuffer;
    ctime: double;
  end;

var
  Data: TAPI_BK8xData;
  ThreadIO: TThreadIo;

//------------------------------------------------------------------------------
procedure TAPI_bk8x.SetBit (var OfByte: byte; BitIndex:byte; State: boolean);
begin
  api_base.setbit( ofbyte, bitindex, state );
end;

//------------------------------------------------------------------------------
function TAPI_bk8x.BitIsSet (OfByte, BitIndex:byte): boolean;
begin
  result:= api_base.bitisset( ofbyte, bitindex );
end;

//------------------------------------------------------------------------------
function Bk8xInputs: tbuffer;
begin
  threadio.lock.acquire;
  try
    result:= threadio.inputs;
  finally
    threadio.lock.release;
  end;
end;

procedure Bk8xOutputs(outputs: tbuffer); overload;
begin
  threadio.lock.acquire;
  try
    threadio.outputs:= outputs;
  finally
    threadio.lock.release;
  end;
end;

function Bk8xOutputs: tbuffer; overload;
begin
  threadio.lock.acquire;
  try
    result:= threadio.outputs;
  finally
    threadio.lock.release;
  end;
end;

//------------------------------------------------------------------------------
constructor TAPI_bk8x.Create(aowner: tcomponent);
begin
  inherited Create(aowner);
  version:= 'r1.06/ari.pikivirta@kolumbus.fi';

  // critical section
  data.lock:= tcriticalsection.Create;
  threadio.lock:= tcriticalsection.create;

  // init bk8x variables
  address:= 11;
  messageident:= 1;
  writelength:= 0;
  threadpriority:= tpnormal;
  expectedreadlength:= 0;
  retrycount:= 5;

  // initialize com variables
  com_init;
  //com_setdefaults;
  com_setport(0);
  com_setbaudrate(38400);
  com_setnomessage(true);
  com_setreadtimeout(1000);
  com_setbytesize(8);
  com_setparity(EVENPARITY);
  com_setstopbits(ONESTOPBIT);
end;

destructor TAPI_bk8x.Destroy;
begin
  open:= false;
  sleep(200);
  com_kill;

  threadio.lock.free;
  data.lock.Free;
  inherited Destroy;
end;

function TAPI_bk8x.SaveSettings(FileName: string; UseComponentName: boolean = true): boolean;
var
  ini: tinifile;
  section: string;
begin
  ini:= tinifile.create(filename);
  try
    if UseComponentName then section:= self.name else section:= 'BK8x';
    ini.WriteInteger(section, 'port', port);
    ini.writeinteger(section, 'baudrate', baudrate);
    ini.WriteInteger(section, 'address', address);
    ini.writeinteger(section, 'messageident', messageident);
    ini.writeinteger(section, 'writelength', writelength);
    ini.writeinteger(section, 'priority', integer(threadpriority));
    ini.writeinteger(section, 'retrycount', retrycount);
    result:= true;
  finally
    ini.free;
  end;
end;

function TAPI_bk8x.OpenSettings(FileName: string; UseComponentName: boolean = true): boolean;
var
  ini: tinifile;
  section: string;
begin
  result:= falsE;
  if fileexists(Filename) then
  begin
    ini:= tinifile.create(filename);
    try
      if UseComponentName then section:= self.name else section:= 'BK8x';
      port:= ini.ReadInteger(section, 'port', 0);
      baudrate:= ini.readinteger(section, 'baudrate', 38400);
      address:= ini.readInteger(section, 'address', 11);
      messageident:= ini.readinteger(section, 'messageident', 1);
      writelength:= ini.readinteger(section, 'writelength', 0);
      threadpriority:= tthreadpriority(ini.readinteger(section, 'priority', integer(tpnormal)));
      retrycount:= ini.readinteger(section, 'retrycount', 5);
      result:= true;
    finally
      ini.free;
    end;
  end;
end;

function TAPI_bk8x.getport: integer;
begin
  result:= com_getport;
end;

procedure TAPI_bk8x.setport(i: integer);
begin
  com_setport(i);
end;

function TAPI_bk8x.getbaudrate: integer;
begin
  result:= com_getbaudrate;
end;

procedure TAPI_bk8x.setbaudrate(i: integer);
begin
  com_setbaudrate(i);
end;

function TAPI_bk8x.Inputs: tbuffer;
begin
  result:= Bk8xInputs;
end;

procedure TAPI_bk8x.Outputs(outputs: tbuffer);
begin
  Bk8xOutputs(outputs);
end;

function TAPI_bk8x.Outputs: tbuffer;
begin
  result:= Bk8xOutputs;
end;

function TAPI_bk8x.getopen: boolean;
begin
  result:= com_isopen;
end;

function TAPI_bk8x.BytesRead: int64;
begin
  result:= com_readbytes;
end;

function TAPI_bk8x.BytesSent: int64;
begin
  result:= com_sentbytes;
end;

function TAPI_bk8x.ReadQueue: int64;
begin
  result:= com_getinputque;
end;

function TAPI_bk8x.SendQueue: int64;
begin
  result:= com_getoutputque;
end;

function TAPI_bk8x.getreadtimeout: integer;
begin
  result:= com_getreadtimeout;
end;

procedure TAPI_bk8x.setreadtimeout(i: integer);
begin
  com_setreadtimeout(i);
end;

procedure TAPI_bk8x.setopen(b: boolean);
begin
  if (b) and (not com_isopen) and (not (csDesigning in ComponentState)) then
  begin
    // test opening port
    if com_open then
    begin
      com_clear;
      com_close;
    end else
    begin
      Error('Failed to open port');
      exit;
    end;
    // open port and
    // start io thread
    if com_open then
    begin
      fthread:= TAPI_BK8xThread.Create(true);
      fthread.Priority:= fpriority;
      fthread.fevent:= fevent;
      fthread.ferror:= ferror;
      fthread.fforceexpectread:= expectedreadlength;
      fthread.fretrycount:= fretrycount;
      fthread.FreeOnTerminate:= true;
      fthread.Resume;
      fstart:= now;
      if assigned(fonopen) then
        fonopen(self);      
    end else
      // failed to opoen
      Error('');
  end else

  if com_isopen then
  begin
    // end io thread
    fthread.Terminate;
    // close com port
    if not com_close then Error('');
    fstart:= 0;
    if assigned(fonclose) then
      fonclose(self);
  end;
end;

function getaddress: integer;
begin
  data.lock.Acquire;
  try
    result:= data.address;
  finally
    data.lock.Release;
  end;
end;

function TAPI_bk8x.getaddress: integer;
begin
  result:= api_bk8x.getaddress;
end;

procedure TAPI_bk8x.setaddress(i: integer);
begin
  if (i<0) or (i>99) then exit;
  data.lock.Acquire;
  try
    data.address:= i;
  finally
    data.lock.Release;
  end;
end;

function  TAPI_bk8x.getexpectedreadlength: integer;
begin
  data.lock.acquire;
  try
    result:= data.expectedreadlength;
  finally
    data.lock.release;
  end;
end;

procedure TAPI_bk8x.setexpectedreadlength(i: integer);
begin
  if i<0 then exit;
  data.lock.acquire;
  try
    data.expectedreadlength:= i;
  finally
    data.lock.release;
  end;
end;

function getmessageident: integer;
begin
  data.lock.Acquire;
  try
    result:= data.messageident;
  finally
    data.lock.Release;
  end;
end;

function TAPI_bk8x.getmessageident: integer;
begin
  result:= API_bk8x.getmessageident;
end;

procedure TAPI_bk8x.setmessageident(i: integer);
begin
  if (i<0) or (i>255) then exit;
  data.lock.Acquire;
  try
    data.messageident:= i;
  finally
    data.lock.Release;
  end;
end;

function getwritewords: integer;
begin
  data.lock.Acquire;
  try
    result:= data.writewords;
  finally
    data.lock.Release;
  end;
end;

function TAPI_bk8x.getwritewords: integer;
begin
  result:= api_bk8x.getwritewords;
end;

procedure TAPI_bk8x.setwritewords(i: integer);
begin
  if (i<0) or (i>128) then exit;
  data.lock.Acquire;
  try
    data.writewords:= i;
  finally
    data.lock.Release;
  end;
end;

function TAPI_bk8x.StartTime: tdatetime;
begin
  result:= fstart;
end;

// next is a bit difficult to explain, but i'll try..
// basically everything on this component is from inside
// the thread, and also most errors are risen by the
// the thread -> the main component will need to rise
// some errors as well, but to not to have different
// event for that i'll use exactly same looking event
// (same event, but not assigned to the thread yet)
procedure TAPI_bk8x.Error(Custom: string = '');
begin
  if assigned(ferror) then
    if custom<>'' then ferror(nil, custom)
      else if com_lasterror<>'' then ferror(nil, com_lasterror);
end;

//------------------------------------------------------------------------------

procedure TAPI_BK8xThread.Execute;
begin
  ftimer:= tapi_timer.Create(nil);
  try
    ftimer.Start;                                     // start api_timer
    finterval:= 0;
    fexpectread:= 256;                                // wait fur full buffer
    fretrycounter:= fretrycount;                      // reset retry counter
    com_clear;                                        // clear com buffers
    sleep(10);                                        // wait 10ms

    while not terminated do                           // while thread is terminated
    begin

      // thread io write
      threadio.lock.Acquire;
      try
        fout:= threadio.outputs;
      finally
        threadio.lock.release;
      end;

      // event and readwrite
      if (WriteOutputs) and (ReadInputs) then         // read & write io buffers
      begin
        Event;                                        // do event
        fretrycounter:= fretrycount;                  // reset retry counter
      end;

      // thread io update
      threadio.lock.Acquire;
      try
        threadio.outputs:= fout;
        threadio.inputs:= fin;
        threadio.ctime:= finterval;
      finally
        threadio.lock.release;
      end;

      // wait & cycle time
      finterval:= ftimer.InstervalMs;                 // check interval
      if finterval<1 then sleep(1);                   // sleep if below 1ms cycle

    end;

    ftimer.Stop;
  finally
    ftimer.free;
  end;
end;

function TAPI_BK8xThread.ReadInputs: boolean;
var
  buf: tcombuffer;
  s: string;
  b, inputwords, inputlength, checksum: byte;
  i, len: integer;
begin
  result:= false;
  if not com_isopen then exit;

  // expected read length
  if fforceexpectread>0 then fexpectread:= fforceexpectread;

  // read from serial port
  if not com_readbuf(buf, len, fexpectread) then
  begin
    Error('Read timeout');
    exit;
  end;

  // Start identifier                     0       ’p‘       (0x70)
  // Number of process data input words   1                 0 – 255
  // Message ident                        2                 0 –255
  // Multipoint Addresse                  3                 0 – 99
  // Status                               4                 0 – 255
  // Process data input LOW Byte          5 + 2 x n         0 – 255
  // Process data output HIGH Byte        6 + 2 x n         0 – 255
  // Checksum                             6 + 2 x n + 1     0 – 255

  if len<6 then
  begin
    Error('Response too short');
    exit;
  end;

  checksum:= 0;

  b:= buf[0];
  if b<>ord('p') then
  begin
    Error('Invalid reponse header ('+chr(b)+')');
    exit;
  end;
  inc(checksum, b);

  b:= buf[1];
  inputwords:= b;
  inputlength:= (inputwords * 2);
  inc(checksum, b);

  b:= buf[2];
  if getmessageident<>b then
  begin
    Error('Invalid response message ident ('+inttostr(b)+')');
    exit;
  end;
  inc(checksum, b);

  b:= buf[3];
  if getaddress<>b then
  begin
    // another option is that this message was
    // not meant for this component to catch :)
    // Error('Invalid response address ('+inttostr(b)+'/'+inttostr(getaddress)+')');
    // exit;
  end;
  inc(checksum, b);

  b:= buf[4];
  if b<>0 then
  begin
    s:= 'Status #'+inttostr(b);
    if (b and BIT0)>0 then s:= 'Terminal bus error';
    if (b and BIT1)>0 then s:= 'Configuration error';
    if (b and BIT3)>0 then s:= 'Invalid output length';
    Error(s);
    exit;
  end;
  inc(checksum, b);

  if len<6+inputlength then
  begin
    Error('Too short response ('+inttostr(len)+'/'+inttostr(6+inputlength)+')');
    exit;
  end;

  for i:=0 to inputlength-1 do
  begin
    b:= buf[5+i];                     // get next byte
    fin[i]:= b;                       // upadte input buffer
    inc(checksum, b);                 // add to checksum
  end;

  b:= buf[5+inputlength];
  if checksum<>b then
  begin
    Error('Checksum failed ('+inttostr(b)+'/'+inttostr(checksum)+')');
    exit;
  end;

  fexpectread:= len; //+1; // +1 added 28012008

  result:= true;
end;

function TAPI_BK8xThread.WriteOutputs: boolean;
var
  buf: tcombuffer;
  checksum, writelength: byte;
  i, count: integer;
begin
  result:= false;
  if not com_isopen then exit;

  // Start identifier                     0       ’P‘       (0x50)
  // Number of process data output words  1                 0 – 255
  // Message ident                        2                 0 – 255
  // Multipoint address                   3                 0 – 99
  // Process data output LOW Byte         4 + 2 x n         0 – 255
  // Process data output HIGH Byte        5 + 2 x n         0 – 255
  // Checksum                             6 + 2 x n + 1     0 – 255

  writelength:= getwritewords;
  count:= 4+(writelength*2)+1;

  // start with header
  buf[0]:= $50;
  buf[1]:= writelength;
  buf[2]:= getmessageident;
  buf[3]:= getaddress;
  for i:=0 to (writelength*2)-1 do
    buf[4+i]:= fout[i];

  // calculate checksum
  checksum:= 0;
  for i:=0 to count-2 do
    inc(checksum, buf[i]);
  buf[count-1]:= checksum;

  // write to serial buffer
  result:= com_writebuf(buf, count);
end;

procedure TAPI_BK8xThread.Event;
begin
  if assigned(fevent) then
  try
    fevent(self, finterval, fin, fout);
  except
    // ignore faults here
    Error('Exception on Thread Event');
  end;
end;

procedure TAPI_BK8xThread.Error(Custom: string = '');
var
  len: integer;
  buf: tcombuffer;
begin
  if fretrycounter>0 then
  begin
    fretrycounter:= fretrycounter - 1;                          // decrease retry counter
    com_readbuf(buf, len);
    com_clear;                                                  // reset buffers
  end else
  if assigned(ferror) then
  begin
    if custom<>'' then ferrstr:= custom
      else if com_lasterror<>'' then ferrstr:= com_lasterror;
    synchronize(doerror);                                       // synhronize error event
    fretrycounter:= fretrycount;                                // reset retry counter
  end;
end;

procedure TAPI_bk8xthread.DoError;
begin
  ferror(self, ferrstr);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_bk8x]);
end;

end.
