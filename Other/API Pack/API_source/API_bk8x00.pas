unit API_bk8x00;

//------------------------------------------------------------------------------
(*
   communication unit to handle beckhoff's bk8x00 bus couplers. with this unit
   you can use all available beckhoff modules with the coupler - component is
   provided as-is and author is not responsible of any damages you cause with
   the buscouplers attached with this component. however, i've done also
   commercial applications using this..
*)
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// TAPI_BK8x00
//
// TODO:
//  * find out why thread cycle time is much faster when ever delphi environment
//    is open (no matter which project is open inside that)
//
// 05092007, r1.27, ari pikivirta
//  * fixed ManageError not assigning the errorcode as it was supposed to
//
// 06072007, r1.26, ari pikivirta
//  * changed thread delay to be executed after the event to prevent error(s)
//    on stopping the thread and port
//  * made it again possible to set threaddelay to zero (might cause application
//    to hand if priority is set to timecritical and thread is not synchronized)
//  * added thread handle into the threadevent -> will cause need to reassing that event
//
// 04072007, r1.25, ari pikivirta
//  * manageerror procedure added (to simplify the code)
//  * added api_timer to calculate cycle time of the thread
//  * read/write functions include clearing of buffers internally, was before
//    only in combined function
//  * moved bitisset and setbit functions outside the component for easier access
//  * removed unnecessary properties for easier access
//  * fixed bug on clearing last error on re-open port
//
// 27062007, r1.24, ari pikivirta
//  * moved quer, quew to function instead of being property (not able to modify anyway)
//  * minimum thread cycle delay checking added into the thread
//  * removed dummyi procedure because not needed
//
// 22022007, r1.23, ari pikivirta
//  * added OpenRetrycount as property (default = 5)
//
// 13022007, r1.22, ari pikivirta
//  * fixed problem with invalid com file name (was not set at all)
//
// 24012007, r1.21, ari pikivirta
//  * changed serial port baudrate and port setting to real value property
//    to allow ports up to 255
//
// 21012007, r1.20, ari pikivirta
//  * if waitcycles is set to zero, the input queue max waiting time is ignored
//    note! this might cause infinite loop there if the input queue is never
//    filled up enough.
//  * changed waitcycles to work in 1ms period (was cpu cycle before)
//
// r1.19, ari pikivirta
// * added error text in errornitification
// r1.18, ari pikivirta
// * added possibility to not to synchronize thread with parent thread
// r1.17, ari pikivirta
// * fixed the bug of running without coupler was accepted..
// r1.16, ari pikivirta
// * added closeonerror property
// r1.15, ari pikivirta
// * added clearbuffers property for failed read and write
// r1.14, ari pikivirta
// * fixed the destroy method, now it closes the port correctly
// r1.13, ari pikivirta
// * added new events to thread after create and after terminate
// r1.12, ari pikivirta
// * fixed error event (removed txt string)
// * fixed onopen to afteropen event
// * added new property for automatically starting thread after port open
//   and terminating thread onport close
// * added new error message when trying to open port "none"
// r1.11, ari pikivirta
// * added status message string function
// * internal thread possibility added (synchronized)
// r1.10, ari pikivirta
// * fixed error events
// 1.09, ari pikivirta
// * added some events..
// 1.08, ari pikivirta
// * removed input length from settings window
// 1.07, ari pikivirta
// * changed write and read to readwrite function
// * moved refreshques to private
// * added designing state regognition
// * added bitisset and setbit functions
// * added clear error function
// 1.06, ari pikivirta
// * input length doesn't matter now on, it's compared,
//   but also corrected while reading coupler info
// * improved reading message - should be faster
//   now, because there is 2 wait stages now (4 bytes + rest)
// * error processing changed to go through numbering
// 1.05, ari pikivirta
// * fixed input and output length calculation in message creation
// 1.04, ari pikivirta
// * fixed the setup window save
// 1.03, ari pikivirta
// * added show, open and save settings
// 1.02, ari pikivirta
// * input length added (as a messagewait)
// * bug in checksum calculation fixed
// * bug with pcomstat (tx and rx) ques fixed
//

interface

{$WARN UNSAFE_TYPE ON} {$WARN UNSAFE_CODE ON} {$WARN UNSAFE_CAST ON}

uses
  Windows, SysUtils, Classes, SyncObjs, API_timer;

const

  bk8x00_statusmessage: array [0..7] of string = (
    'Status 00: Terminal bus error: an error has occurred in data communication with the terminals',
    'Status 01: Configuration error: see occur codes 1 and 2 (leds)',
    'Status 02: --',
    'Status 03: --',
    'Status 04: Invalid process data output length: the received number of process output words is unequal to the physically existing data length on the K bus.',
    'Status 05: --',
    'Status 06: --',
    'Status 07: --');

  maxerrorcount = 47;

  bk8x00_errormessage: array [0..maxerrorcount-1] of string = (
    'No errors.',
    'Err01: Cannot change port while port is open.',
    'Err02: Cannot change baudrate while port is open.',
    'Err03: Cannot change parity while port is open.',
    'Err04: Cannot change databits while port is open.',
    'Err05: Cannot change stopbits while port is open.',
    'Err06: Cannot change rx buffer size while port is open.',
    'Err07: Cannot change tx buffer size while port is open.',
    'Err08: Cannot change rx timeout while port is open.',
    'Err09: Cannot change tx timeout while port is open.',
    'Err10: Port is already open.',
    'Err11: Unable to locate selected com port.',
    'Err12: Failed to create com file.',
    'Err13: Failed to setup comm buffers.',
    'Err14: Failed to get comm state.',
    'Err15: Failed to set comm state.',
    'Err16: Failed to set comm timeouts.',
    'Err17: Port is not open.',
    'Err18: Port is not open (getrxdize).',
    'Err19: The hardware detected a break condition.',
    'Err20: A parallel device is not selected.',
    'Err21: The hardware detected a framing error.',
    'Err22: An I/O error occurred during communications with the device.',
    'Err23: The requested mode is not supported, or the hFile parameter is invalid. If this value is specified, it is the only valid error.',
    'Err24: A parallel device signaled that it is out of paper.',
    'Err25: A character-buffer overrun has occurred. The next character is lost.',
    'Err26: A time-out occurred on a parallel device.',
    'Err27: An input buffer overflow has occurred. There is either no room in the input buffer, or a character was received after the end-of-file (EOF) character.',
    'Err28: The hardware detected a parity error.',
    'Err29: The application tried to transmit a character, but the output buffer was full.',
    'Err30: Failed to write string to port.',
    'Err31: Waiting input buffer max cycles reached.',
    'Err32: Failed to read port.',
    'Err33: Input message header failed.',
    'Err34: Input message ident differs from last sent message.',
    'Err35: Coupler returned wrong bus address.',
    'Err36: Read checksum failed.',
    'Err37: Failed to save settings.',
    'Err38: Failed to open settings.',
    'Err39: Coupler status differs from zero.',
    'Err40: Thread is already assigned.',
    'Err41: Thread is not assigned.',
    'Err42: Cannot use readwrite while thread assigned.',
    'Err43: Cannot open port while set to none.',
    'Err44: Failed to start thread automatically.',
    'Err45: Failed to terminate thread automatically.',
    'Err46: Invalid baudrate setting.'
    );

type

  TBk8x00ErrorEvent = procedure (Sender: TObject; ErrNo: integer; ErrorMsg: string) of object;
  Tparity = (pNone, pOdd, pEven, pMark);
  Tdatabits = (dbFour, dbFive, dbSix, dbSeven, dbEight);
  Tstopbits = (sbOne, sbOnePointFive, sbTwo);
  TBuffer = array[0..256] of byte;
  bk8x00threadevent = procedure (thread: tthread) of object;
  bk8x00threadtime = procedure (intervalMS: double) of object;

  TAPI_bk8x00thread = class(TThread)
  private
    fdelay: integer;                  // cycle interval, 1ms minimum
    factive: boolean;                 // thread status
    fevent: bk8x00threadevent;        // event to fire every cycle
    fthreadtime: bk8x00threadtime;    // event to update time
    fsynchronized: boolean;           // run synchronized - flag
    ftimer: tapi_timer;               // cycle timer
    procedure ThreadEvent;            // thread event
    procedure UpdateTime;             // thread event (synchronized always)
  protected
    procedure Execute; override;
  public
    constructor Create(Suspended: Boolean);
    destructor Destroy; override;
  end;

  // main bk8x00 control component
  TAPI_bk8x00 = class(TComponent)
  private
    fversion: string;                 // component revision information
    fstarttime: tdatetime;            // online time

    // errors
    ferrorevent: tbk8x00errorevent;   // error event to fire on error
    flasterror: byte;                 // last error code occured
    fcloseonerror: boolean;           // shutdown thread on error

    // events
    fafteropen: tnotifyevent;         // after port open event
    fonclose: tnotifyevent;           // before close port event

    // bk8x00
    fbusaddress: byte;                // bus address of the coupler
    fmessageident: byte;              // message number sent to the coupler
    finputlength: byte;               // input image length (words)
    finputwaitcycles: word;           // read timeout (1ms)
    foutputlength: byte;              // output image length (words = 2 bytes)
    fstatus: byte;                    // coupler status reported on read

    // serial port
    fopen: boolean;                   // serial port open flag
    fopenretries: integer;            // how many retries on opening port
    fhandle: thandle;                 // serial file handle
    fport: byte;                      // port number to be opened
    fbaudrate: integer;               // serial port baudrate (38400 = bk8x000 default)
    fparity: tparity;                 // even parity by default
    fdatabits: tdatabits;             // 8 databits by default
    fstopbits: tstopbits;             // 1 stopbit by default
    finbuffer: integer;               // input buffer
    foutbuffer: integer;              // output buffer
    freadtimeout: cardinal;           // serieal port read timeout
    fwritetimeout: cardinal;          // serial port write timeout
    finque: integer;                  // input queue size
    foutque: integer;                 // output queue size
    fclearbuffersonfailedread: boolean;   //
    fclearbuffersonfailedwrite: boolean;  //

    // thread
    fthread: tapi_bk8x00thread;       // thread class if thread is used
    fthreadactive: boolean;           // thread status
    fthreadautoactivate: boolean;     // automatically activate thread on port open?
    fthreaddelay: integer;            // internal thread delay
    fthreadevent: bk8x00threadevent;  // event to fire on thread cycle
    fthreadpriority: tthreadpriority; // thread priority (normal priority table)
    fafterthreadcreate: tnotifyevent; //
    fafterthreadterminate: tnotifyevent;  //
    fthreadsynchronized: boolean;     // run event synchronized?
    fthreadcycletime: double;         // temporary from the thread

    procedure setthreadactive(b: boolean);

    // dummies
    procedure dummys(s: string);
    procedure dummyb(b: byte);

    // bk8x00
    function  getbusaddress: byte;
    procedure setbusaddress(b: byte);
    function  getmessageident: byte;
    procedure setmessageident(b: byte);
    function  getoutputlength: byte;
    procedure setoutputlength(b: byte);
    function  getinputlength: byte;
    procedure setinputlength(b: byte);
    function  getinputwaitcycles: word;
    procedure setinputwaitcycles(w: word);

    // serial port
    procedure setopen(b: boolean);
    procedure setport(p: byte);
    procedure setbaudrate(b: integer);

    // internal
    function  _open: boolean;
    procedure _refreshques;
    function  _read( var inp: tbuffer ): boolean;
    function  _write( outp: tbuffer ): boolean;
    function  _close: boolean;
    procedure ThreadTimeEvent( d: double );
    procedure ManageError(newerrorcode: integer);

  protected
  public
    constructor Create (aowner: tcomponent); override;
    destructor  Destroy; override;

    // readwrite
    function  StartTime: tdatetime;
    function  ReadWrite ( var inp: tbuffer; outp: tbuffer ): boolean;

    // settings related
    procedure ShowSettings;
    function  SaveSettings( fname: string ): boolean;
    function  OpenSettings( fname: string ): boolean;

    // status
    function  GetStatus: string;
    function  GetError( errno: byte ): string;
    procedure ClearError;

    // port
    procedure ClearReadBuffer;
    procedure ClearWriteBuffer;
    procedure ClearBuffers;

    // thread
    function  ThreadTime: double;

    // miscellous
    procedure SetBit (var fbyte: byte; bitnr:byte; state: boolean);
    function  BitIsSet (fbyte, bitnr:byte): boolean;

  published
    // common
    property Version: string read fversion write dummys stored false;

    // errors
    property LastError: byte read flasterror write dummyb;
    property OnError: tbk8x00errorevent read ferrorevent write ferrorevent;
    property CloseOnError: boolean read fcloseonerror write fcloseonerror;

    // bk8x00
    property BusAddress: byte read getbusaddress write setbusaddress;
    property MessageIdent: byte read getmessageident write setmessageident;
    property OutputLength: byte read getoutputlength write setoutputlength;
    property InputLength: byte read getinputlength write setinputlength;
    property InputWaitCycles: word read getinputwaitcycles write setinputwaitcycles;

    // serial port
    property Open: boolean read fopen write setopen;
    property OpenRetries: integer read fopenretries write fopenretries;
    property AfterOpen: tnotifyevent read fafteropen write fafteropen;
    property OnClose: tnotifyevent read fonclose write fonclose;
    property ClearBuffersOnFailedRead: boolean read fclearbuffersonfailedread write fclearbuffersonfailedread;
    property ClearBuffersOnFailedWrite: boolean read fclearbuffersonfailedwrite write fclearbuffersonfailedwrite;

    property Port: byte read fport write setport;
    property Baudrate: integer read fbaudrate write setbaudrate;

    // thread
    property ThreadEvent: bk8x00threadevent read fthreadevent write fthreadevent;
    property ThreadDelay: integer read fthreaddelay write fthreaddelay;
    property ThreadPriority: tthreadpriority read fthreadpriority write fthreadpriority;
    property ThreadActive: boolean read fthreadactive write setthreadactive;
    property TheaadAutoActivate: boolean read fthreadautoactivate write fthreadautoactivate;
    property AfterThreadCreate: tnotifyevent read fafterthreadcreate write fafterthreadcreate;
    property AfterThreadTerminate: tnotifyevent read fafterthreadterminate write fafterthreadterminate;
    property ThreadSynchronized: boolean read fthreadsynchronized write fthreadsynchronized;

  end;

//------------------------------------------------------------------------------
// common functions
procedure SetBit (var fbyte: byte; bitnr:byte; state: boolean);
function BitIsSet (fbyte, bitnr:byte): boolean;

procedure Register;

implementation

uses
  u_bk8x00_settings;

{$WARN UNSAFE_CODE OFF}
{$R *.RES}

const
  versioninfo_bk8x00: string = 'r1.27/ari.pikivirta(at)kolumbus.fi';

var
  bk8x00ComStat: ComStat;

//------------------------------------------------------------------------------
procedure SetBit (var fbyte: byte; bitnr:byte; state: boolean);
begin
  if state then fbyte:=fbyte or (1 shl Bitnr)
    else fbyte:=fbyte and ($FF xor (1 shl Bitnr));
end;

procedure TAPI_bk8x00.SetBit (var fbyte: byte; bitnr:byte; state: boolean);
begin
  api_bk8x00.setbit( fbyte, bitnr, state );
end;

//------------------------------------------------------------------------------
function BitIsSet (fbyte, bitnr:byte): boolean;
begin
  result:=(fbyte and (1 shl Bitnr))<>0;
end;

function TAPI_bk8x00.BitIsSet (fbyte, bitnr:byte): boolean;
begin
  result:= api_bk8x00.bitisset( fbyte, bitnr );
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.dummys(s: string);begin end;
procedure TAPI_bk8x00.dummyb(b: byte); begin end;

//------------------------------------------------------------------------------
constructor TAPI_bk8x00.create(aowner: tcomponent);
begin
  inherited create(aowner);

  // component
  fversion:=versioninfo_bk8x00;     // version info string
  fstarttime:=0;                    // clear start time
  flasterror:=0;                    // clear last error flag
  fcloseonerror:=true;              // close port on error

  // bk8x00
  fbusaddress:=11;                  // bus address selector on the coupler
  fmessageident:=1;                 // message identifier
  foutputlength:=0;                 // no output length defined
  finputlength:=0;                  // no imput length defined
  finputwaitcycles:=1000;           // default timeout to 1s
  fstatus:=0;                       // zero status (ok)

  // serial port
  fopen:=false;                     // serial not open
  fopenretries:= 2;                 // openretries to 2 by default
  fhandle:= INVALID_HANDLE_VALUE;   // no handle
  fport:= 0;                        // no port defined
  fbaudrate:= 38400;                // baudrate to 38,4k (default on coupler)
  fparity:= peven;                  // even parity
  fdatabits:= dbeight;              // 8 data bits
  fstopbits:= sbone;
  finbuffer:=512;                   // serial port input buffer
  foutbuffer:=512;
  freadtimeout:=500;               // 1s timeout on serial port
  fwritetimeout:=500;
  fclearbuffersonfailedread:=true;
  fclearbuffersonfailedwrite:=true;

  // thread
  fthreadactive:=false;
  fthreadpriority:=tpidle;          // thread priority default
  fthreaddelay:=2;                  // 2ms cycle
  fthreadsynchronized:=true;        // synchronized with the main thread
  fthreadcycletime:= 0;
end;

//------------------------------------------------------------------------------
destructor TAPI_bk8x00.destroy;
begin
  if fopen then setopen(false);         // close port on exit
  //flock.Free;                           // free critical section
  //threaddata.lock.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00.StartTime: tdatetime;
begin
  result:= fstarttime;
end;

//------------------------------------------------------------------------------
// results error message that corresponds the error code given
// to this function as parameter
function TAPI_bk8x00.geterror( errno: byte ): string;
begin
  if (errno<maxerrorcount) then result:=bk8x00_errormessage[errno]
    else result:='';
end;

//------------------------------------------------------------------------------
// results current status of bk8x00 bus coupler as string
function TAPI_bk8x00.getstatus: string;
begin
  result:='ok';
  if bitisset(fstatus,0) then result:=bk8x00_statusmessage[0];
  if bitisset(fstatus,1) then result:=bk8x00_statusmessage[1];
  if bitisset(fstatus,2) then result:=bk8x00_statusmessage[2];
  if bitisset(fstatus,3) then result:=bk8x00_statusmessage[3];
  if bitisset(fstatus,4) then result:=bk8x00_statusmessage[4];
  if bitisset(fstatus,5) then result:=bk8x00_statusmessage[5];
  if bitisset(fstatus,6) then result:=bk8x00_statusmessage[6];
  if bitisset(fstatus,7) then result:=bk8x00_statusmessage[7];
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setopen (b: boolean);
begin
  if b<>fopen then
  begin
    if fopen then
    begin
      if fthreadautoactivate then
        setthreadactive(false);
      _close;
    end else
    begin
      if _open then
        if fthreadautoactivate then
          setthreadactive(true);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setport (p: byte);
begin
  if p<>fport then
  begin
    if fopen then
    begin
      ManageError(1);
      exit;
    end;
    fport:=p;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setbaudrate (b: integer);
begin
  if b<>fbaudrate then
  begin
    if fopen then
    begin
      ManageError(2);
      exit;
    end;
    fbaudrate:=b;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setinputwaitcycles(w: word);
begin
  finputwaitcycles:=w;
end;

function TAPI_bk8x00.getinputwaitcycles: word;
begin
  result:= finputwaitcycles;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.ThreadTimeEvent( d: double );
begin
  fthreadcycletime:= d;
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00.ThreadTime: double;
begin
  result:= Fthreadcycletime;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.ClearReadBuffer;
begin
  if not open then exit;
  PurgeComm(fhandle, PURGE_RXABORT or PURGE_RXCLEAR);
end;

procedure TAPI_bk8x00.ClearWriteBuffer;
begin
  if not open then exit;
  PurgeComm(fhandle, PURGE_TXABORT or PURGE_TXCLEAR);
end;

procedure TAPI_bk8x00.ClearBuffers;
begin
  if not open then exit;
  clearreadbuffer;
  clearwritebuffer;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setbusaddress(b: byte);
begin
  fbusaddress:=b;
end;

function TAPI_bk8x00.getbusaddress: byte;
begin
  result:= fbusaddress;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setmessageident(b: byte);
begin
  fmessageident:=b;
end;

function TAPI_bk8x00.getmessageident: byte;
begin
  result:= fmessageident;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setoutputlength(b: byte);
begin
  foutputlength:=b;
end;

function TAPI_bk8x00.getoutputlength: byte;
begin
  result:= foutputlength;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setinputlength(b: byte);
begin
  finputlength:=b;
end;

function TAPI_bk8x00.getinputlength: byte;
begin
  result:= finputlength;
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00._open: boolean;
var
  commtimeouts: tcommtimeouts;
  retries: integer;
  dcb: _dcb;
  portstring: string;
begin
  result:=false;
  retries:= fopenretries;

  // get device name
  if fport<1 then
  begin
    ManageError(43);
    exit;
  end;

  // retry up to defined count
  while (retries>0) and (not fopen) do
  begin
    retries:= retries - 1;

    // close port if open (as re-open)
    if fhandle<>INVALID_HANDLE_VALUE then
      setopen(false); // close thread and port

    // open handle
    if fport>9 then portstring:= '\\.\'+'COM'+inttostr(fport)
      else portstring:= 'COM'+inttostr(fport);
    fhandle:= CreateFile(pchar(portstring), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    // result
    if (fhandle=invalid_handle_value) then
    begin
      ManageError(12);
      break;
    end;

    // set buffer sizes
    (*
    if not setupcomm(fhandle, foutbuffer, finbuffer) then
    begin
      ManageError(13);
      break;
    end;
    *)

    // get dcb struct
    if not getcommstate(fhandle, dcb) then
    begin
      ManageError(14);
      break;
    end;

    // set baudrate
    case (fbaudrate) of
      110:    dcb.BaudRate:= CBR_110;
      19200:  dcb.BaudRate:= CBR_19200;
      300:    dcb.BaudRate:= CBR_300;
      38400:  dcb.BaudRate:= CBR_38400;
      600:    dcb.BaudRate:= CBR_600;
      56000:  dcb.BaudRate:= CBR_56000;
      1200:   dcb.BaudRate:= CBR_1200;
      57600:  dcb.BaudRate:= CBR_57600;
      2400:   dcb.BaudRate:= CBR_2400;
      115200: dcb.BaudRate:= CBR_115200;
      4800:   dcb.BaudRate:= CBR_4800;
      128000: dcb.BaudRate:= CBR_128000;
      9600:   dcb.BaudRate:= CBR_9600;
      256000: dcb.BaudRate:= CBR_256000;
      14400:  dcb.BaudRate:= CBR_14400;
      else begin
        ManageError(46);
        exit;
      end;
    end;
    dcb.BaudRate:= fbaudrate;

    // set bytesize
    case fdatabits of
      dbFour: dcb.ByteSize:=4;
      dbFive: dcb.ByteSize:=5;
      dbSix: dcb.ByteSize:=6;
      dbSeven: dcb.ByteSize:=7;
      dbEight: dcb.ByteSize:=8;
      else dcb.bytesize:=8;
    end;

    // set parity
    case fparity of
      pNone: dcb.parity:=NOPARITY;
      pOdd: dcb.parity:=ODDPARITY;
      pEven: dcb.parity:=EVENPARITY;
      pMark: dcb.parity:=MARKPARITY;
      else dcb.parity:= EVENPARITY;
    end;

    // set stopbits
    case fstopbits of
      sbOne: dcb.StopBits:= ONESTOPBIT;
      sbOnePointFive: dcb.StopBits:= ONE5STOPBITS;
      sbTwo: dcb.StopBits:= TWOSTOPBITS;
      else dcb.stopbits:= ONESTOPBIT;
    end;

    dcb.Flags:= 1; //(1 or 2 or 16384);
    {
      DWORD fBinary: 1;          // binary mode, no EOF check   1
      DWORD fParity: 1;          // enable parity checking      2
      DWORD fOutxCtsFlow:1;      // CTS output flow control     3
      DWORD fOutxDsrFlow:1;      // DSR output flow control     4
      DWORD fDtrControl:2;       // DTR flow control type       5, 6
      DWORD fDsrSensitivity:1;   // DSR sensitivity             7
      DWORD fTXContinueOnXoff:1; // XOFF continues Tx           8
      DWORD fOutX: 1;            // XON/XOFF out flow control   9
      DWORD fInX: 1;             // XON/XOFF in flow control    10
      DWORD fErrorChar: 1;       // enable error replacement    11
      DWORD fNull: 1;            // enable null stripping       12
      DWORD fRtsControl:2;       // RTS flow control            13, 14
      DWORD fAbortOnError:1;     // abort reads/writes on error 15
      DWORD fDummy2:17;          // reserved                    16
    }

    // set dcb struct
    if not setcommstate(fhandle, DCB) then
    begin
      ManageError(15);
      break;
    end;

    // set timeouts
    commtimeouts.ReadIntervalTimeout:=0;
    commtimeouts.ReadTotalTimeoutMultiplier:=0;
    commtimeouts.ReadTotalTimeoutConstant:= freadtimeout;
    commtimeouts.WriteTotalTimeoutMultiplier:=0;
    commtimeouts.WriteTotalTimeoutConstant:= fwritetimeout;

    if not setcommtimeouts(fhandle, commtimeouts) then
    begin
      ManageError(16);
      break;
    end;

    ClearBuffers;
    ClearError;

    fstarttime:=now;
    if assigned(fafteropen) then fafteropen(self);
    result:=true;

    fopen:= true;

  end;
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00._close: boolean;
begin
  result:=False;

  // check if open
  if fhandle<>INVALID_HANDLE_VALUE then
  begin
    if assigned(fonclose) then
      fonclose(self);
    fstarttime:=0;
    closehandle(fhandle);
    fhandle:= INVALID_HANDLE_VALUE;
    fopen:=false;
    result:=true;
  end;
end;

//------------------------------------------------------------------------------
// refresh queues is called only from inside the lock
// so we don't have to care about lock things
procedure TAPI_bk8x00._refreshques;
var
  errors: cardinal;
begin
  errors:=0;

  // check that port handle is opened
  if fhandle=INVALID_HANDLE_VALUE then
  begin
    ManageError(18);
    exit;
  end;

  //
  clearcommerror(fhandle, errors, @bk8x00comstat);

  // check error bits
  case errors of
    CE_BREAK:     flasterror:=19;
    CE_DNS:       flasterror:=20;
    CE_FRAME:     flasterror:=21;
    CE_IOE:       flasterror:=22;
    CE_MODE:      flasterror:=23;
    CE_OOP:       flasterror:=24;
    CE_OVERRUN:   flasterror:=25;
    CE_PTO:       flasterror:=26;
    CE_RXOVER:    flasterror:=27;
    CE_RXPARITY:  flasterror:=28;
    CE_TXFULL:    flasterror:=29;
  end;

  // refresh que values
  foutque:= bk8x00comstat.cbOutQue;
  finque:= bk8x00comstat.cbInQue;

  // manage above serial port error(s)
  if flasterror>0 then
    ManageError(flasterror);
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00._write( outp: tbuffer ): boolean;
var
  bw: dword;
  buf: array[0..300] of byte;
  len: cardinal;
  i: integer;
  cs: byte;
begin
  result:=False;

  // check if open
  // -------------
  if fhandle=INVALID_HANDLE_VALUE then
  begin
    ManageError(18);
    exit;
  end;

  // message header
  // --------------
  buf[0]:=ord('P');
  buf[1]:=foutputlength;
  buf[2]:=fmessageident;
  buf[3]:=fbusaddress;
  cs:=ord('P')
    +foutputlength
    +fmessageident
    +fbusaddress;

  // output data
  // -----------
  for i:=0 to foutputlength-1 do
  begin
    buf[4+i*2]:=outp[i*2];
    buf[5+i*2]:=outp[i*2+1];
    cs:=cs+outp[i*2]+outp[i*2+1];
  end;

  // recalc length
  // -------------
  if foutputlength>0 then len:=5+foutputlength*2
    else len:=5;

  // checksum
  // --------
  buf[len-1]:=byte(cs);

  // send to outbuffer
  // -----------------
  if not writefile(fhandle, buf, len, bw, nil) then
  begin
    ManageError(30);
    if fclearbuffersonfailedwrite then
      clearwritebuffer;
    exit;
  end;

  // result
  // ------
  result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.ManageError(newerrorcode: integer);
begin
  flasterror:= newerrorcode;
  if assigned(ferrorevent) then
    ferrorevent(self, flasterror, GetError(flasterror));
  if fcloseonerror then
    setopen(false);
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00._read( var inp: tbuffer ): boolean;
var
  br: integer;
  buf: array[0..300] of byte;
  i: integer;
  cs: integer;
  b: byte;
  len: word;
  ws: word;
  erroralready: boolean;
begin
  result:=False;
  erroralready:=false;
  cs:=0;

  // check if open
  // -------------
  if fhandle=INVALID_HANDLE_VALUE then
  begin
    ManageError(18);
    exit;
  end;

  // wait for 5 bytes
  // ----------------
  len:=5;

  ws:=0;
  if finputwaitcycles>0 then
  begin
    while (ws<finputwaitcycles) and (finque<len) do
    begin
      _refreshques;       // query queue size(s)
      ws:=ws+1;           // increase wait cycles
      sleep(1);           // wait 1ms
    end;
    if ws>=finputwaitcycles then
    begin
      ManageError(31);
      exit;
    end;
  end else
    while (finque<len) do
      _refreshques;

  // read buffer
  // -----------
  if not readfile(fhandle, buf, len, dword(br), nil) then
  begin
    ManageError(32);
    if fclearbuffersonfailedread then
      ClearReadBuffer;
    exit;
  end;

  // check header
  // ------------
  if buf[0]<>ord('p') then
  begin
    flasterror:=33;
    erroralready:=true;
  end;
  cs:=cs+buf[0];

  // check input length
  // ------------------
  if finputlength<>buf[1] then
  begin
    // flasterror:='Input length differs from specified.'; since 1.06
    finputlength:=buf[1];     // internally modify input length
    // erroralready:=true;
  end;
  cs:=cs+buf[1];

  // message ident check
  // -------------------
  if buf[2]<>fmessageident then
  begin
    flasterror:=34;
    erroralready:=true;
  end;
  cs:=cs+buf[2];

  // check the master addr
  // ---------------------
  if buf[3]<>0 then
  begin
    flasterror:=35;
    erroralready:=true;
  end;
  cs:=cs+buf[3];

  // check status
  // ------------
  fstatus:=buf[4];
  cs:=cs+buf[4];
  if fstatus<>0 then
  begin
    flasterror:=39;
    erroralready:=true;
  end;

  // check for errors -> is it need to continue
  // ------------------------------------------
  if (erroralready) then
  begin
    ManageError(flasterror);
    if fclearbuffersonfailedread then
      ClearReadBuffer;
    exit;
  end;

  // length check
  if finputlength>0 then len:=finputlength*2+1
    else len:=1;

  // wait for rest of needed information
  // -----------------------------------
  ws:=0;
  if (finputwaitcycles>0) then
  begin
    while (ws<finputwaitcycles) and (finque<len) do
    begin
      _refreshques;
      ws:=ws+1;
      sleep(1);
    end;
    if ws>=finputwaitcycles then
    begin
      ManageError(31);
      exit;
    end;
  end else
    while (finque<len) do
      _refreshques;

  // read data buffer
  if not readfile(fhandle, buf, len, dword(br), nil) then
  begin
    ManageError(32);
    if fclearbuffersonfailedread then
      ClearReadBuffer;
    exit;
  end;

  // read data block
  for i:=0 to finputlength-1 do
  begin
    inp[i*2]:=buf[i*2];
    inp[i*2+1]:=buf[i*2+1];
    cs:=cs+buf[i*2]+buf[i*2+1];
  end;

  // checksum check
  b:=buf[len-1];
  if byte(cs)<>b then
  begin
    ManageError(36);
    if fclearbuffersonfailedread then
      ClearReadBuffer;
    exit;
  end;

  // result
  result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.showsettings;
begin
  // create window
  fbk8x00_settings:=tfbk8x00_settings.Create(self);

  fbk8x00_settings.fport:=integer(fport);
  fbk8x00_settings.fbaudrate:=integer(fbaudrate);
  fbk8x00_settings.fbusaddress:=fbusaddress;
  fbk8x00_settings.fwritelength:=foutputlength;

  // show settings dialog
  fbk8x00_settings.showmodal;

  // do not save settings if open
  if not fopen then
  if fbk8x00_settings.fexecute then
  begin
    fport:= fbk8x00_settings.fport;
    fbaudrate:= fbk8x00_settings.fbaudrate;
    fbusaddress:=fbk8x00_settings.fbusaddress;
    foutputlength:=fbk8x00_settings.fwritelength;
  end;

  // check if handle is not freed
  if fbk8x00_settings.HandleAllocated then
    fbk8x00_settings.Destroy;
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00.savesettings( fname: string ): boolean;
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
    // bk8x00
    writeln(f,fbusaddress);
    writeln(f,fmessageident);
    writeln(f,finputlength);
    writeln(f,foutputlength);

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
    ManageError(37);
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00.opensettings( fname: string ): boolean;
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
    // bk8x00
    readln(f,fbusaddress);
    readln(f,fmessageident);
    readln(f,finputlength);
    readln(f,foutputlength);

    // serial port
    readln(f,i); fport:= i;
    readln(f,i); fbaudrate:= i;
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
    ManageError(38);
end;

//------------------------------------------------------------------------------
function TAPI_bk8x00.readwrite ( var inp: tbuffer; outp: tbuffer ): boolean;
begin
  result:=false;
  if _write( outp ) then
    if _read( inp ) then
      result:=true;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.clearerror;
begin
  flasterror:=0;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00.setthreadactive(b: boolean);
begin
  if csdesigning in componentstate then exit;

  if (not fthreadactive) and (b) then
  begin
    // make sure thread delay is above zero
    if fthreaddelay<0 then
      fthreaddelay:=1;

    fthread:=tapi_bk8x00thread.create(true);        // create thread suspended
    fthread.fevent:=fthreadevent;                   // assign event to run
    fthread.fdelay:=fthreaddelay;                   // assign delay
    fthread.Priority:=fthreadpriority;              // set priority
    fthread.fsynchronized:=fthreadsynchronized;     // set synchronized flag
    fthread.fthreadtime:= ThreadTimeEvent;          // time event
    fthread.Resume;                                 // continue running the thread

    if assigned(fafterthreadcreate) then            // fire after created event
      fafterthreadcreate(self);

    fthreadactive:=b;                               // set thread as active
  end else

  if (not b) and (fthreadactive) and (fopen) then
  begin
    fthread.terminate;

    sleep(100);

    if assigned(fafterthreadterminate) then
      fafterthreadterminate(self);

    fthreadactive:=b;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_bk8x00thread.execute;
begin
  factive:=true;
  ftimer.Start;

  while not terminated do
  begin

    // fire thread event
    if assigned(fevent) then
      if fsynchronized then synchronize(ThreadEvent)
        else ThreadEvent;

    // apply cycletime value for this round
    synchronize(UpdateTime);

    // wait for defined delay unless terminated
    if not terminated then
      sleep(fdelay);

  end;

  ftimer.Stop;
  factive:=false;
end;

procedure TAPI_bk8x00thread.UpdateTime;
begin
  if assigned(fthreadtime) then
    fthreadtime( ftimer.InstervalMs );
end;

procedure TAPI_bk8x00thread.ThreadEvent;
begin
  fevent( self );
end;

constructor TAPI_bk8x00thread.create(suspended: boolean);
begin
  inherited create(suspended);
  ftimer:= tapi_timer.create(nil);   // create timer
end;

destructor TAPI_bk8x00thread.destroy;
begin
  ftimer.free;  // free timer
  inherited Destroy;
end;


//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_bk8x00]);
end;

end.
