unit API_232rw;

//------------------------------------------------------------------------------
// API_232rw unit for serial port communication
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
// 03112009, ari pikivirta
//  * changed all strings to ansistring for or because of d2009 and above
//
// 01092009, ari pikivirta
//  * fixed bug on readchar that caused return char always be 0x00
//  * added parameter to getqueue functions for refreshing them internally
//
// 19122008, ari pikivirta
//  * added reachchar and writechar functions
//  * added functions to read back currently set values
//  * added waitlength function
//
// 12082008, ari pikivirta
//  * added xonoff setting
//
// 04022008, ari pikivirta
//  * replaced all millisecondspan etc. functions with tick count difference
//    calculation because millisecond span fails on day change
//
// 28012008, ari pikivirta
//  * went back to criticalsections from MREW to be absolute sure with the threads
//    and more easy to understand structure
//
// 14082007, ari pikivirta
//  * added getreadtimeout function for bk8x component
//
// 01082007, ari pikivirta
//  * converted remaining readln function to use readbuf internally
//
// 23072007, ari pikivirta
//  * added possibility to read/write buffers
//  * added setparity and setstopbits procedures
//  * readbuf function can be extend to expect some readlength
//
// 22082005, ari pikivirta
//  * removed inque checking in readln function, also took off the sleep(1)
//    which is really not needed when waiting serial end mark to come within
//    defined time period.
//
// 15052005, ari pikivirta
//  * fixed readln and writeln functions to work much faster, also added
//    functions to wait certain text or any text to complete
//  * fixed critical sections to be used all trough the functions, but now
//    there must be init and kill called in main app create and destroy.
//
// 30112004, ari pikivirta
// * added exception handling to readln and writeln functions
// * fixed the lasterror to be empty if no errors during last readln or writeln
// * added criticalsection to avoid conflicts even if used from different
//   threads
// * added nomessage
//
// 29112004, ari pikivirta
// * added wait function for waiting responses
// * added get last error function and disable error messages
//
// 19112004, ari pikivirta
// * readln and writeln functions with automatic port open and close possibility
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, SyncObjs;

const
  MAXCOMBUFFERSIZE = 512;

type
  TCOMBuffer = array[0..MAXCOMBUFFERSIZE] of byte;

// - - - - - initialization
procedure com_init;                                       // initialize unit
function  com_setdefaults: boolean;                       // set default values
function  com_setnomessage(b: boolean = true): boolean;
function  com_getreadtimeout: integer;
function  com_setreadtimeout(timeMS: integer = 1000): boolean;
function  com_getbytesize: integer;
function  com_setbytesize(bsize: integer = 8): boolean;
function  com_setport(port: integer = 1): boolean;
function  com_getport: integer; overload;
function  com_getportstring: string; overload;
function  com_geteofchar: char;
function  com_seteofchar(ch: char = #13): boolean;
function  com_getbaudrate: integer;
function  com_setbaudrate(br: integer = 38400): boolean;
function  com_getstopbits: integer;
procedure com_setstopbits(i: integer);
function  com_getparity: integer;
procedure com_setparity(i: integer);
procedure com_xonoff(State: boolean = TRUE);

// - - - - - open
function  com_open: boolean;                              // opens com port
function  int_com_isopen: boolean;
function  com_isopen: boolean;                            // returns true if com is open

function  com_handle: thandle;

// - - - - - close
function  com_close: boolean;                             // closes com port
procedure com_kill;                                       // free unit

// - - - - - runtime
function  com_lasterror: string;
function  com_writebuf(buf: tcombuffer; len: integer): boolean;
function  com_writechar(ch: char): boolean;
function  com_write(Const txt: AnsiString): boolean;
function  com_writeln(Const txt: AnsiString): boolean;
function  com_readbuf(var buf: tcombuffer; var len: integer; Const expectedreadlength: integer = 16): boolean;
function  com_readchar(var ch: Ansichar): boolean;
function  com_read(var txt: Ansistring): boolean;
function  com_readln(var txt: Ansistring): boolean;
function  com_waitany(var txt: Ansistring; Const timeoutMS: integer = 1000): boolean;
function  com_waittxt(Const txt: Ansistring; Const timeoutMS: integer = 1000): boolean;
function  com_waitlength(var buf: tcombuffer; Const ALength: integer = 1; Const timeoutMs: integer = 1000): boolean;
function  com_clear: boolean;
procedure com_clearRXbuffer;
procedure com_clearTXbuffer;
function  com_getinputque(Const ARefreshQueues: Boolean = FALSE): integer;
function  com_getoutputque(Const ARefreshQueues: Boolean = FALSE): integer;
function  com_refreshques: boolean;
function  com_cleardtr: boolean;
function  com_clearrts: boolean;
function  com_sentmessages: int64;
function  com_readmessages: int64;
function  com_sentbytes: int64;
function  com_readbytes: int64;

//------------------------------------------------------------------------------
//##############################################################################
//------------------------------------------------------------------------------

implementation

uses
  dialogs;

type
  tcomportdata = record
    sync: tcriticalsection;
    nomessages: boolean;
    lasterror: string;
    // configuration
    port: integer;                      // serial port selected
    baudrate: integer;
    outbuffer: integer;
    inbuffer: integer;
    bytesize: integer;
    parity: integer;
    stopbits: integer;
    readtimeout: integer;
    eofchar: char;
    xonoff: boolean;
    // windows.h
    handle: thandle;                    // com port handle
    cstat: tComStat;                    // com statistics
    // internal statistics
    readmessages: int64;
    readbytes: int64;
    sentmessages: int64;
    sentbytes: int64;
  end;

var
  comdata: Tcomportdata;

//------------------------------------------------------------------------------

  // used for delay calculations
  // millisecondspan etc. will fail on
  // day change!!!!
  function GetTickDiff(const AOldTickCount, ANewTickCount : Cardinal):Cardinal;
  begin
    if ANewTickCount >= AOldTickCount then Result := ANewTickCount - AOldTickCount
      else Result := High(Cardinal) - AOldTickCount + ANewTickCount;
  end;

//------------------------------------------------------------------------------
procedure com_init;
begin
  //comdata.sync:= tmultireadexclusivewritesynchronizer.create;
  comdata.sync:= tcriticalsection.create;
  comdata.handle:= invalid_handle_value;
  com_setdefaults;
end;

//------------------------------------------------------------------------------
procedure com_kill;
begin
  if com_isopen then
  begin
    com_clearTXbuffer;
    com_clearRXbuffer;
    com_close;
  end;
  comdata.sync.Free;
end;

//------------------------------------------------------------------------------
function com_setdefaults: boolean;
begin
  comdata.sync.Acquire;
  try
    if not int_com_isopen then
    begin
      comdata.port:=      1;
      comdata.baudrate:=  CBR_38400;
      comdata.inbuffer:=  512;
      comdata.outbuffer:= 512;
      comdata.bytesize:=  8;
      comdata.parity:=    NOPARITY;
      comdata.stopbits:=  ONESTOPBIT;
      comdata.readtimeout:=   500;
      comdata.eofchar:=   '\';
      comdata.nomessages:= true;
      comdata.lasterror:= '';
      comdata.xonoff:=    false;
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function com_lasterror: string;
begin
  comdata.sync.Acquire;
  try
    result:= comdata.lasterror;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function com_sentmessages: int64;
begin
  comdata.sync.Acquire;
  try
    result:= comdata.sentmessages;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure com_xonoff(State: boolean = TRUE);
begin
  comdata.sync.acquire;
  try
    comdata.xonoff:= state;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function  com_readmessages: int64;
begin
  comdata.sync.Acquire;
  try
    result:= comdata.readmessages;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function  com_sentbytes: int64;
begin
  comdata.sync.Acquire;
  try
    result:= comdata.sentbytes;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function  com_readbytes: int64;
begin
  comdata.sync.Acquire;
  try
    result:= comdata.readbytes;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function  com_setnomessage(b: boolean): boolean;
begin
  comdata.sync.Acquire;
  try
    comdata.nomessages:= b;
    result:= true;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure int_com_refreshques;
var
  errors: cardinal;
begin
  errors:= 0;
  clearcommerror( comdata.handle, errors, @comdata.cstat );
  case errors of
    CE_BREAK:     comdata.lasterror:='The hardware detected a break condition.';
    CE_DNS:       comdata.lasterror:='A parallel device is not selected.';
    CE_FRAME:     comdata.lasterror:='The hardware detected a framing error.';
    CE_IOE:       comdata.lasterror:='An I/O error occurred during communications with the device.';
    CE_MODE:      comdata.lasterror:='The requested mode is not supported, or the hFile parameter is invalid. If this value is specified, it is the only valid error.';
    CE_OOP:       comdata.lasterror:='A parallel device signaled that it is out of paper.';
    CE_OVERRUN:   comdata.lasterror:='A character-buffer overrun has occurred. The next character is lost.¨';
    CE_PTO:       comdata.lasterror:='A time-out occurred on a parallel device.';
    CE_RXOVER:    comdata.lasterror:='An input buffer overflow has occurred. There is either no room in the input buffer, or a character was received after the end-of-file (EOF) character.';
    CE_RXPARITY:  comdata.lasterror:='The hardware detected a parity error.';
    CE_TXFULL:    comdata.lasterror:='The application tried to transmit a character, but the output buffer was full.';
  end;
end;

//------------------------------------------------------------------------------
function com_refreshques: boolean;
begin
  comdata.sync.Acquire;
  try
    if int_com_isopen then
    begin
      int_com_refreshques;
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function com_getinputque(Const ARefreshQueues: Boolean = FALSE): integer;
begin
  comdata.sync.Acquire;
  try
    if ARefreshqueues then int_com_refreshques;
    result:= comdata.cstat.cbInQue;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function com_getoutputque(Const ARefreshQueues: Boolean = FALSE): integer;
begin
  comdata.sync.Acquire;
  try
    if ARefreshqueues then int_com_refreshques;
    result:= comdata.cstat.cbOutQue;
  finally
    comdata.sync.Release;
  end;
end;

//------------------------------------------------------------------------------
function com_setreadtimeout(timeMS: integer): boolean;
begin
  comdata.sync.Acquire;
  try
    if not int_com_isopen then
    begin
      comdata.readtimeout:= timems;
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function  com_getreadtimeout: integer;
begin
  comdata.sync.acquire;
  try
    result:= comdata.readtimeout;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_seteofchar(ch: char): boolean;
begin
  comdata.sync.acquire;
  try
    if int_com_isopen then
    begin
      comdata.eofchar:= ch;
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_geteofchar: char;
begin
  comdata.sync.acquire;
  try
    result:= comdata.eofchar;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function int_com_isopen: boolean;
begin
  result:= comdata.handle<>INVALID_HANDLE_VALUE;
end;

function com_isopen: boolean;
begin
  comdata.sync.acquire;
  try
    result:= int_com_isopen;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_getport: integer;
begin
  comdata.sync.acquire;
  try
    result:= comdata.port;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function  com_getportstring: string;
begin
  comdata.sync.acquire;
  try
    result:= 'COM'+inttostr(comdata.port);
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_setbaudrate(br: integer): boolean;
begin
  comdata.sync.acquire;
  try
    result:= false;
    if int_com_isopen then exit;

    case br of
      110: comdata.baudrate:=     CBR_110;
      300: comdata.baudrate:=     CBR_300;
      600: comdata.baudrate:=     CBR_600;
      1200: comdata.baudrate:=    CBR_1200;
      2400: comdata.baudrate:=    CBR_2400;
      4800: comdata.baudrate:=    CBR_4800;
      9600: comdata.baudrate:=    CBR_9600;
      14400: comdata.baudrate:=   CBR_14400;
      19200: comdata.baudrate:=   CBR_19200;
      38400: comdata.baudrate:=   CBR_38400;
      56000: comdata.baudrate:=   CBR_56000;
      115200: comdata.baudrate:=  CBR_115200;
      128000: comdata.baudrate:=  CBR_128000;
      256000: comdata.baudrate:=  CBR_256000;
      else begin
        comdata.baudrate:= 0;
        exit;
      end;
    end;
    result:= true;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_getbaudrate: integer;
begin
  comdata.sync.acquire;
  try
    case comdata.baudrate of
      CBR_110: result:=       110;
      CBR_300: result:=       300;
      CBR_600: result:=       600;
      CBR_1200: result:=      1200;
      CBR_2400: result:=      2400;
      CBR_4800: result:=      4800;
      CBR_9600: result:=      9600;
      CBR_14400: result:=     14400;
      CBR_19200: result:=     19200;
      CBR_38400: result:=     38400;
      CBR_56000: result:=     56000;
      CBR_115200: result:=    115200;
      CBR_128000: result:=    128000;
      CBR_256000: result:=    256000;
      else begin
        result:= 0;
      end;
    end;
  finally
    comdata.sync.release;
  end;
end;

//-----------------------------------------------------------------------------
function com_getbytesize: integer;
begin
  comdata.sync.acquire;
  try
    result:= comdata.bytesize;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_setbytesize(bsize: integer): boolean;
begin
  if (bsize>3) and (bsize<9) then
  begin
    comdata.sync.acquire;
    try
      comdata.bytesize:= bsize;
      result:= true;
    finally
      comdata.sync.release;
    end;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function com_setport(port: integer): boolean;
begin
  comdata.sync.acquire;
  try
    if not int_com_isopen then
    begin
      comdata.port:= port;
      result:= true;
    end else
      result:= falsE;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_getstopbits: integer;
begin
  comdata.sync.acquire;
  try
    result:= comdata.stopbits;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
procedure com_setstopbits(i: integer);
begin
  comdata.sync.acquire;
  try
    comdata.stopbits:= i;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function  com_getparity: integer;
begin
  comdata.sync.acquire;
  try
    result:= comdata.parity;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
procedure com_setparity(i: integer);
begin
  comdata.sync.acquire;
  try
    comdata.parity:= i;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_open: boolean;

  procedure do_open_error(text: string);
  begin
    if not comdata.nomessages then showmessage(text);
    comdata.lasterror:= text;
    if comdata.handle<>INVALID_HANDLE_VALUE then
    begin
      closehandle(comdata.handle);
      comdata.handle:= INVALID_HANDLE_VALUE;
    end;
  end;

var
  dcb: _dcb;
  commtimeouts: tcommtimeouts;
  portstring: string;
begin
  comdata.sync.acquire;
  try
    result:= false;

    // close if it was open
    if int_com_isopen then
    begin
      closehandle(comdata.handle);
      comdata.handle:= INVALID_HANDLE_VALUE;
    end;

    if comdata.port>9 then portstring:= '\\.\'+'COM'+inttostr(comdata.port)
      else portstring:= 'COM'+inttostr(comdata.port);

    comdata.handle:= CreateFile( pchar(portstring),
      GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if comdata.handle = invalid_handle_value then
    begin
      do_open_error('Error opening com handle');
      exit;
    end;

    (* unnecessary, on these we'll trust ms
    if not setupcomm( comdata.handle, comdata.outbuffer, comdata.inbuffer) then
    begin
      do_open_error('Failed to setup com buffers');
      exit;
    end;
    *)

    if not getcommstate( comdata.handle, dcb) then
    begin
      do_open_error('Failed to get com state');
      exit;
    end;

    dcb.BaudRate:=  comdata.baudrate;
    dcb.bytesize:=  comdata.bytesize;
    dcb.Parity:=    comdata.parity;
    dcb.StopBits:=  comdata.stopbits;

    {
      DWORD fBinary: 1;          // binary mode, no EOF check   1       1
      DWORD fParity: 1;          // enable parity checking      2       2
      DWORD fOutxCtsFlow:1;      // CTS output flow control     3       4
      DWORD fOutxDsrFlow:1;      // DSR output flow control     4       8
      DWORD fDtrControl:2;       // DTR flow control type       5, 6    16, 32
      DWORD fDsrSensitivity:1;   // DSR sensitivity             7       64
      DWORD fTXContinueOnXoff:1; // XOFF continues Tx           8       128
      DWORD fOutX: 1;            // XON/XOFF out flow control   9       256
      DWORD fInX: 1;             // XON/XOFF in flow control    10      512
      DWORD fErrorChar: 1;       // enable error replacement    11      1024
      DWORD fNull: 1;            // enable null stripping       12      2048
      DWORD fRtsControl:2;       // RTS flow control            13, 14  4096, 8192
      DWORD fAbortOnError:1;     // abort reads/writes on error 15      16384
      DWORD fDummy2:17;          // reserved                    16
    }
    if comdata.xonoff then
    begin
      dcb.Flags:= 256 or 512;
    end else
      dcb.Flags:= 1; // binary mode

    if not setcommstate( comdata.handle, dcb ) then
    begin
      do_open_error('Failed to set com state');
      exit;
    end;

    // set timeouts, ms sucks on these
    // they're defined by default something
    // they shouldn't be to get usb-serial
    // adapter to work for example
    commtimeouts.ReadIntervalTimeout:=          100;
    commtimeouts.ReadTotalTimeoutMultiplier:=   0;
    commtimeouts.ReadTotalTimeoutConstant:=     500;
    commtimeouts.WriteTotalTimeoutMultiplier:=  0;
    commtimeouts.WriteTotalTimeoutConstant:=    500;
    if not setcommtimeouts(comdata.handle, commtimeouts) then
    begin
      do_open_error('Failed to set timeouts');
      exit;
    end;

    comdata.sentbytes:= 0;
    comdata.readbytes:= 0;
    comdata.sentmessages:= 0;
    comdata.readmessages:= 0;
    comdata.lasterror:= '';

    // com clear..
    PurgeComm(comdata.handle, PURGE_TXCLEAR or PURGE_RXCLEAR);

    result:= true;

  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function  com_handle: thandle;
begin
  comdata.sync.acquire;
  try
    result:= comdata.handle;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_close: boolean;
begin
  // close handle
  comdata.sync.acquire;
  try
    if int_com_isopen then
    begin
      closehandle(comdata.handle);
      comdata.handle:= INVALID_HANDLE_VALUE;
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_clear: boolean;
begin
  // clear buffers
  comdata.sync.acquire;
  try
    if int_com_isopen then
    begin
      // abort all transfers
      PurgeComm(comdata.handle, PURGE_TXABORT or PURGE_TXCLEAR or PURGE_RXABORT or PURGE_RXCLEAR);
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
procedure com_clearRXbuffer;
begin
  comdata.sync.acquire;
  try
    if int_com_isopen then
      PurgeComm(comdata.handle, PURGE_RXABORT or PURGE_RXCLEAR);
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
procedure com_clearTXbuffer;
begin
  comdata.sync.acquire;
  try
    if int_com_isopen then
      PurgeComm(comdata.handle, PURGE_TXABORT or PURGE_TXCLEAR);
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_cleardtr: boolean;
begin
  comdata.sync.acquire;
  try
    if int_com_isopen then
    begin
      EscapeCommFunction( comdata.handle, Windows.CLRDTR );
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_clearrts: boolean;
begin
  comdata.sync.acquire;
  try
    if int_com_isopen then
    begin
      EscapeCommFunction( comdata.handle, Windows.CLRRTS );
      result:= true;
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_writebuf(buf: tcombuffer; len: integer): boolean;
var
  bw: cardinal;
begin
  comdata.sync.acquire;
  try

    result:= false;
    if not int_com_isopen then exit;

    // send buffer
    if not windows.WriteFile(comdata.handle, buf, cardinal(len), bw, nil) then
    begin
      if not comdata.nomessages then showmessage('Failed to write to port');
      comdata.lasterror:= 'Failed to write to port';
    end else
    begin
      if bw<>cardinal(len) then
      begin
        if not comdata.nomessages then showmessage('Bytes written not equal to length');
        comdata.lasterror:= 'Bytes written not equal to length';
      end else
      begin
        comdata.sentmessages:= comdata.sentmessages + 1;
        result:= true;
      end;
      comdata.sentbytes:= comdata.sentbytes + bw;
      int_com_refreshques;
    end;

  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_writechar(ch: char): boolean;
var
  b: tcombuffer;
begin
  comdata.sync.Acquire;
  try
    if int_com_isopen then
    begin
      b[0]:= ord(ch);
      result:= com_writebuf(b, 1);
    end else
      result:= false;
  finally
    comdata.sync.release;
  end;
end;

function com_readchar(var ch: char): boolean;
var
  b: tcombuffer;
  br: cardinal;
begin
  comdata.sync.Acquire;
  try
    result:= false;
    if int_com_isopen then
    begin
      if readfile(comdata.handle, b, 1, br, nil) then
      begin
        ch:= char(b[0]); // 01092009
        result:= true;
        comdata.readmessages:= comdata.readmessages + 1;
        comdata.readbytes:= comdata.readbytes + 1;
      end;
    end;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_write(Const txt: Ansistring): boolean;
var
  b: tcombuffer;
  i: integer;
begin
  for i:=1 to length(txt) do
    b[i-1]:= ord(txt[i]);
  result:= com_writebuf(b, length(txt));
end;

//------------------------------------------------------------------------------
function com_writeln(Const txt: Ansistring): boolean;
var
  tmpS: AnsiString;
begin
  tmpS:= txt;
  if tmpS<>'' then
    if tmpS[length(tmpS)]<>comdata.eofchar then
      tmpS:= tmpS + comdata.eofchar;                    // add eof char to end of line
  result:= com_write(tmpS);
end;

//------------------------------------------------------------------------------
function com_readbuf(var buf: tcombuffer; var len: integer; Const expectedreadlength: integer = 16): boolean;
var
  br: cardinal;
  greater: cardinal;
  starttime: cardinal;
begin
  comdata.sync.acquire;
  try

    result:= false;
    if not int_com_isopen then exit;

    starttime:= GetTickCount;

    while (GetTickDiff(starttime, GetTickCount)<cardinal(comdata.readtimeout)) and (not result) do
    begin

      // refresh queues
      int_com_refreshques;

      // check if something was read
      if comdata.cstat.cbInQue>0 then
      begin

        // select greater char length to read
        if comdata.cstat.cbinque>dword(expectedreadlength) then
          greater:= comdata.cstat.cbinque
          else greater:= expectedreadlength;

        // read from serial port
        if readfile(comdata.handle, buf, greater, br, nil) then
        begin
          inc(comdata.readmessages, 1);
          inc(comdata.readbytes, br);
          len:= br;
          result:= true;
        end;

      end else

        // sleep here, to help threads to work even they
        // directly run this read buffer function
        sleep(1);

    end;
  finally
    comdata.sync.release;
  end;
end;

//------------------------------------------------------------------------------
function com_read(var txt: AnsiString): boolean;
var
  b: tcombuffer;
  i, br: integer;
begin
  result:= false;
  if not com_isopen then exit;

  txt:= '';
  if com_readbuf(b, br) then
  begin
    for i:=0 to br-1 do
      txt:= txt + chr(b[i]);
    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function com_readln(var txt: Ansistring): boolean;
var
  received: Ansistring;
  buf: tcombuffer;
  len, i: integer;
  ok: boolean;
begin
  // read input queue items
  received:= '';
  ok:= false;
  while (com_readbuf(buf, len, 4))            // if read any of expected 8 characters
    and (not ok) do                           // or end-of-char received
  begin
    for i:=0 to len-1 do
    begin
      received:= received + chr(buf[i]);      // add to received buffer
      ok:= (chr(buf[i]) = com_geteofchar);    // ok if [eofchar] found as character
    end;
  end;
  // check result
  if ok then
  begin
    txt:= received;
    result:= true;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function com_waitany(var txt: Ansistring; Const timeoutMS: integer = 1000): boolean;
var
  stime: cardinal;
begin
  txt:= '';
  stime:= GetTickCOunt;
  if com_isopen then
    while (GetTickDiff(stime, gettickcount)<cardinal(timeoutMS)) and (txt='') do
      com_readln(txt);
  result:= (txt <> '');
end;

//------------------------------------------------------------------------------
function com_waittxt(Const txt: Ansistring; Const timeoutMS: integer = 1000): boolean;
var
  temp: Ansistring;
  stime: cardinal;
begin
  temp:= '';
  stime:= GetTickCount;
  if com_isopen then
  begin
    if timeoutMS>0 then
    begin
      // wait defined time
      while (GetTickDiff(stime, gettickcount)<cardinal(timeoutMS)) and (temp<>txt) do
        com_readln(temp);
    end else
      // infinite wait
      while (temp = txt) do
        com_readln(temp);
  end;
  // return true if succesfull
  result:= (temp = txt);
end;

//------------------------------------------------------------------------------
function com_waitlength(var buf: tcombuffer; Const ALength: integer = 1; Const timeoutMs: integer = 1000): boolean;
var
  stime: cardinal;
  pos, readlen, bytesread: integer;
  tempbuf: tcombuffer;
begin
  result:= FALSE;
  stime:= GetTickCount;
  //
  if (com_isopen) and (timeoutMs>0) then
  begin
    bytesread:= 0;
    while (GetTickDiff(stime, GetTickCount)<cardinal(timeoutMs)) and (bytesread<ALength) do
    begin
      if (com_readbuf(tempbuf, readlen, 1)) and (readlen>0) then
      begin
        for pos:=0 to readlen-1 do
          buf[bytesread+pos]:= tempbuf[pos];
        inc(bytesread, readlen);
      end;
    end;
  end;
end;

end.


