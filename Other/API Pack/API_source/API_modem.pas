unit API_modem;

//------------------------------------------------------------------------------
// TAPI_modem, as simplest possible user interface for the modems using
// serial port. as this is just for testing purposes, there's only very
// basic items implemented, but from this - it shouldn't be problem to extend
// this to do everything..
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// revision history
//
//  r1.01, 12072007, ari pikivirta
//    * removed stupid properties
//    * added at commands from the standard to the constants (easier to remember)
//    * implemented at command request / receive interface
//    * implemented simple SMS sending in text mode
//    * faults signaled trough exceptions
//
//  r1.00, ari pikivirta
//    * first revision
//------------------------------------------------------------------------------
(*

    get list of sms messages

      AT
      OK
      AT+CMGF=1
      OK
      AT+CMGL="ALL"
      +CMGL: 1,"REC READ","+85291234567",,"06/11/11,00:30:29+32"
      Hello, welcome to our SMS tutorial.
      +CMGL: 2,"REC READ","+85291234567",,"06/11/11,00:32:20+32"
      A simple demo of SMS text messaging.

      OK

      <<< should be same as AT+CMGL=1 (if 0 then it's only unread messages)

*)

interface

uses
  Windows, SysUtils, Classes;

const
  AT_BREAK      = #27;            // esc
  AT_ENTER      = #13;            // enter
  AT_END        = #26;            // ctrl + z

  // AT COMMANDS from the specification
  AT_AT         = 'AT';           // AT_ENTER (to check if at commands are avail)
  AT_MODE       = 'AT+CMGF=';     // 0 = pdu mode, 1 = text mode + AT_ENTER
  AT_CHARSET    = 'AT+CSCS=';     // "charset" + AT_ENTER
  AT_SMSCENTER  = 'AT+CSCA=';     // "smscenter number" + AT_ENTER
  AT_SMSMSG     = 'AT+CMGS=';     // AT_ENTER.. message + AT_END
  AT_LISTALL    = 'AT+CMGL=0';    // list all received messages
  AT_LISTNEW    = 'AT+CMGL=1';    // list new received messages

  // response texts..
  RESP_OK       = 'OK'+#13#10;    // ok response
  RESP_LF       = '>'+#32;        // line feed response
  RESP_LIST     = '+CMGL:';       // new list item

type
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

type
  TSMSItem = record
    sender: string;     // sender phone number
    stamp: string;      // time stamp
    text: string;       // contents
  end;

  TSMSList = record
    count: integer;
    data: array of TSMSItem;
  end;

  TAPI_modem = class(TComponent)
  private
    fversion: string;
    fopen: boolean;
    fhandle: thandle;
    fport: integer;
    fbaudrate: integer;
    fparity: tparity;
    fdatabits: tdatabits;
    fstopbits: tstopbits;
    finbuffer: integer;
    foutbuffer: integer;
    freadtimeout: cardinal;
    fwritetimeout: cardinal;
    finque: integer;
    foutque: integer;
    flog: string;

    // serial port
    procedure setopen(b: boolean);
    procedure setport(i: integer);
    procedure setbaudrate(b: integer);
    function _open: boolean; // internal
    function _close: boolean; // internal
    procedure _refreshques;

    function _write( s: string ): boolean;
    function _read( var s: string ): boolean;

  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    // communication
    function SendCMD(command: string): boolean;
    function Wait(ToWait: string; timeout: cardinal = 1000): boolean;

    procedure ClearLog;
    function GetLog: string;

    // sms
    function SendSMS_TextMode(
        recipient: string;
        text: string;
        smscenter: string = '';
        charset: string = ''
      ): boolean;

    function ListSMS_TextMode(
        var List: TSMSList; OnlyNew: boolean = true
      ): boolean;

    // settings related
    function SaveSettings( fname: string ): boolean;
    function OpenSettings( fname: string ): boolean;

  published
    property Version: string read fversion;
    property open: boolean read fopen write setopen;
    property port: integer read fport write setport;
    property baudrate: integer read fbaudrate write setbaudrate;

  end;

procedure Register;

implementation

const
  versioninfostring = 'r1.01/ari.pikivirta@kolumbus.fi';

var
  cstat: comstat;

{$r *.res}

//------------------------------------------------------------------------------
constructor TAPI_modem.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=      versioninfostring;
  fopen:=         false;
  fhandle:=       INVALID_HANDLE_VALUE;
  fport:=         0;
  fbaudrate:=     115200;
  fparity:=       peven;
  fdatabits:=     dbeight;
  fstopbits:=     sbone;
  finbuffer:=     512;
  foutbuffer:=    512;
  freadtimeout:=  1000;
  fwritetimeout:= 1000;
  flog:=          '';
end;

//------------------------------------------------------------------------------
destructor TAPI_modem.destroy;
begin
  _close;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_modem.setopen (b: boolean);
begin
  if b<>fopen then
    if b then _open
      else _close;
end;

//------------------------------------------------------------------------------
procedure TAPI_modem.setport (i: integer);
begin
  if fopen then
    raise exception.create('Cannot change port while port is open');
  fport:= i;
end;

//------------------------------------------------------------------------------
procedure TAPI_modem.setbaudrate (b: integer);
begin
  if fopen then
    raise exception.create('Cannot change baudrate while port is open');
  fbaudrate:=b;
end;

//------------------------------------------------------------------------------
function TAPI_modem._open: boolean;
var
  portstr: string;
  commtimeouts: tcommtimeouts;
  dcb: _dcb;
begin
  result:=false;

  // check if open
  if fopen then
  begin
    //raise exception.create('Port is already open.');
    exit;
  end;

  // get device name
  portstr:= 'COM'+inttostr(fport);

  // open handle
  fhandle := CreateFile(@portstr[1], GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  // result
  if (fhandle=invalid_handle_value) then
    raise exception.Create('Failed to create com file');

  // set buffer sizes
  if not setupcomm(fhandle, foutbuffer, finbuffer) then
  begin
    closehandle(fhandle);
    raise exception.create('Failed to setup comm buffers');
  end;

  // get dcb struct
  if not getcommstate(fhandle, dcb) then
  begin
    closehandle(fhandle);
    raise exception.create('Failed to get comm state');
  end;

   // set baudrate
  case fbaudrate of
    110: dcb.BaudRate:=CBR_110;
    300: dcb.BaudRate:=CBR_300;
    600: dcb.BaudRate:=CBR_600;
    1200: dcb.BaudRate:=CBR_1200;
    2400: dcb.BaudRate:=CBR_2400;
    4800: dcb.BaudRate:=CBR_4800;
    9600: dcb.BaudRate:=CBR_9600;
    14400: dcb.BaudRate:=CBR_14400;
    19200: dcb.BaudRate:=CBR_19200;
    38400: dcb.BaudRate:=CBR_38400;
    56000: dcb.baudrate:=CBR_56000;
    115200: dcb.BaudRate:=CBR_115200;
    128000: dcb.BaudRate:=CBR_128000;
    256000: dcb.BaudRate:=CBR_256000;
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
  if not setcommstate(fhandle, DCB) then
  begin
    closehandle(fhandle);
    raise exception.create('Failed to set comm state');
  end;

  // set timeouts
  commtimeouts.ReadIntervalTimeout:=0;
  commtimeouts.ReadTotalTimeoutMultiplier:=0;
  commtimeouts.ReadTotalTimeoutConstant:= freadtimeout;
  commtimeouts.WriteTotalTimeoutMultiplier:=0;
  commtimeouts.WriteTotalTimeoutConstant:= fwritetimeout;

  if not setcommtimeouts(fhandle, commtimeouts) then
  begin
    closehandle(fhandle);
    raise exception.create('Failed to set comm timeouts');
  end;

  PurgeComm(fhandle, PURGE_TXABORT or PURGE_TXCLEAR);
  PurgeComm(fhandle, PURGE_RXABORT or PURGE_RXCLEAR);

  fopen:=true;
  result:=true;
end;

//------------------------------------------------------------------------------
function TAPI_modem._close: boolean;
begin
  result:=False;

  // check if open
  if not fopen then
  begin
    result:= true;
    exit;
  end;

  // close handle
  closehandle(fhandle);
  fopen:=false;
end;

//------------------------------------------------------------------------------
procedure TAPI_modem._refreshques;
var
  errors: cardinal;
  flasterror: string;
begin
  errors:=0;

  if not fopen then
    raise exception.create('Port is not open');

  clearcommerror(fhandle, errors, @cstat);

  case errors of
    CE_BREAK:     flasterror:='The hardware detected a break condition.';
    CE_DNS:       flasterror:='A parallel device is not selected.';
    CE_FRAME:     flasterror:='The hardware detected a framing error.';
    CE_IOE:       flasterror:='An I/O error occurred during communications with the device.';
    CE_MODE:      flasterror:='The requested mode is not supported, or the hFile parameter is invalid. If this value is specified, it is the only valid error.';
    CE_OOP:       flasterror:='A parallel device signaled that it is out of paper.';
    CE_OVERRUN:   flasterror:='A character-buffer overrun has occurred. The next character is lost.¨';
    CE_PTO:       flasterror:='A time-out occurred on a parallel device.';
    CE_RXOVER:    flasterror:='An input buffer overflow has occurred. There is either no room in the input buffer, or a character was received after the end-of-file (EOF) character.';
    CE_RXPARITY:  flasterror:='The hardware detected a parity error.';
    CE_TXFULL:    flasterror:='The application tried to transmit a character, but the output buffer was full.';
    else          flasterror:='';
  end;
  if flasterror<>'' then
    raise exception.create(flasterror);

  foutque:= cstat.cbOutQue;
  finque:=  cstat.cbInQue;
end;

//------------------------------------------------------------------------------
function TAPI_modem._write( s: string ): boolean;
const
  MAXBUFSIZE = 1000;
var
  bw: dword;
  i: integer;
  buf: array[0..MAXBUFSIZE] of char;
begin
  if not fopen then
    raise exception.create('Port is not open');

  // copy string to array if chars
  for i:=1 to MAXBUFSIZE do
    if i<=length(s) then buf[i-1]:= s[i]
      else buf[i-1]:=#0;

  // send buffer out
  result:= writefile(fhandle, buf, length(s), bw, nil);

  // add to internal log
  flog:= flog + s;

  // refresh queues & status
  _refreshques;
end;

//------------------------------------------------------------------------------
function TAPI_modem._read( var s: string ): boolean;
var
  br: integer;
  buf: array[0..1000] of char;
  i: integer;
begin
  s:='';

  if not fopen then
    raise exception.Create('Port is not open');

  // refresh queues to get input size
  _refreshques;

  if finque<1 then
  begin
    result:= false;
    exit;
  end;

  // read buffer
  result:= readfile(fhandle, buf, finque, dword(br), nil);

  // copy buffer to string
  for i:=0 to br-1 do
    s:= s + buf[i];

  // add to internal log
  flog:= flog + s;

  // refresh queues & status
  _refreshques;
end;

//------------------------------------------------------------------------------
procedure TAPI_modem.ClearLog;
begin
  flog:= '';
end;

//------------------------------------------------------------------------------
function TAPI_modem.GetLog: string;
begin
  result:= flog;
end;

//------------------------------------------------------------------------------
// send command
function TAPI_modem.sendCMD(command: string): boolean;
begin
  result:= _write(command);
end;

//------------------------------------------------------------------------------
// wait ok response
function TAPI_modem.Wait(ToWait: string; timeout: cardinal = 1000): boolean;
var
  tw, s, buf: string;
  wst: cardinal;
begin
  result:=  false;
  if not fopen then exit;

  // wait for defined response
  tw:=      uppercase(towait);
  buf:=     '';
  wst:=     gettickcount;
  repeat

    if _read(s) then
    begin
      buf:= buf + uppercase(s);

      // check for text to wait
      if pos(tw, buf)>0 then
      begin
        result:= true;
        break;
      end else

      // check for error response
      if pos('ERROR', buf)>0 then
      begin
        result:= false;
        break;
      end;

    end;

  until (wst+timeout<gettickcount);
end;

//------------------------------------------------------------------------------
// sms text mode
function TAPI_modem.sendSMS_TextMode(
      recipient: string;
      text: string;
      smscenter: string = '';
      charset: string = ''): boolean;
begin
  result:= false;

  // port must be open
  if not fopen then exit;

  // break last operation
  sendcmd(AT_BREAK+AT_ENTER);

  // start at comands
  sendcmd(AT_AT+AT_ENTER);
  if not wait(RESP_OK) then exit;

  // text mode
  sendcmd(AT_MODE+'1'+AT_ENTER);
  if not wait(RESP_OK) then exit;

  // service center
  if smscenter<>'' then
  begin
    sendcmd(AT_SMSCENTER+'"'+smscenter+'"'+AT_ENTER);
    if not wait(RESP_OK) then exit;
  end;

  // charset
  if charset<>'' then
  begin
    sendcmd(AT_CHARSET+'"'+charset+'"'+AT_ENTER);
    if not wait(RESP_OK) then exit;
  end;

  // set recipient
  sendcmd(AT_SMSMSG+'"'+recipient+'"'+AT_ENTER);
  if not wait(RESP_LF) then exit;

  // write message
  sendcmd(text+AT_BREAK+AT_ENTER);
  if not wait(RESP_OK) then exit;

  // ok!!!
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_modem.ListSMS_TextMode(
        var List: TSMSList; OnlyNew: boolean = true
      ): boolean;
var
  s, tmp: string;
begin
  result:= false;

  // port must be open
  if not fopen then exit;

  // break last operation
  sendcmd(AT_BREAK+AT_ENTER);

  // start at comands
  sendcmd(AT_AT+AT_ENTER);
  if not wait(RESP_OK) then exit;

  // text mode
  sendcmd(AT_MODE+'1'+AT_ENTER);
  if not wait(RESP_OK) then exit;

  // list new messages
  if Onlynew then sendcmd(AT_LISTNEW+AT_ENTER)
    else sendcmd(AT_LISTALL+AT_ENTER);
  // sendcmd('AT+CMGL=?'+AT_ENTER);

  // wait for at least one list item
  if not wait(RESP_LIST) then exit;

  // clear list
  list.count:=0;
  setlength(list.data, list.count);

  // read until there's no new messages
  // left on the phone
  repeat

    // add new item
    list.count:= list.count + 1;
    setlength(list.data, list.count);

    // read until line change (#13#10)
    s:= '';
    repeat
      if _read(tmp) then
        s:= s + tmp;
    until (pos(#13, s)>0);

    // parse read line

    // add to the item created
    list.data[ list.count-1 ].sender:= '';
    list.data[ list.count-1 ].stamp:= '';
    list.data[ list.count-1 ].text:= s;

  until not wait(RESP_LIST);

  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_modem.savesettings( fname: string ): boolean;
var
  f: textfile;
begin
  {$i-}
  assignfile(f,fname);
  rewrite(f);
  {$i+}
  if ioresult=0 then
  begin
    writeln(f,fport);
    writeln(f,integer(fbaudrate));
    writeln(f,integer(fparity));
    writeln(f,integer(fdatabits));
    writeln(f,integer(fstopbits));
    writeln(f,finbuffer);
    writeln(f,foutbuffer);
    writeln(f,freadtimeout);
    writeln(f,fwritetimeout);
    closefile(f);
    result:= true;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function TAPI_modem.opensettings( fname: string ): boolean;
var
  f: textfile;
  i: integer;
begin
  {$i-}
  assignfile(f,fname);
  reset(f);
  {$i+}
  if ioresult=0 then
  begin
    readln(f,fport);
    readln(f,fbaudrate);
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
    result:= false;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_modem]);
end;

end.
