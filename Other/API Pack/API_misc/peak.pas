unit peak;

//
//------------------------------------------------------------------------------
//
// PEAK CAN DONGLE INTERFACE UNIT (see: http://www.peak-system.com/)
// ---------------
//
// before you're able to use this as interface unit for easier access to the
// peak's usb-can dongle threadsafe, peak drivers must be installed because
// this uses dll trough the peaks example interface unit (pcanlight).
//
// regards,
// ari pikivirta
//
//------------------------------------------------------------------------------
//
// HISTORY:
// --------
//
// 20052009, ari pikivirta
//  * added general use function CanmsgToString(temp: tcanmessage): string;
//
// 23032009, ari pikivirta
//  * fixed bug on clearbuffers function that didn't mark the remote nor write flags correctly
//
// 23022009, ari pikivirta
//  * added CanObject to have ALL messages as event when read or write using
//    this interface unit
//
// 17022009, ari pikivirta
//  * cleaned up code on the reset hardware function and
//
// 05022009, ari pikivirta
//  * started implementing read and write strings trough SDO segmented transfer
//
// 09122008, ari pikivirta
//  * added rwlock for the actual read and write operations trought the dll
//  * changed status check to function
//  * changed way to detect bus heavy on bus off on status check
//  * added funclock to block cross using functions from different threads
//
// 01072008, ari pikivirta
//  * more detailed definitions to SDO commands and responses byte
//
// 30052008, ari pikivirta
//  * added remote frame flag on reading those out of the buffer
//  * added write frame flag on messages written trough this library
//
// 29052008, ari pikivirta
//  * added clearing of receive buffer at the end of read message function (with filter)
//
// 13052008, ari pivivirta
//  * updated according to latest peak library (PCANLight)
//
// 13032008, ari pikivirta
//  * remote request messages not ignored anymore, included into the reading
//
// 07012008, ari pikivirta
//  * added reopening of dongle on bus off situation
//  * added bus heavy state checking
//
// 01082007, ari pikivirta
//  * replaced critical section with multireadexclusivewritesynchronizer
//  * moved already sorted variable into data record
//
// 19062007, ari pikivirta
//  * added stat_count function to retrieve amount of different messages
//    currently stored into the can static list
//  * added alreadysorted flag to not to do unnecessary sorting even if
//    stat parameter is set to true
//
// 06062007, ari pikivirta
//  * recreated from scratch to maximize compatibility
//
//------------------------------------------------------------------------------

interface

uses
  windows, sysutils, classes, syncobjs;

const
  // bit definitions
  BIT0                = 1;
  BIT1                = 2;
  BIT2                = 4;
  BIT3                = 8;
  BIT4                = 16;
  BIT5                = 32;
  BIT6                = 64;
  BIT7                = 128;
  BIT8                = 256;
  BIT9                = 512;
  BIT10               = 1024;
  BIT11               = 2046;
  BIT12               = 4096;
  BIT13               = 8192;
  BIT14               = 16384;
  BIT15               = 32768;

  // can
  DEFAULTCANTIMEOUT   = 500;

  // can-open
  PDO_CANID_TX        = $180;
  PDO_CANID_RX        = $200;
  SDO_CANID_TX        = $580;
  SDO_CANID_RX        = $600;
  SDO_readrequest     = $40;
  SDO_readresponse8   = $4f;
  SDO_readresponse16  = $4b;
  SDO_readresponse32  = $43;
  SDO_writerequest8   = $2f;
  SDO_writerequest16  = $2b;
  SDO_writerequest32  = $23;
  SDO_writeresponse   = $60;            // always same
  DEAULTSDOTIMEOUT    = 1000;

type
  // can message record
  TCanMessage = record
    Id: integer;                        // message index (ex. 0x0187)
    Len: integer;                       // can message length
    Data: array[0..7] of Integer;       // data bytes
    Remote: boolean;                    // true if was remote frame
    Write: boolean;                     // true if was written by this app
    TimeMS: Double;
  end;

  // object to have onlog events
  // usage: Create object on application create and then assign oncanmessage
  //        event to the similar event added to the form, all that you need
  //        to do after that is to apply anything you need from the msg
  // note:  CanObject is already defined as variable

const
  MAXCANBUFFERCOUNT = 4096; // 4k for temporary buffer

type

  TOnCanMessage = procedure(Const msg: TCanMessage) of object;

  TCanLogBuffer = record
    Count: Integer;
    Item: array of TCanMessage;
  end;

  TCanObject = class(tobject)
    private
      FLock: tcriticalsection;
      FOnCanMessage: TOnCanMessage;
      FBuffer: TCanLogBuffer;
      FBufferOverrun: integer;
    protected
    public
      constructor Create;
      destructor Destroy; override;
      // functions
      procedure ClearCanBuffer;
      procedure AddMsgToBuffer(msg: TCanMessage);
      procedure ExportAndClearBuffer(var list: TCanLogBuffer);
      function  BufferCount: integer;
      function  BufferOverrun: integer;
      // events
      property OnCanMessage: TOnCanMessage read FOnCanMessage write FOnCanMessage;
  end;

var
  CanObject: TCanObject;

// miscellaneous general use function(s):
function CanmsgToString(temp: tcanmessage): string;

// initialization
procedure can_init;                     // initialize can unit
procedure can_clear; overload;          // clear all statistics
procedure can_clear(var msg: tcanmessage); overload;
function  can_open(baudrate: integer): boolean; overload;
function  can_open: boolean; overload;  // check can open state
function  can_reopen: boolean;          // re-open the can dongle
procedure can_reset;                    // reset dongle driver
procedure can_heavy(state: boolean); overload;
function  can_heavy: boolean; overload; // bus heavy status
procedure can_int_close;
procedure can_close;                    // close driver
procedure can_kill;                     // free can unit

procedure can_filter( start, stop: integer );
procedure can_clearfilter;              // clear above defined filter

// internal use only
procedure int_can_clearbuffers;             // read until can buffer is empty
function  int_can_send(msg: tcanmessage): boolean;
function  int_can_read(var msg: tcanmessage): boolean; overload;
function  int_can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer = DEFAULTCANTIMEOUT): boolean; overload;

// communication
procedure can_clearbuffers;             // read until can buffer is empty
function  can_send(msg: tcanmessage): boolean;
function  can_read(var msg: tcanmessage): boolean; overload;
function  can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer = DEFAULTCANTIMEOUT): boolean; overload;
function  can_custom(var msg: tcanmessage; filter: tcanmessage; timeout: integer = DEFAULTCANTIMEOUT): boolean;

procedure can_sdotimeout(const valueMS: integer); overload;
function  can_sdotimeout: integer; overload;
function  can_sdoread(const node, obj, sub: integer; var value: integer): boolean; overload;
function  CAN_SdoRead(const node, obj, sub: integer; var value: string): boolean; overload;
function  can_sdosend(const node, obj, sub, value: integer; Bits: integer = 16): boolean; overload;
function  CAN_SdoSend(const node, obj, sub: integer; value: string): boolean; overload;

// miscellous
procedure can_incsentmessages(msg: tcanmessage);
procedure can_increadmessages(msg: tcanmessage);
function  can_readmessages: int64;      // total messages received
function  can_bytesread: int64;         // total bytes received
function  can_sentmessages: int64;      // total messages sent
function  can_bytessent: int64;         // total bytes sent
function  can_lastmessage: tcanmessage; // last message content
function  can_lastmsgtime: tdatetime;   // last message time stamp
function  can_resetcount: integer;      // how many times dongle has been reset?
procedure can_incresetcount;

// static list, note that statlist is currently formmated to be outputted
// to api_listbox component with certain number of columns -> if outputted
// to something else this procedure must be changed.
procedure can_statclear;                // clear statistics
function  can_statcount: integer;       // items in the statistics listing
procedure can_statadd(msg: tcanmessage);
procedure can_internal_statadd(msg: tcanmessage);
procedure can_statlist(list: tstrings);    // export static listing (to api_listbox)
function  can_statmsgtimedif(id: integer): double;

implementation

uses
  PCanLight, {PCANLight.pas}
  DateUtils, API_Timer, api_base;

type
  TStatitem = record
    timestamp: double;                    // time stamp in millisecs
    oldtimestamp: double;                 // previous message timestamp
    count: integer;                       // message count with same id and length
    msg: tcanmessage;                     // last message contents
  end;

  TData = record
    lock: tcriticalsection;               // critical section for threaded access
    open: boolean;                        // device initialized
    readmessages: int64;                  // amount of read messages
    bytesread: int64;                     // bytes read total
    sentmessages: int64;                  // amount of sent messages
    bytessent: int64;                     // bytes written total
    lastmessage: tcanmessage;             // last message content
    lastmsgtime: tdatetime;                   // last message time stamp
    resetcount: integer;                  // how many times bus is resetted
    statcount: integer;                   // amount of messages
    statlist: array of tstatitem;         // array of can messages
    timer: tapi_timer;                    // timer for the intervals
    sdo_timeout: integer;
    baudrate: integer;                    // baudrate in use
    bus_heavy: boolean;                   // bus heavy load
  end;

var
  data: TData;
  can: Canlight;    {PCANLight.pas}

//------------------------------------------------------------------------------
function CanmsgToString(temp: tcanmessage): string;
begin
  result:= inttohex(temp.Id,4)+', ';
  result:= result + inttostr(temp.Len)+' bytes ';
  if temp.Len>0 then result:= result + '['+ inttohex(temp.Data[0],2);
  if temp.len>1 then result:= result + ' '+ inttohex(temp.Data[1],2);
  if temp.len>2 then result:= result + ' '+ inttohex(temp.Data[2],2);
  if temp.len>3 then result:= result + ' '+ inttohex(temp.Data[3],2);
  if temp.len>4 then result:= result + ' '+ inttohex(temp.Data[4],2);
  if temp.len>5 then result:= result + ' '+ inttohex(temp.Data[5],2);
  if temp.len>6 then result:= result + ' '+ inttohex(temp.Data[6],2);
  if temp.len>7 then result:= result + ' '+ inttohex(temp.Data[7],2);
  if temp.len>0 then result:= result + ']';
end;

//------------------------------------------------------------------------------
constructor TCanObject.Create;
begin
  inherited Create;
  flock:= tcriticalsection.create;
  ClearCanBuffer;
end;

destructor TCanObject.Destroy;
begin
  flock.free;
  inherited Destroy;
end;

procedure TCanObject.ClearCanBuffer;
begin
  FLock.Acquire;
  try
    self.FBuffer.Count:= 0;
    setlength(self.FBuffer.Item, self.fbuffer.Count);
  finally
    flock.release;
  end;
end;

procedure TCanObject.ExportAndClearBuffer(var list: TCanLogBuffer);
var
  i: integer;
begin
  // copies all data to list buffer from the internal one
  // and clears the internal one to receive more data
  Flock.acquire;
  try
    // copy to list
    list.Count:= fbuffer.count;
    setlength(list.Item, list.count);
    for i:=0 to list.count-1 do
     list.Item[i]:= fbuffer.Item[i];
    // clear internal
    FBuffer.Count:= 0;
    setlength(FBuffer.Item, fbuffer.Count);
  finally
    FLock.Release;
  end;
end;

procedure TCanObject.AddMsgToBuffer(msg: TCanMessage);
begin
  flock.acquire;
  try
    if fbuffer.count<MAXCANBUFFERCOUNT then
    begin
      // add message to buffer
      fbuffer.count:= fbuffer.count+1;
      setlength(fbuffer.item, fbuffer.count);
      fbuffer.item[fbuffer.count-1]:= msg;
    end else
    begin
      // clear & increase overrun counter
      fbuffer.count:= 0;
      setlength(fbuffer.item, fbuffer.count);
      fbufferoverrun:= fbufferoverrun + 1;
    end;
  finally
    flock.release;
  end;
end;

function TCanObject.BufferCount: integer;
begin
  flock.acquire;
  try
    result:= fbuffer.count;
  finally
    flock.release;
  end;
end;

function TCanObject.BufferOverrun: integer;
begin
  flock.acquire;
  try
    result:= fbufferoverrun;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_init;
begin
  can:= canlight.Create;                  // init device
  canobject:= tcanobject.create;          // create object
  data.lock:= tcriticalsection.create;    // create critical section
  data.timer:= tapi_timer.create(nil);    // create timer
  data.open:= false;                      // device not open yet
  data.resetcount:= 0;                    // reset count to zero
  data.sdo_timeout:= 500;
  can_clear;                              // clear all variables
end;

//------------------------------------------------------------------------------
procedure can_kill;
begin
  if can_open then can_close;             // close if was open
  data.timer.free;                        // free timer
  canobject.free;
  can.Free;                               // free device
  data.lock.free;                         // free critical section
end;

//------------------------------------------------------------------------------
procedure int_can_clear;
begin
  data.readmessages:= 0;                // read messages
  data.bytesread:= 0;                   // total size of read messages
  data.sentmessages:= 0;
  data.bytessent:= 0;
  data.lastmsgtime:= 0;
  data.statcount:= 0;                   // empty static list
  setlength(data.statlist, data.statcount);
  can_clear(data.lastmessage);          //
  data.timer.Stop;                      // make sure timer is stopped
  data.timer.Start;                     // start timer
end;

procedure can_clear;
begin
  data.lock.Acquire;
  try
    int_can_clear;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_clear(var msg: tcanmessage);
var
  i: integer;
begin
  msg.id:= -1;                            // set id to -1
  msg.len:= -1;                           // set length to -1
  for i:=0 to 7 do                        // go trough contents
    msg.data[i]:= -1;                     // clear data to -1
end;

//------------------------------------------------------------------------------
function can_int_open( baudrate: integer ): boolean;
var
  bdr: Baudrates;
begin
  if data.open then can_int_close;

  case baudrate of                      // check for valid baudrate
    1000: bdr:= BAUD_1M;
    500:  bdr:= BAUD_500K;
    250:  bdr:= BAUD_250K;
    125:  bdr:= BAUD_125K;
    100:  bdr:= BAUD_100K;
    50:   bdr:= BAUD_50K;
    20:   bdr:= BAUD_20K;
    10:   bdr:= BAUD_10K;
    5:    bdr:= BAUD_5K;
    else  bdr:= BAUD_500K;
  end;

  data.baudrate:= baudrate;             // store old baudrate

  if can.Init(USB_1CH, bdr, INIT_TYPE_ST) = ERR_OK then
  begin
    data.open:= true;
    int_can_clear;
    result:= true;
  end else
    result:= false;
end;

function can_open( baudrate: integer ): boolean;
begin
  data.lock.Acquire;                      // enter critical section
  try
    result:= can_int_open(baudrate);
  finally
    data.lock.release;                    // release critical section
  end;
end;

//------------------------------------------------------------------------------
function can_int_reopen: boolean;
var
  baudrate: integer;
begin
  baudrate:= data.baudrate;             // get previous baudrate
  result:= can_int_open(baudrate);            // reopen hardware
end;

function can_reopen: boolean;
begin
  data.lock.acquire;                      // acquire lock
  try
    result:= can_int_reopen;
  finally
    data.lock.release;                    // free lock
  end;
end;

//------------------------------------------------------------------------------
function can_open: boolean;
begin
  data.lock.acquire;
  try
    result:= data.open;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_reset;
var
  res: canresult;   {PCANLight.pas}
begin
  data.lock.acquire;
  try
    res:= can.ResetClient(USB_1CH);         // reset (reopen) can device
    if res=ERR_OK then
    begin
      int_can_clear;
      data.resetcount:= data.resetcount + 1;  // increase reset counter
    end;
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_int_close;
var
  res: canresult;   {PCANLight.pas}
begin
  res:= can.Close(USB_1CH);               // close can device handle
  if res=ERR_OK then
  begin
    data.open:= false;                    // set open variable to false
    data.timer.Stop;                      // and stop timer from running
  end;
end;

procedure can_close;
begin
  data.lock.Acquire;
  try
    can_int_close;
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_clearfilter;
begin
  can.ResetFilter(USB_1CH);
end;

//------------------------------------------------------------------------------
procedure can_filter( start, stop: integer );
begin
  can.ResetFilter(USB_1CH);
  can.MsgFilter(USB_1CH, start, stop, MSGTYPE_STANDARD);
end;

//------------------------------------------------------------------------------
procedure can_int_incsentmessages(var msg: tcanmessage);
begin
  data.sentmessages:= data.sentmessages + 1;
  data.lastmsgtime:= now;
  data.lastmessage:= msg;
  data.bytessent:= data.bytessent + msg.len;
  can_internal_statadd(msg);
end;

procedure can_incsentmessages(msg: tcanmessage);
begin
  data.lock.Acquire;
  try
    can_int_incsentmessages(msg);
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_int_increadmessages(var msg: tcanmessage);
begin
  data.readmessages:= data.readmessages + 1;
  data.lastmsgtime:= now;
  data.lastmessage:= msg;
  data.bytesread:= data.bytesread + msg.len;
  can_internal_statadd(msg);
end;

procedure can_increadmessages(msg: tcanmessage);
begin
  data.lock.Acquire;
  try
    can_int_increadmessages(msg);
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_incresetcount;
begin
  data.lock.Acquire;
  try
    data.resetcount:= data.resetcount + 1;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_readmessages: int64;
begin
  data.lock.Acquire;
  try
    result:= data.readmessages;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_bytesread: int64;
begin
  data.lock.Acquire;
  try
    result:= data.bytesread;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_sentmessages: int64;
begin
  data.lock.Acquire;
  try
    result:= data.sentmessages;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_lastmessage: tcanmessage;
begin
  data.lock.Acquire;
  try
    result:= data.lastmessage;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_resetcount: integer;
begin
  data.lock.Acquire;
  try
    result:= data.resetcount;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_lastmsgtime: tdatetime;
begin
  data.lock.Acquire;
  try
    result:= data.lastmsgtime;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_bytessent: int64;
begin
  data.lock.Acquire;
  try
    result:= data.bytessent;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_int_statuscheck(res: canresult): boolean; // result = ST_OK
var
  bus_heavy: boolean;
begin
  bus_heavy:= false;
  // bus totally off
  if (word(res) and word(ERR_BUSOFF)>0) then
  begin
    // in case bus goes off for some reason, we need to
    // reset client to get it back working correctly!
    can_int_reopen;                         // reopen dongle driver
    data.resetcount:= data.resetcount + 1;  // increase reset counter
  end else
  // bus light or heavy
  if (word(res) and word(ERR_BUSHEAVY)>0) then // or (res and ERR_BUSLIGHT>0) then
  begin
    // bus load is heavy; this usually means that there
    // is some trouble on wiring of can bus (for example
    // terminator missing)
    bus_heavy:= true;
  end;
  data.bus_heavy:= bus_heavy;                     // set can bus heavy flag true
  result:= (res=ERR_OK);
end;

function can_statuscheck(res: canresult): boolean; // result = ST_OK
begin
  data.lock.acquire;
  try
    result:= can_int_statuscheck(res);
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function can_heavy: boolean;
begin
  data.lock.acquire;
  try
    result:= data.bus_heavy;
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_heavy(state: boolean);
begin
  data.lock.acquire;
  try
    data.bus_heavy:= state;
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function int_can_send(msg: tcanmessage): boolean;
var
  m: TPCANMsg;
  i: integer;
  res: canresult; {PCANLight.pas}
begin
  m.ID:= msg.id;                          // set message can-id
  m.MSGTYPE:= byte(MSGTYPE_STANDARD);     // standard message
  m.LEN:= msg.len;                        // set message length
  for i:=0 to m.LEN-1 do                  // fill in the data
    m.data[i]:= msg.data[i];
  res:= can.Write(USB_1CH, m);
  if can_int_statuscheck(res) then
  begin
    msg.Write:= TRUE;
    msg.Remote:= FALSE;
    can_int_incsentmessages(msg);             // increase write counter
    result:= true;
  end else
    result:= false;
end;

function can_send(msg: tcanmessage): boolean;
begin
  data.lock.Acquire;
  try
    result:= int_can_send(msg);
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function int_can_read(var msg: tcanmessage): boolean;
var
  m: TPCANMsg;
  i: integer;
  res: canresult; {PCANLight.pas}
begin
  result:= FALSE;
  res:= can.Read(USB_1CH, m);
  if can_int_statuscheck(res) then                        // read available can frame
  begin
    if (MsgTypes(m.MSGTYPE)=MSGTYPE_STANDARD) or (MsgTypes(m.MSGTYPE)=MSGTYPE_RTR) then
    begin
      msg.id:= m.ID;
      msg.Write:= FALSE;                              // not written, right?
      if (MsgTypes(m.MSGTYPE)=MSGTYPE_STANDARD) then                          // if not remote frame
      begin
        msg.Remote:= FALSE;
        msg.len:= m.LEN;
        for i:=0 to m.LEN-1 do msg.data[i]:= m.data[i];
      end else
      begin
        msg.Remote:= TRUE;
        msg.len:= 0;                                  // no contents for remote frame
      end;
      can_int_increadmessages(msg);                       // increase read counter
      result:= TRUE;
    end;
  end; // read ok
end;

function can_read(var msg: tcanmessage): boolean;
begin
  data.lock.Acquire;
  try
    result:= int_can_read(msg);
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure int_can_clearbuffers;
var
  msg: tcanmessage;
  m: TPCANMsg;
  i: integer;
  res: canresult; {PCANLight.pas}
begin
  repeat
    res:= can.Read(USB_1CH, m);
    if can_int_statuscheck(res) then
    begin
      if (MsgTypes(m.MSGTYPE)=MSGTYPE_STANDARD) or (MsgTypes(m.MSGTYPE)=MSGTYPE_RTR) then
      begin
        msg.write:= FALSE;
        msg.id:= m.ID;
        if not (MsgTypes(m.MSGTYPE)=MSGTYPE_RTR) then
        begin
          msg.Remote:= FALSE;
          msg.len:= m.LEN;
          for i:=0 to m.LEN-1 do msg.data[i]:= m.data[i];
        end else
        begin
          msg.len:= 0;
          msg.Remote:= TRUE;
        end;
        can_int_increadmessages(msg);
      end;
    end;
  until (res=CANResult(ERR_QRCVEMPTY)); // repeat until receive queue is empty
end;

procedure can_clearbuffers;
begin
  data.lock.Acquire;
  try
    if not data.open then exit;
    int_can_clearbuffers;
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function int_can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer): boolean;
var
  start: integer;
  done: boolean;
  i: integer;
begin
  start:= gettickcount;
  done:= false;
  while (not done) and (cardinal(start+timeout)>gettickcount) do
  begin
    if int_can_read(msg) then
    begin
      done:= true;
      if (filter.id>-1) and (msg.id<>filter.id) then done:= false;
      if (filter.len>-1) and (msg.len<>filter.len) then done:= false;
      for i:=0 to 7 do
        if (filter.data[i]>-1) and (msg.data[i]<>filter.data[i]) then done:= false;
    end;
  end;
  int_can_clearbuffers; // empty buffer before leaving this routing (29052008)
  result:= done;
end;

function can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer): boolean;
begin
  data.lock.Acquire;
  try
    result:= int_can_read(msg, filter, timeout);
  finally
    data.lock.release;
  end;
end;

//------------------------------------------------------------------------------
function can_custom(var msg: tcanmessage; filter: tcanmessage; timeout: integer): boolean;
begin
  result:= false;

  data.lock.Acquire;
  try
    // we need to clear buffers before going into next phase
    // to not to gather too much stuff into the peak device's
    // buffers in case the reading requires some more time
    // than expected (17.8.2007, for testing, ari)
    int_can_clearbuffers;

    // actual request
    if int_can_send(msg) then
      if int_can_read(msg, filter, timeout) then
        result:= true;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_sdotimeout(const valueMS: integer);
begin
  data.lock.Acquire;
  try
    data.sdo_timeout:= valueMS;
  finally
    data.lock.Release;
  end;
end;

function can_sdotimeout: integer;
begin
  data.lock.Acquire;
  try
    result:= data.sdo_timeout;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function can_sdoread(const node, obj, sub: integer; var value: integer): boolean;
var
  m: tcanmessage;
  f: tcanmessage;
begin
  // create request
  m.id:= sdo_canid_rx + node;
  m.len:= 8;
  m.data[0]:= sdo_readrequest;
  m.data[1]:= (obj and $ff);
  m.data[2]:= (obj and $ff00) shr 8;
  m.data[3]:= (sub and $ff);
  m.data[4]:= 0;
  m.data[5]:= 0;
  m.data[6]:= 0;
  m.data[7]:= 0;

  // create filter
  can_clear(f);
  f.id:= sdo_canid_tx + node;
  f.data[1]:= m.data[1];
  f.data[2]:= m.data[2];
  f.data[3]:= m.data[3];

  // do query
  if can_custom(m, f, can_sdotimeout) and (m.data[0] < $80) then
  begin
    value:= m.data[4];
    value:= value+(m.data[5] shl 8);
    value:= value+(m.data[6] shl 16);
    value:= value+(m.data[7] shl 24);
    result:= true;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function CAN_SdoRead(const node, obj, sub: integer; var value: string): boolean;
var
  command, response, p, v: byte;
  l: integer;
  m: tcanmessage;
  f: tcanmessage;
  toggle: boolean;
begin
  result:= false;
  m.id:= sdo_canid_rx + node;
  m.len:= 8;
  value:= '';

  //  Read Dictionary object =
  // aaa  	010 (ccs = 2: initiate upload response)
  // bbbbb  	00000 (not used)
  //  command = 01000000 = $40
  command:=   $40;
  m.data[0]:= command;
  m.data[1]:= (obj and $ff);
  m.data[2]:= (obj and $ff00) shr 8;
  m.data[3]:= (sub and $ff);
  m.data[4]:= 0;
  m.data[5]:= 0;
  m.data[6]:= 0;
  m.data[7]:= 0;

  // reply
  can_clear(f);
  f.id:= sdo_canid_tx + node;
  response:=  $41;
  f.data[0]:= response;
  f.data[1]:= m.data[1];
  f.data[2]:= m.data[2];
  f.data[3]:= m.data[3];
  if not can_custom(m, f, can_SDOtimeout) then
  begin
    // send error
    exit;
  end;

  // read length
  l:= m.data[4];
  l:= l + m.data[5] shl 8;
  l:= l + m.data[6] shl 16;
  l:= l + m.data[7] shl 24;

  //  start write loop

  //  Command: aaabccce  	CAL Multiplexed Domain Transfer Protocol
  //  aaa  	000 scs = 0: download segment request
  //  b  	  toggle bit. The toggle bit must alternate for each subsequent segment request (first request has toggle bit set to 0). The slave response copies the master toggle bit. An incorrect toggle bit initiates a Abort Transfer.
  //  ccc  	number of data bytes ([9-n to 8]) that do NOT contain data
  //  e  	  1 indicates last segment
  command:= $60; // clear command
  toggle:= FALSE;

  p:= 0;
  while (p<l) do // while all bytes written
  begin

    // toggle bit
    toggle:= not toggle;
    setbit(command, 4, toggle);

    // set eof if comes next
    if (p+4>=l) then setbit(command, 8, TRUE); // end of write

    // read next block
    m.data[0]:= command;
    m.data[1]:= 0;
    m.data[2]:= 0;
    m.data[3]:= 0;
    m.data[4]:= 0;
    m.data[5]:= 0;
    m.data[6]:= 0;
    m.data[7]:= 0;

    // Command: aaabcces  	CAL Multiplexed Domain Transfer Protocol
    //  aaa  	001 (scs = 1: download segment response)
    //  b  	  1/0 (toggle bit)
    //  cccc  0000 (not used)
    can_clear(f);
    f.id:= sdo_canid_tx + node;
    if not can_custom(m, f, can_SDOtimeout) then
    begin
      // send error
      exit;
    end;

    p:= p + 1; if p<l then begin v:= m.data[1]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[2]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[3]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[4]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[5]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[6]; value:= value + chr(v); end;
    p:= p + 1; if p<l then begin v:= m.data[7]; value:= value + chr(v); end;

  end; // while loop

  result:= true;
end;


//------------------------------------------------------------------------------
function  can_sdosend(const node, obj, sub, value: integer; Bits: integer = 16): boolean;
var
  m: tcanmessage;
  f: tcanmessage;
begin
  // create request
  m.id:= sdo_canid_rx + node;
  m.len:= 8;
  if (Bits = 8) then m.data[0]:= sdo_writerequest8
    else if (Bits = 16) then m.data[0]:= sdo_writerequest16
      else if (Bits = 32) then m.data[0]:= sdo_writerequest32
        else m.data[0]:= $00;
  m.data[1]:= (obj and $ff);
  m.data[2]:= (obj and $ff00) shr 8;
  m.data[3]:= (sub and $ff);
  m.data[4]:= (value and $000000ff);
  m.data[5]:= (value and $0000ff00) shr 8;
  m.data[6]:= (value and $00ff0000) shr 16;
  m.data[7]:= (value and $ff000000) shr 24;

  // create filter
  can_clear(f);
  f.id:= sdo_canid_tx + node;
  f.data[0]:= sdo_writeresponse;
  f.data[1]:= m.data[1];
  f.data[2]:= m.data[2];
  f.data[3]:= m.data[3];

  // try set value
  if can_custom(m, f, can_sdotimeout) then
  begin
    result:= true;
  end else
    result:= false;
end;

//------------------------------------------------------------------------------
function CAN_SdoSend(const node, obj, sub: integer; value: string): boolean;
var
  command, response, p, v: byte;
  l: integer;
  m: tcanmessage;
  f: tcanmessage;
  toggle: boolean;
begin
  result:= false;
  m.id:= sdo_canid_rx + node;
  m.len:= 8;

  //  Read Dictionary object =
  // aaa  	010 (ccs = 2: initiate upload response)
  // bbbbb  	00000 (not used)
  //  command = 01000000 = $40

  //  Command: aaabcces  	CAL Multiplexed Domain Transfer Protocol
  //  aaa  	001   (ccs = 1: initiate upload response)
  //  b  	  0     (not used)
  //  cc  	00    (not valid)
  //  e  	  0     (e=0: normal transfer)
  //  s  	  1     (s = 1: data set size is indicated)
  command:=   $21;  //001 0 00 0 1;
  m.data[0]:= command;
  m.data[1]:= (obj and $ff);
  m.data[2]:= (obj and $ff00) shr 8;
  m.data[3]:= (sub and $ff);
  l:= length(value); // get length
  m.data[4]:= (l and $000000ff);
  m.data[5]:= (l and $0000ff00) shr 8;
  m.data[6]:= (l and $00ff0000) shr 16;
  m.data[7]:= (l and $ff000000) shr 24;

  //  Reply: aaabbbbb  	CAL Multiplexed Domain Transfer Protocol
  //  aaa  	011   (scs = 3: initiate upload response)
  //  bbbbb 00000 (not used)
  can_clear(f);
  f.id:= sdo_canid_tx + node;
  response:=  $60;  // 011 00000
  f.data[0]:= response;
  f.data[1]:= m.data[1];
  f.data[2]:= m.data[2];
  f.data[3]:= m.data[3];
  if not can_custom(m, f, can_SDOtimeout) then
  begin
    // send error
    exit;
  end;

  //  start write loop

  //  Command: aaabccce  	CAL Multiplexed Domain Transfer Protocol
  //  aaa  	000 scs = 0: download segment request
  //  b  	  toggle bit. The toggle bit must alternate for each subsequent segment request (first request has toggle bit set to 0). The slave response copies the master toggle bit. An incorrect toggle bit initiates a Abort Transfer.
  //  ccc  	number of data bytes ([9-n to 8]) that do NOT contain data
  //  e  	  1 indicates last segment
  command:= 0; // clear command
  toggle:= FALSE;

  p:= 0;
  while (p<l) do // while all bytes written
  begin

    // toggle bit
    toggle:= not toggle;
    setbit(command, 4, toggle);

    // set eof if comes next
    if (p+7>=l) then setbit(command, 8, TRUE); // end of write

    // write next block
    m.data[0]:= command;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[1]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[2]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[3]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[4]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[5]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[6]:= v;
    p:= p + 1; if p<l then v:= ord(value[p]) else v:=0; m.data[7]:= v;

    // Command: aaabcces  	CAL Multiplexed Domain Transfer Protocol
    //  aaa  	001 (scs = 1: download segment response)
    //  b  	  1/0 (toggle bit)
    //  cccc  0000 (not used)
    can_clear(f);
    f.id:= sdo_canid_tx + node;
    response:=  $20; //00100000 + toggle
    setbit(response, 4, toggle);
    f.data[0]:= response;
    if not can_custom(m, f, can_SDOtimeout) then
    begin
      // send error
      exit;
    end;

  end; // while loop

  result:= true;
end;

//------------------------------------------------------------------------------
procedure can_statclear;
begin
  can_clear;
end;

//------------------------------------------------------------------------------
function can_statcount: integer;
begin
  data.lock.Acquire;
  try
    result:= data.statcount;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
// must be always called inside the lock!
procedure can_internal_statadd(msg: tcanmessage);
var
  i,j: integer;
  t: tstatitem;
begin
  // last message time stamp
  msg.TimeMS:= data.timer.ElapsedMs;
  data.lastmsgtime:= msg.timeMS;

  // store to buffer
  CanObject.AddMsgToBuffer(msg);

  // can message object, ext event
  if assigned(CanObject.OnCanMessage) then
    CanObject.OnCanMessage(msg);    // launch event

  // check if already exists in list
  for i:=0 to data.statcount-1 do
    if (data.statlist[i].msg.id = msg.id) then
    begin
      data.statlist[i].oldtimestamp:= data.statlist[i].timestamp;
      data.statlist[i].timestamp:= data.timer.ElapsedMs;
      data.statlist[i].count:= data.statlist[i].count + 1;
      data.statlist[i].msg:= msg;
      exit;
    end;

  // create new entry
  data.statcount:= data.statcount + 1;
  setlength(data.statlist, data.statcount);
  data.statlist[data.statcount-1].oldtimestamp:= 0;
  data.statlist[data.statcount-1].timestamp:= data.timer.ElapsedMs;
  data.statlist[data.statcount-1].count:= 1;
  data.statlist[data.statcount-1].msg:= msg;

  // sort static list
  for j:=0 to data.statcount-1 do
    for i:=0 to j do
      if (data.statlist[i].msg.id>data.statlist[j].msg.id) then
      begin
        t:= data.statlist[i];
        data.statlist[i]:= data.statlist[j];
        data.statlist[j]:= t;
      end;
end;

//------------------------------------------------------------------------------
procedure can_statadd(msg: tcanmessage);
begin
  data.lock.Acquire;
  try
    can_internal_statadd(msg);
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
function  can_statmsgtimedif(id: integer): double;
var
  i: integer;
begin
  data.lock.Acquire;
  try
    result:= -1;
    for i:=0 to data.statcount-1 do
      if data.statlist[i].msg.id = id then
      begin
        result:= data.timer.ElapsedMs - data.statlist[i].timestamp;
        break;
      end;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_statlist(list: tstrings);
var
  i,j: integer;
  s,ss,t: string;
  flags: string;
begin
  data.lock.Acquire;
  try
    list.clear;

    // add items into the list
    for i:=0 to data.statcount-1 do
    begin

      // format data as string
      s:= '';
      ss:= '';
      for j:=0 to 7 do
      begin
        if data.statlist[i].msg.len>j then begin
          s:=s+inttohex(data.statlist[i].msg.data[j],2)+' ';
          if (data.statlist[i].msg.data[j]>32) then
            ss:= ss + chr(data.statlist[i].msg.data[j])
            else ss:= ss + ' ';
        end;
      end;

      flags:= '';
      if data.statlist[i].msg.Remote then flags:= 'R';
      if data.statlist[i].msg.Write then flags:= 'W';

      // add to list, formatted to be shown
      // on the api_listbox component
      t:= formatfloat('0.0',data.statlist[i].timestamp/1000)+'||'+
          '0x'+inttohex(data.statlist[i].msg.id,4)+'||'+
          inttostr(data.statlist[i].msg.len)+'||'+
          s+'||'+
          inttostr(trunc(data.statlist[i].timestamp-data.statlist[i].oldtimestamp))+'||'+
          inttostr(data.statlist[i].count)+'||'+
          ss+'||'+
          flags+'||';

      // mark old texts (this is another feature
      // that is only available on api_listbox
      // component to make some lines old or disabled after some
      // period of time)
      if (data.timer.ElapsedMs-data.statlist[i].timestamp>10000) or (data.statlist[i].timestamp=0) then
        t:= '<!--'+t;

      // add to stringlist
      list.Add(t);

    end;

  finally
    data.lock.Release;
  end;
end;

end.
