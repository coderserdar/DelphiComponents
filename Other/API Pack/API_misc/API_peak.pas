unit API-peak;

//------------------------------------------------------------------------------
//
// PEAK CAN DONGLE INTERFACE UNIT (see: http://www.peak-system.com/)
// ---------------
//
// before you're able to use this as interface unit for easier access to the
// peak's usb-can dongle threadsafe, peak drivers must be installed because
// this uses dll trough the peaks example interface unit (pcanlight). NOTE!
// you need to donwload driver for your CAN-USB dongle to obtain the delphi
// unit mentioned above.
//
// regards,
// ari pikivirta
//
//------------------------------------------------------------------------------
//
// HISTORY:
// --------
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
  sysutils, classes;

const
  // bit definitions
  BIT0              = 1;
  BIT1              = 2;
  BIT2              = 4;
  BIT3              = 8;
  BIT4              = 16;
  BIT5              = 32;
  BIT6              = 64;
  BIT7              = 128;
  BIT8              = 256;
  BIT9              = 512;
  BIT10             = 1024;
  BIT11             = 2046;
  BIT12             = 4096;
  BIT13             = 8192;
  BIT14             = 16384;
  BIT15             = 32768;

  // can
  DEFAULTCANTIMEOUT = 500;

  // can-open
  PDO_CANID_TX      = $180;
  PDO_CANID_RX      = $200;
  SDO_CANID_TX      = $580;
  SDO_CANID_RX      = $600;
  SDO_readrequest   = $40;
  SDO_writerequest0 = $2f;              // 8 bit
  SDO_writerequest1 = $2b;              // 16 bit
  SDO_writerequest2 = $23;              // 32 bit
  SDO_writeresponse = $60;              // always same
  DEAULTSDOTIMEOUT  = 1000;

type
  // can message contents
  TCanMessage = record
    Id: integer;                        // message index (ex. 0x0187)
    Len: integer;                       // can message length
    Data: array[0..7] of integer;       // data bytes
  end;

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
procedure can_close;                    // close driver
procedure can_kill;                     // free can unit

procedure can_filter( start, stop: integer );
procedure can_clearfilter;              // clear above defined filter

// communication
procedure can_clearbuffers;             // read until can buffer is empty
function  can_send(msg: tcanmessage): boolean;
function  can_read(var msg: tcanmessage): boolean; overload;
function  can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer = DEFAULTCANTIMEOUT): boolean; overload;
function  can_custom(var msg: tcanmessage; filter: tcanmessage; timeout: integer = DEFAULTCANTIMEOUT): boolean;

procedure can_sdotimeout(const valueMS: integer); overload;
function  can_sdotimeout: integer; overload;
function  can_sdoread(const node, obj, sub: integer; var value: integer): boolean;
function  can_sdosend(const node, obj, sub, value: integer; Bits: integer = 16): boolean;

// miscellous
procedure can_incsentmessages(msg: tcanmessage);
procedure can_increadmessages(msg: tcanmessage);
function  can_readmessages: int64;      // total messages received
function  can_bytesread: int64;         // total bytes received
function  can_sentmessages: int64;      // total messages sent
function  can_bytessent: int64;         // total bytes sent
function  can_lastmessage: tcanmessage; // last message content
function  can_lastmsgtime: double;      // last message time stamp
function  can_resetcount: integer;      // how many times dongle has been reset?
procedure can_incresetcount;

// static list, note that statlist is currently formmated to be outputted
// to api_listbox component with certain number of columns -> if outputted
// to something else this procedure must be changed.
procedure can_statclear;                // clear statistics
function  can_statcount: integer;       // items in the statistics listing
procedure can_statadd(msg: tcanmessage);
procedure can_internal_statadd(msg: tcanmessage);
function  can_statlist: tstringlist;    // export static listing (to api_listbox)
function  can_statmsgtimedif(id: integer): double;

implementation

uses
  PCanLight,  {PCANLight.pas}
  DateUtils, API_Timer, SyncObjs;

type
  tstatitem = record
    timestamp: double;                    // time stamp in millisecs
    oldtimestamp: double;                 // previous message timestamp
    count: integer;                       // message count with same id and length
    msg: tcanmessage;                     // last message contents
  end;

  tdata = record
    lock: tcriticalsection;               // critical section for threaded access
    open: boolean;                        // device initialized
    readmessages: int64;                  // amount of read messages
    bytesread: int64;                     // bytes read total
    sentmessages: int64;                  // amount of sent messages
    bytessent: int64;                     // bytes written total
    lastmessage: tcanmessage;             // last message content
    lastmsgtime: tdatetime;               // last message time stamp
    resetcount: integer;                  // how many times bus is resetted
    statcount: integer;                   // amount of messages
    statlist: array of tstatitem;         // array of can messages
    timer: tapi_timer;                    // timer for the intervals
    sdo_timeout: integer;
    baudrate: integer;                    // baudrate in use
    bus_heavy: boolean;                   // bus heavy load
  end;

var
  data: tdata;
  can: canlight;    {PCANLight.pas}
  res: canresult;   {PCANLight.pas}

//------------------------------------------------------------------------------
procedure can_init;
begin
  can:= canlight.Create;                  // init device
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
  data.lock.free;                         // free critical section
  can.Free;                               // free device
end;

//------------------------------------------------------------------------------
procedure can_clear;
begin
  data.lock.Acquire;
  try
    data.readmessages:= 0;                // read messages
    data.bytesread:= 0;                   // total size of read messages
    data.sentmessages:= 0;
    data.bytessent:= 0;
    data.lastmsgtime:= 0;
    data.statcount:= 0;                   // empty static list
    setlength(data.statlist, data.statcount);
    can_clear(data.lastmessage);          //
    // restart timer
    data.timer.Stop;                      // make sure timer is stopped
    data.timer.Start;                     // start timer
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
function can_open( baudrate: integer ): boolean;
var
  bdr: Baudrates;
begin
  if can_open then can_close;             // close first

  data.lock.Acquire;                      // enter critical section
  try
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
    res:= can.Init(USB, bdr, INIT_TYPE_ST);
    result:= (res=ERR_OK);                // apply result
    data.open:= result;                   // open flag according to result
  finally
    data.lock.release;                    // release critical section
  end;

  if result then                          // in case opened succesfully
    can_clear;                            // clear can variables and timer
end;

//------------------------------------------------------------------------------
function can_reopen: boolean;
var
  baudrate: integer;
begin
  data.lock.acquire;                      // acquire lock
  try
    baudrate:= data.baudrate;             // get previous baudrate
  finally
    data.lock.release;                    // free lock
  end;
  result:= can_open(baudrate);            // reopen hardware
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
begin
  res:= can.ResetClient(USB);               // reset (reopen) can device
  if res=ERR_OK then                        // if succesfully resetted
  begin
    can_clear;                              // clear can variables
    can_incresetcount;                      // and add reset counter
  end;
end;

//------------------------------------------------------------------------------
procedure can_close;
begin
  data.lock.Acquire;
  try
    res:= can.Close(USB);                   // close can device handle
    if res=ERR_OK then                      // if succesfully closed
    begin
      data.open:= false;                    // set open variable to false
      data.timer.Stop;                      // and stop timer from running
    end;
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_clearfilter;
begin
  can.ResetFilter(USB);
end;

//------------------------------------------------------------------------------
procedure can_filter( start, stop: integer );
begin
  can.ResetFilter(USB);
  res:= can.MsgFilter(USB, start, stop, INIT_TYPE_ST);
end;

//------------------------------------------------------------------------------
procedure can_incsentmessages(msg: tcanmessage);
begin
  data.lock.Acquire;
  try
    data.sentmessages:= data.sentmessages + 1;
    data.lastmsgtime:= now;
    data.lastmessage:= msg;
    data.bytessent:= data.bytessent + msg.len;
    can_internal_statadd(msg);
  finally
    data.lock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure can_increadmessages(msg: tcanmessage);
begin
  data.lock.Acquire;
  try
    data.readmessages:= data.readmessages + 1;
    data.lastmsgtime:= now;
    data.lastmessage:= msg;
    data.bytesread:= data.bytesread + msg.len;
    can_internal_statadd(msg);
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
function can_lastmsgtime: double;
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
procedure can_statuscheck;
var
  bus_heavy: boolean;
begin
  bus_heavy:= false;

  // bus totally off
  if res=ERR_BUSOFF then
  begin
    // in case bus goes off for some reason, we need to
    // reset client to get it back working correctly!
    can_reopen;                             // reopen dongle driver
    can_incresetcount;                      // increase reset counter
  end else

  // bus light or heavy
  if (res=ERR_BUSHEAVY) or (res=ERR_BUSLIGHT) then
  begin
    // bus load is heavy; this usually means that there
    // is some trouble on wiring of can bus (for example
    // terminator missing)
    bus_heavy:= true;
  end;

  // set flags
  can_heavy(bus_heavy);                     // set can bus heavy flag true
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
function can_send(msg: tcanmessage): boolean;
var
  m: TPCANMsg;
  i: integer;
begin
  result:= false;
  if not can_open then exit;              // if device not open, exit
  m.ID:= msg.id;                          // set message can-id
  m.MSGTYPE:= byte(MSGTYPE_STANDARD);     // standard message
  m.LEN:= msg.len;                        // set message length
  for i:=0 to m.LEN-1 do                  // fill in the data
    m.data[i]:= msg.data[i];
  res:= can.Write(USB, m);                // send message to can
  result:= (res=ERR_OK);                  // check result
  if result then                          // if ok
    can_incsentmessages(msg)              // increase write counter
    else can_statuscheck;
end;

//------------------------------------------------------------------------------
function can_read(var msg: tcanmessage): boolean;
var
  m: TPCANMsg;
  i: integer;
begin
  result:= false;
  if not can_open then exit;              // exit if device is not open

  res:= can.Read(USB, m);                 // read available can frame

  if (res=ERR_OK) and (MsgTypes(m.MSGTYPE)=MSGTYPE_STANDARD) then
  begin
    msg.id:= m.ID;
    msg.len:= m.LEN;
    for i:=0 to m.LEN-1 do
      msg.data[i]:= m.data[i];
    can_increadmessages(msg);             // increase read counter
    result:= true;
  end else

    can_statuscheck;                      // go to check bus state
end;

//------------------------------------------------------------------------------
procedure can_clearbuffers;
var
  msg: tcanmessage;
  m: TPCANMsg;
  i: integer;
begin
  if not can_open then exit;
  
  repeat
    res:= can.Read(USB, m);

    // normal message
    if (res=ERR_OK) and (MsgTypes(m.MSGTYPE)=MSGTYPE_STANDARD) then
    begin
      msg.id:= m.ID;
      msg.len:= m.LEN;
      for i:=0 to m.LEN-1 do
        msg.data[i]:= m.data[i];
      can_increadmessages(msg);
    end else

    // not ok result
    if (longword(res)<>0) then //ERR_BUSOFF) then
    begin
      can_statuscheck;
      exit;
    end;

  // repeat until receive queue is empty
  until (res=CANResult(ERR_QRCVEMPTY));
end;

//------------------------------------------------------------------------------
function can_read(var msg: tcanmessage; filter: tcanmessage; timeout: integer): boolean;
var
  start: tdatetime;
  done: boolean;
  i: integer;
begin
  result:= false;
  if not can_open then exit;
  start:= now;
  done:= false;
  while (not done) and (millisecondspan(start, now)<timeout) do
  begin
    if can_read(msg) then
    begin
      done:= true;
      if (filter.id>-1) and (msg.id<>filter.id) then done:= false;
      if (filter.len>-1) and (msg.len<>filter.len) then done:= false;
      for i:=0 to 7 do
        if (filter.data[i]>-1) and (msg.data[i]<>filter.data[i]) then done:= false;
    end;
  end;
  result:= done;
end;

//------------------------------------------------------------------------------
function can_custom(var msg: tcanmessage; filter: tcanmessage; timeout: integer): boolean;
begin
  result:= false;

  // we need to clear buffers before going into next phase
  // to not to gather too much stuff into the peak device's
  // buffers in case the reading requires some more time
  // than expected (17.8.2007, for testing, ari)
  can_clearbuffers;

  if can_send(msg) then
    if can_read(msg, filter, timeout) then
      result:= true;
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
function  can_sdosend(const node, obj, sub, value: integer; Bits: integer = 16): boolean;
var
  m: tcanmessage;
  f: tcanmessage;
begin
  // create request
  m.id:= sdo_canid_rx + node;
  m.len:= 8;
  if (Bits = 8) then m.data[0]:= sdo_writerequest0
    else if (Bits = 16) then m.data[0]:= sdo_writerequest2
      else if (Bits = 32) then m.data[0]:= sdo_writerequest1
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
  data.lastmsgtime:= data.timer.ElapsedMs;

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
function  can_statlist: tstringlist;
var
  i: integer;
  s,t: string;
begin
  result:= tstringlist.create;
  result.clear;

  data.lock.Acquire;
  try

    // add items into the list
    for i:=0 to data.statcount-1 do
    begin

      // format data as string
      s:= '';
      if data.statlist[i].msg.len>0 then s:=s+inttohex(data.statlist[i].msg.data[0],2)+' ';
      if data.statlist[i].msg.len>1 then s:=s+inttohex(data.statlist[i].msg.data[1],2)+' ';
      if data.statlist[i].msg.len>2 then s:=s+inttohex(data.statlist[i].msg.data[2],2)+' ';
      if data.statlist[i].msg.len>3 then s:=s+inttohex(data.statlist[i].msg.data[3],2)+' ';
      if data.statlist[i].msg.len>4 then s:=s+inttohex(data.statlist[i].msg.data[4],2)+' ';
      if data.statlist[i].msg.len>5 then s:=s+inttohex(data.statlist[i].msg.data[5],2)+' ';
      if data.statlist[i].msg.len>6 then s:=s+inttohex(data.statlist[i].msg.data[6],2)+' ';
      if data.statlist[i].msg.len>7 then s:=s+inttohex(data.statlist[i].msg.data[7],2)+' ';

      // add to list, formatted to be shown
      // on the api_listbox component
      t:= formatfloat('0.0',data.statlist[i].timestamp/1000)+'||'+
          '0x'+inttohex(data.statlist[i].msg.id,4)+'||'+
          inttostr(data.statlist[i].msg.len)+'||'+
          s+'||'+
          inttostr(trunc(data.statlist[i].timestamp-data.statlist[i].oldtimestamp))+'||'+
          inttostr(data.statlist[i].count)+'||';

      // mark old texts (this is another feature
      // that is only available on api_listbox
      // component to make some lines old or disabled after some
      // period of time)
      if (data.timer.ElapsedMs-data.statlist[i].timestamp>10000) or (data.statlist[i].timestamp=0) then
        t:= '<!--'+t;

      // add to stringlist
      result.Add(t);

    end;

  finally
    data.lock.Release;
  end;
end;

end.
