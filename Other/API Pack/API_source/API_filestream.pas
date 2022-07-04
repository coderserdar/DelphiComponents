unit API_filestream;

//------------------------------------------------------------------------------
// Customized filestream that has the possibilitity to
// limit transfer speed to what ever you want. Main purpose
// was to use with limiting speeds of transferring files.
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
// 02072009, ari pikivirta
//  * applied GetTickDiff to also calculate sleep time
//
// 29062009, ari pikivirta
//  * removed suspicious overloads of create event on tapi_filestream
//  * assigned events to transferlist that should be tested!
//
// 01062009, ari pikivirta
//  * added Create overloads with Flags parameter instead of Mode and Rights
//
// 03092008, ari pikivirta
//  * added uniqueId to each transfer (randomized from max integer)
//  * started implementing class to hold transfers list
//
// 17062008, ari pikivirta
//  * fixed overloaded create function without filesize
//
// 16062008, ari pikivirta
//  * added possibility to define file size on create (ftp download etc. where
//    the stream doesn't know the size)
//
// 15122007, ari pikivirta
//  * added checking of sleep time, must be above zero!
//
// 12072006, ari pikivirta
//  * added onprogress event
//
//------------------------------------------------------------------------------

interface

uses
  sysutils, classes, windows, syncobjs;

type
  TAPI_filestream_onprogress = procedure (
    const Filename: string;
    const Pos, Size: int64;
    const UniqueId: string) of object;

  TAPI_filestream_ondestroy = procedure (
    const Filename: string;
    const Pos, Size: int64; const BytesRead, BytesWritten: int64;
    const Time: double;
    const UniqueId: string) of object;

  TAPI_filestream = class (tfilestream)
  private
    funiqueId: string;
    fbps: cardinal;
    ffilename: string;
    fstarttime: dword;
    fbytesread: int64;
    ffilesize: int64;
    fbyteswritten: int64;
    fonprogress: TAPI_filestream_onprogress;
    fondestroy: TAPI_filestream_ondestroy;
  public
    constructor Create(Const FileName: string; Const Flags: Cardinal; Const LimitBaudrate: Longint; Const ForceFileSize: int64; var uniqueId: string); overload;
    constructor Create(Const FileName: string; Const Flags: Cardinal; Const LimitBaudrate: Longint; Const ForceFileSize: int64 = -1); overload;
    //constructor Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: Longint; ForceFileSize: int64; var uniqueId: string); overload;
    //constructor Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: Longint; ForceFileSize: int64 = -1); overload;
    //constructor Create(const FileName: string; Mode: Word; Rights: Cardinal); overload;
    //constructor Create(const FileName: string; Flags: Cardinal); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OnProgress: TAPI_filestream_onprogress read fonprogress write fonprogress;
    property OnDestroy: TAPI_filestream_ondestroy read fondestroy write fondestroy;
  end;

  TAPI_FileTransferItem = record
    Filename: String;     // file name
    Filesize: int64;      // file size
    Position: int64;      // current position
    Baudrate: cardinal;   // set transfer speed
    StartTime: TDatetime; // when created?
    UniqueId: string;
  end;

  // FileTransferList (Class)
  TAPI_FileTransferList = class(TComponent)
  private
    fLock: TCriticalSection;
    fCount: Integer;
    fItem: Array of TAPI_FileTransferItem;
    fOnDestroy: TAPI_filestream_ondestroy;
    fOnProgress: TAPI_filestream_onprogress;
    procedure IntOnProgress (const filename: string; const pos, size: int64; const uniqueId: string);
    procedure IntOnDestroy (const filename: string;
      const pos, size: int64; const bytesread, byteswritten: int64;
      const time: double; const uniqueId: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function New(Const Filename: string; Const Flags: Cardinal; Const BaudRate: Longint = 0; Const Size: Int64 = -1): TAPI_FileStream;
    (*
    function Count: integer;
    function GetItem(Index: Integer): TAPI_FileTransferItem;
    function GetName(Index: integer): string;
    function GetPos(Index: integer): string;
    function GetSize(Index: integer): string;
    function GetElapsed(Index: integer): tdatetime;
    function GetTimeLeft(Index: integer): tdatetime;
    *)
  published
    property OnDestroy: TAPI_filestream_ondestroy read fondestroy write fondestroy;
    property OnProgress: TAPI_filestream_onprogress read fonprogress write fonprogress;
  end;

implementation

//uses
//  api_strings;

//------------------------------------------------------------------------------
constructor TAPI_filestream.Create(const FileName: string; Const Flags: Cardinal; Const LimitBaudrate: Longint; Const ForceFileSize: int64; var uniqueId: string);
begin
  inherited create( filename, flags );
  randomize;
  funiqueid:= inttostr(random(high(integer)));
  fbps:= limitbaudrate;
  ffilename:= filename;
  fstarttime:= gettickcount;
  if forcefilesize<0 then ffilesize:= self.size
    else ffilesize:= ForceFileSize;
  fbytesread:= 0;
  fbyteswritten:= 0;
  uniqueid:= funiqueid; // return unique id
end;

constructor TAPI_filestream.Create(Const FileName: string; Const Flags: Cardinal; Const LimitBaudrate: Longint; Const ForceFileSize: int64 = -1);
var
  s: string;
begin
  Create(Filename, Flags, Limitbaudrate, Forcefilesize, s);
end;

(*
constructor TAPI_filestream.Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: Longint; ForceFileSize: int64; var uniqueId: string);
begin
  Create(filename, mode or rights, limitbaudrate, forcefilesize, uniqueid);
end;

constructor TAPI_filestream.Create(const FileName: string; Mode: Word; Rights: Cardinal; LimitBaudrate: Longint; ForceFileSize: int64 = -1);
var
  tempuniqueid: string;
begin
  Create(filename, mode, rights, limitbaudrate, forcefilesize, tempuniqueid);
end;

constructor TAPI_filestream.Create(const FileName: string; Mode: Word; Rights: Cardinal);
var
  id: string;
begin
  Create(filename, Mode or Rights, 0, 0, id);
end;

constructor TAPI_filestream.Create(const FileName: string; Flags: Cardinal);
var
  id: string;
begin
  Create(filename, Flags, 0, 0, id);
end;
*)

//------------------------------------------------------------------------------
function GetTickDiff(const AOldTickCount, ANewTickCount : Cardinal):Cardinal;
begin
  if ANewTickCount >= AOldTickCount then Result := ANewTickCount - AOldTickCount
    else Result := High(Cardinal) - AOldTickCount + ANewTickCount;
end;

//------------------------------------------------------------------------------
function TAPI_filestream.Read(var Buffer; Count: Longint): Longint;
var
  ftime1: dword;
  fendtime: dword;
  fwaittime: dword;
  fsleeptime: integer;
begin
  ftime1:= gettickcount;
  result:= inherited read( buffer, count );
  fbytesread:= fbytesread+result;

  fendtime:= gettickcount;
  if (result>0) and (fbps>0) then
  begin
    fwaittime:= dword(1000*result) div fbps;
    fsleeptime:= GetTickDiff(fwaittime, (fendtime-ftime1));
    if fsleeptime>0 then sleep(fsleeptime);
  end;

  if assigned(fonprogress) then
    fonprogress(ffilename, fbytesread, ffilesize, funiqueid);
end;

//------------------------------------------------------------------------------
function TAPI_filestream.Write(const Buffer; Count: Longint): Longint;
var
  ftime1: dword;
  fendtime: dword;
  fwaittime: dword;
  fsleeptime: integer;
begin
  ftime1:= gettickcount;
  result:= inherited write( buffer, count );
  fbyteswritten:= fbyteswritten+result;

  fendtime:= gettickcount;
  if (result>0) and (fbps>0) then
  begin
    fwaittime:= dword(1000*result) div fbps;
    fsleeptime:= GetTickDiff(fwaittime, (fendtime-ftime1));
    if fsleeptime>0 then sleep(fsleeptime);
  end;

  if assigned(fonprogress) then
    fonprogress(ffilename, fbyteswritten, ffilesize, funiqueId);
end;

//------------------------------------------------------------------------------
destructor TAPI_filestream.Destroy;
begin
  if assigned(fondestroy) then
    fondestroy( ffilename, self.Position, self.Size, fbytesread,
    fbyteswritten, GetTickDiff( fstarttime, GetTickCount), funiqueId );
  inherited destroy;
end;

//------------------------------------------------------------------------------
constructor TAPI_FileTransferList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  flock:= tcriticalsection.create;
  fcount:= 0;
  setlength(fitem, fcount);
end;

//------------------------------------------------------------------------------
destructor TAPI_FileTransferList.Destroy;
begin
  //ile Count>0 do sleep(1);  // wait all transfers to finish
  flock.Free;                 // release lock
  inherited Destroy;          // destroy the whole shit
end;

//------------------------------------------------------------------------------
procedure TAPI_FileTransferList.IntOnProgress (
  const filename: string; const pos, size: int64;
  const uniqueId: string);
var
  p: integer;
begin
  flock.Acquire;
  try
    for p:=0 to fcount-1 do
      if fitem[p].UniqueId = uniqueId then
      begin
        fitem[p].Filename:= filename;
        fitem[p].Filesize:= size;
        fitem[p].Position:= pos;
      end;
  finally
    flock.release;
  end;
  // fire event
  if assigned(fonprogress) then
    fonprogress(filename, pos, size, uniqueid);
end;

//------------------------------------------------------------------------------
procedure TAPI_FileTransferList.IntOnDestroy (
  const filename: string;
  const pos, size: int64; const bytesread, byteswritten: int64;
  const time: double; const uniqueId: String);
var
  p, k: integer;
begin
  flock.acquire;
  try
    for p:=0 to fcount-1 do
      if fitem[p].UniqueId = uniqueId then
      begin
        for k:= p to fcount-2 do
          fitem[k]:= fitem[k+1];
        fcount:= fcount - 1;
        setlength(fitem, fcount);
        break;
      end;
  finally
    flock.release;
  end;
  // fire event
  if assigned(fondestroy) then
    fondestroy(filename, pos, size, bytesread, byteswritten, time, uniqueId);
end;

//------------------------------------------------------------------------------
function TAPI_FileTransferList.New(
  Const Filename: string; Const Flags: Cardinal;
  Const BaudRate: Longint = 0;
  Const Size: Int64 = -1): TAPI_FileStream;
var
  uniqueId: string;
begin
  result:= TAPI_FileStream.Create(filename, flags, baudrate, size, uniqueId);
  // assign events that'll be fired on owner
  // of TAPI_FileTransfer list ?
  result.OnProgress:= TAPI_FileTransferlist(self).IntOnProgress;
  result.OnDestroy:= TAPI_FileTransferlist(self).IntOnDestroy;
  // create new item to list
  flock.Acquire;
  try
    fcount:= fcount + 1;
    setlength(fitem, fcount);
    fitem[fcount-1].Filename:= filename;
    if size<0 then fitem[fcount-1].Filesize:= result.Size
      else fitem[fcount-1].Filesize:= size;
    fitem[fcount-1].Position:= result.Position;
    fitem[fcount-1].Baudrate:= baudrate;
    fitem[fcount-1].StartTime:= now;
    fitem[fcount-1].UniqueId:= uniqueId;
  finally
    flock.release;
  end;
end;

end.
