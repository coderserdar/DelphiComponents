unit API_thread;

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
// r1.06/26032007, ari pikivirta
//  * extended threadsafe variables functionality
//  * added identification number code to each tapi_thread   
//  * added threadmanager as not visible component
//
// r1.05/16022007, ari pikivirta
//  * improved the threadsafe stringlist code to have real items property
//    as the original stringlist does (now it can be directly accessed thread
//    safe)
//
// r1.04/12112006, ari pikivirta
//  * added internal critical section for the activate flag monitoring
//  * changed activate flag to drop in the thread execute end.. as message
//  * added some usefull threadsafe classes:
//    - TSInteger     = thread safe integer
//    - TSFloat       = thread safe double
//    - TSString      = thread safe string
//    - TSStringlist  = thread safe stringlist
//
//    usage:
//      var treadsafestring: TSString;
//      ..
//      threadsafestring:= TSString.create;
//      ..
//      threadsafestring.free;
//
// r1.03/08112006, ari pikivirta
//  * added delay property to run thread cycle slower (if not run only once)
//  * added threadexception event
//
// r1.02/19102006, ari pikivirta
//  * added internal lock / release function for easy access critical section
//  * added possibility to launch this thread for methods too
//  * added data pointer variable into threadstop event also
//  * passing simplethread now in the event to be able to do sync(method) also
//
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Windows, Classes, SyncObjs, Messages;

type
  TAPI_thread = class;
  TAPIsimplethread = class;

  TAPIThreadStart = procedure (aMain: TAPI_thread; aThread: TAPISimpleThread; var Data: Pointer; Id: Integer) of object;
  TAPIThreadEvent = procedure (aMain: TAPI_thread; aThread: TAPISimpleThread; var Data: Pointer; Id: Integer) of object;
  TAPIThreadException = procedure (aMain: TAPI_thread; aThread: TAPISimpleThread; e: Exception; Id: Integer) of object;
  TAPIThreadStop = procedure (aMain: TAPI_thread; aThread: TAPISimpleThread; var Data: Pointer; Id: Integer) of object;

  TAPISimpleThread = class(tthread)
  private
    fid: integer;
    fowner: TAPI_thread;
    fpointer: pointer;
    fsynchronized: boolean;
    fcyclecount: int64;
    fthreadevent: tapithreadevent;
    fthreadstop: tapithreadstop;
    fthreadexception: TAPIThreadException;
    fmethod: tthreadmethod;
    fdelay: integer;
    procedure ThreadEvent;
    procedure ThreadMethod;
    procedure ThreadStop;
  protected
    procedure Execute; override;
  public
    procedure Sync(aThreadMethod: tthreadmethod);
  end;

  TAPI_thread = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    Fid: integer;
    fhandle: thandle;
    flock: tcriticalsection;              // external criticalsection
    fintlock: tcriticalsection;           // internal use only
    factive: boolean;
    fthread: tapisimplethread;
    fdelay: integer;
    fsynchronized: boolean;
    fthreadpriority: tthreadpriority;
    fthreadstart: tapithreadstart;
    fthreadevent: tapithreadevent;
    fthreadstop: tapithreadstop;
    fthreadexception: TAPIThreadException;
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure WndProc(var aMsg: TMessage);
    procedure SetActive;
  protected
    { Protected declarations }
    procedure OnTerminate(sender: tobject);
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
    function  Start(RunOnce: boolean; Data: Pointer = nil): boolean; overload;
    function  Start(fThreadMethod: tthreadmethod; RunOnce: boolean = true; Data: Pointer = nil): boolean; overload;
    function  IsActive: boolean;
    procedure Stop;
    procedure Lock;
    procedure Release;
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property Id: integer read fid write dummyi stored false;
    property Priority: tthreadpriority read fthreadpriority write fthreadpriority;
    property Synchronized: boolean read fsynchronized write fsynchronized;
    property Interval: integer read fdelay write fdelay;
    property OnStart: tapithreadstart read fthreadstart write fthreadstart;
    property OnExecute: tapithreadevent read fthreadevent write fthreadevent;
    property OnStop: tapithreadstop read fthreadstop write fthreadstop;
    property OnException: TAPIThreadException read fthreadexception write fthreadexception;
  end;

  // thread safe integer
  TSInteger = class(tobject)
  protected
    fvalue: integer;
    flock: tcriticalsection;
    function getvalue: integer;
    procedure setvalue(i: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Divide(const Value: integer): integer;
    function Add(const Value: integer): integer;
    function Sub(const Value: integer): integer;
    property Value: integer read getvalue write setvalue;
  end;

  // thread safe float
  TSFloat = class(tobject)
  protected
    fvalue: double;
    flock: tcriticalsection;
    function getvalue: double;
    procedure setvalue(d: double);
  public
    constructor Create;
    destructor Destroy; override;
    function Divide(const Value: double): double;
    function Add(const Value: double): double;
    function Sub(const Value: double): double;
    property Value: double read getvalue write setvalue;
  end;

  // threadsafe string
  TSString = class(tobject)
  protected
    fvalue: string;
    flock: tcriticalsection;
    function getvalue: string;
    procedure setvalue(s: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Pos(const TextToFind: string): integer; // 13032007
    function PosEx(const TextToFind: string; const StartPos: integer): integer; // 13032007
    function SameText(const Text: string): boolean;
    property Text: string read getvalue write setvalue; // 13032007 name change
  end;

  // threadsafe stringlist
  (*
    this basically is the only threadsafe class that really
    has some major advantage on managing your application's
    variables - much easier to handle this than having the
    above integers trough this unit.. :)
  *)
  TSStringlist = class(tobject)
  protected
    fvalue:     tstringlist;
    flock:      tcriticalsection;
    function    GetItems: tstringlist;
    procedure   SetItems(sl: tstringlist);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Add(const Item: string);
    function    Count: integer;
    procedure   Delete(const Index: integer); overload;
    procedure   Delete(const Item: string); overload;
    function    IndexOf(const Item: string): integer;
    function    IndexOfObject(obj: tobject): integer;
    function    IndexOfName(const Name: string): integer;
    function    SaveToFile(const Filename: string): boolean;
    function    LoadFromFile(const Filename: string): boolean;
    function    SaveToStream(Stream: Tstream): boolean;
    function    LoadFromStream(Stream: TStream): boolean;
    property    Items: tstringlist read getitems write setitems;
  end;

  // thread manager (not visible component)
  TAPI_threadmanager = class(TComponent)
  private
    fLock: tcriticalsection;
    fIdList: tstringlist;
    fThreads: integer;
    fThread: array of tapi_thread;
    procedure ThreadStop(aMain: TAPI_thread; aThread: TAPISimpleThread; var Data: Pointer; Id: integer);
  public
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
    function Count: integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function Add(var Id: Integer; fThreadMethod: tthreadmethod; RunOnce: boolean = true; Data: Pointer = nil): boolean;
    procedure Stop(Id: integer);
    function IndexOf(Id: integer): integer;
  end;

procedure Register;

implementation

{$R *.RES}

const
  // when thread ends, it's notified to the component & mainthread
  // via this message..
  NOTIFYTHREAD_END = WM_USER + 1;

//------------------------------------------------------------------------------
procedure TAPI_thread.dummys(s: string);
begin
  // does nothing..
end;

procedure TAPI_thread.dummyi(i: integer);
begin
  // does nothing..
end;

//------------------------------------------------------------------------------
constructor TAPI_thread.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  fversion:= 'r1.06/ari.pikivirta@kolumbus.fi'; // set version information

  (*
    here, we'll assign individual id for this thread
    to have possibility to assign same event to each
    thread component on the form and have the id to
    identify different thread usign the event.
  *)
  randomize;
  fid:= random(1000000);

  fsynchronized:= false;                        // not synchronized by default
  fthreadpriority:= tpnormal;                   // normal thread priority
  fdelay:= 0;                                   // minimum delay (1 ms)
  flock:= tcriticalsection.create;              // create critical section
  fintlock:= tcriticalsection.create;           // create critical section
  fhandle:= AllocateHWnd(WndProc);              // create message handle
end;

//------------------------------------------------------------------------------
destructor TAPI_thread.Destroy;
begin
  stop;                                         // terminate the thread if running
  flock.free;                                   // release critical section
  fintlock.free;                                // release internal lock
  DeallocateHWnd(fhandle);
  inherited Destroy;                            // ok to destroy..
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.WndProc(var aMsg: TMessage);
begin
  if amsg.msg = NOTIFYTHREAD_END then           // if thread end message received
  begin
    fintlock.Acquire;
    try
      factive:= false;
    finally
      fintlock.Release;
    end;
  end else
    amsg.result:= DefWindowProc(fhandle, amsg.Msg, amsg.wparam, amsg.lparam);
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.SetActive;
begin
  fintlock.acquire;
  try
    factive:= true;
  finally
    fintlock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.Lock;
begin
  flock.Acquire;
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.Release;
begin
  flock.Release;
end;

//------------------------------------------------------------------------------
function TAPI_Thread.Start(RunOnce: boolean; Data: Pointer = nil): boolean;
begin
  result:= false;
  if not isactive then
  begin
    setactive;                                  // set active flag
    fthread:= tapisimplethread.Create(true);     // create suspended
    fthread.fpointer:= Data;                    // assign pointer
    fthread.fmethod:= nil;                      // no method started
    fthread.fthreadstop:= fthreadstop;          // assign thread stop event
    fthread.fsynchronized:= fsynchronized;
    fthread.fthreadevent:= fthreadevent;        // assign event
    fthread.fthreadexception:= fthreadexception;
    fthread.fdelay:= fdelay;
    fthread.OnTerminate:= Onterminate;          // assign terminate event
    fthread.FreeOnTerminate:= true;             // free on terminate
    fthread.Priority:= fthreadpriority;         // set thread priority
    fthread.fowner:= self;                      // set owner to pass messages
    if runonce then fthread.fcyclecount:= 1
      else fthread.fcyclecount:= 0;
    fthread.Resume;                             // start thread

    if assigned(fthreadstart) then              // thread start event
      fthreadstart(self, fthread, Data, Id);    // does not need synching :)

    result:= true;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_thread.Start(fThreadMethod: tthreadmethod; RunOnce: boolean = true; Data: Pointer = nil): boolean;
begin
  result:= false;
  if not isactive then
  begin
    setactive;                                  // set active flag
    fthread:= tapisimplethread.Create(true);     // create suspended
    fthread.fthreadstop:= fthreadstop;          // assign thread stop event
    fthread.fmethod:= fthreadmethod;            // fire some method in thread
    fthread.fpointer:= Data;                    // attach data
    fthread.fsynchronized:= fsynchronized;      // set synchronized status
    fthread.fthreadevent:= fthreadevent;
    fthread.fthreadexception:= fthreadexception;
    fthread.fdelay:= fdelay;
    fthread.OnTerminate:= OnTerminate;
    fthread.FreeOnTerminate:= true;
    fthread.Priority:= fthreadpriority;         // set thread priority
    fthread.fowner:= self;                      // set owner to pass messages
    if runonce then fthread.fcyclecount:= 1     // run only once
      else fthread.fcyclecount:= 0;             // until terminated
    fthread.resume;
    factive:= true;

    if assigned(fthreadstart) then              // if start event is assigned
      fthreadstart(self, fthread, Data, Id);    // fire start event

    result:= true;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.OnTerminate(sender: TObject);
begin
  // on terminate.. yeah
end;

//------------------------------------------------------------------------------
function TAPI_thread.IsActive: boolean;
begin
  fintlock.Acquire;
  try
    result:= factive;
  finally
    fintlock.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_thread.Stop;
begin
  if isactive then
    fthread.Terminate;                            // terminate running thread
end;

//------------------------------------------------------------------------------
procedure TAPISimpleThread.ThreadEvent;
begin
  if assigned(fthreadevent) then
    fthreadevent(fowner, self, fpointer, Fid);
end;

procedure TAPISimpleThread.ThreadMethod;
begin
  if assigned(fmethod) then
    fmethod;
end;

procedure TAPISimpleThread.ThreadStop;
begin
  if assigned(fthreadstop) then
    fthreadstop(fowner, self, fpointer, fid);
end;

//------------------------------------------------------------------------------
procedure TAPISimpleThread.Sync(aThreadMethod: tthreadmethod);
begin
  Synchronize(athreadmethod);
end;

//------------------------------------------------------------------------------
procedure TAPISimpleThread.Execute;
var
  cycles: int64;
begin
  cycles:= 1;

  repeat

    // thread event
    try
      if (fsynchronized) and (assigned(fthreadevent)) then
        synchronize(ThreadEvent)    // run even synchronized
        else ThreadEvent;           // without synchronization
    except
      on E: Exception do
      begin
        if assigned(fthreadexception) then
        begin
          fthreadexception(fowner, self, E, fid);
        end else
        begin
          Terminate;
          raise;
        end;
      end;
    end;

    // thread method
    try
      if (fsynchronized) and (assigned(fmethod)) then
        synchronize(ThreadMethod)
        else ThreadMethod;
    except
      on E: Exception do
      begin
        if assigned(fthreadexception) then
        begin
          fthreadexception(fowner, self, E, fid);
        end else
        begin
          Terminate;
          raise;
        end;
      end;
    end;

    // increase cycles
    cycles:= cycles + 1;

    // small delay to keep app alive, except
    // if terminating or cyclecount is reached
    if (not terminated) then
      if (fdelay>0) then sleep(fdelay)
        else sleep(1);

  // if cycle count is set to zero this
  // thread will run until terminated
  until (terminated) or ((fcyclecount>0) and (cycles>fcyclecount));

  // thread stop event (always synchronized..)
  if assigned(fthreadstop) then                 // thread stopped event
    synchronize(ThreadStop);                    // synchronize event firing proc

  postmessage(fowner.fhandle, NOTIFYTHREAD_END, 0, 0);
end;

//------------------------------------------------------------------------------
// TSInteger
//------------------------------------------------------------------------------

constructor TSInteger.Create;
begin
  inherited Create;
  flock:= tcriticalsection.create;
  fvalue:= 0;
end;

destructor TSInteger.Destroy;
begin
  inherited Destroy;
  flock.free;
  flock:= nil;
end;

function TSInteger.Divide(const Value: integer): integer;
begin
  flock.Acquire;
  try
    fvalue:= fvalue div value;
    result:= fvalue;
  finally
    flock.Release;
  end;
end;

function TSInteger.Add(const Value: integer): integer;
begin
  flock.acquire;
  try
    fvalue:= fvalue + value;
    result:= fvalue;
  finally
    flock.release;
  end;
end;

function TSInteger.Sub(const Value: integer): integer;
begin
  flock.acquire;
  try
    fvalue:= fvalue - value;
    result:= fvalue;
  finally
    flock.release;
  end;
end;

function TSInteger.getvalue;
begin
  flock.Acquire;
  try
    result:= fvalue;
  finally
    flock.release;
  end;
end;

procedure TSInteger.setvalue(i: Integer);
begin
  flock.Acquire;
  try
    fvalue:= i;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
// TSFloat
//------------------------------------------------------------------------------

constructor TSFloat.Create;
begin
  inherited Create;
  flock:= tcriticalsection.create;
end;

destructor TSFloat.Destroy;
begin
  inherited Destroy;
  flock.free;
  flock:= nil;
end;

function TSFloat.getvalue;
begin
  flock.Acquire;
  try
    result:= fvalue;
  finally
    flock.release;
  end;
end;

function TSFloat.Add(const Value: double): double;
begin
  flock.acquire;
  try
    fvalue:= fvalue + value;
    result:= fvalue;
  finally
    flock.release;
  end;
end;

function TSFloat.Sub(const Value: double): double;
begin
  flock.acquire;
  try
    fvalue:= fvalue - value;
    result:= fvalue;
  finally
    flock.release;
  end;
end;

function TSFloat.Divide(const Value: double): double;
begin
  flock.Acquire;
  try
    // do not divide by zero
    if (fvalue<>0) and (value<>0) then fvalue:= fvalue/value
      else fvalue:= 0;
    result:= fvalue;
  finally
    flock.Release;
  end;
end;

procedure TSFloat.setvalue(d: Double);
begin
  flock.Acquire;
  try
    fvalue:= d;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
// TSString
//------------------------------------------------------------------------------

constructor TSString.Create;
begin
  inherited Create;
  fvalue:= '';
  flock:= tcriticalsection.create;
end;

destructor TSString.Destroy;
begin
  inherited Destroy;
  flock.Free;
  flock:= nil;
end;

function TSString.SameText(const Text: string): boolean;
begin
  flock.acquire;
  try
    result:= (ansisametext(text, fvalue));
  finally
    flock.release;
  end;
end;

function TSString.Pos(const TextToFind: string): integer;
begin
  flock.Acquire;
  try
    result:= system.Pos(texttofind, fvalue);
  finally
    flock.release;
  end;
end;

function TSString.PosEx(const TextToFind: string; const StartPos: integer): integer;
var
  temps: string;
begin
  flock.Acquire;
  try
    temps:= fvalue;                           // use temporary string
  finally
    flock.Release;
  end;
  (*
      trick here, because we did all
      we wanted in the locking already,
      we are threadsafe now even if we
      do rest outside the lock!
  *)
  delete(temps, 1, startpos);                 // to delete beginning
  result:= system.pos(texttofind, temps);     // then try position from rest
end;

function TSString.getvalue;
begin
  flock.Acquire;
  try
    result:= fvalue;
  finally
    flock.release;
  end;
end;

procedure TSString.setvalue;
begin
  flock.acquire;
  try
    fvalue:= s;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
// TSStringList
//------------------------------------------------------------------------------

constructor TSStringlist.Create;
begin
  inherited Create;
  fvalue:= tstringlist.create;
  flock:= tcriticalsection.create;
  Clear;
end;

destructor TSStringlist.Destroy;
begin
  fvalue.Free;
  inherited Destroy;
  flock.Free;
  flock:= nil;
end;

procedure TSStringlist.Clear;
begin
  flock.Acquire;
  try
    fvalue.Clear;
  finally
    flock.Release;
  end;
end;

procedure TSStringlist.Add(const item: string);
begin
  flock.Acquire;
  try
    fvalue.Add(item)
  finally
    flock.Release;
  end;
end;

function TSStringlist.Count: integer;
begin
  flock.Acquire;
  try
    result:= fvalue.Count;
  finally
    flock.Release;
  end;
end;

// 16.2.2007, ari pikivirta
function TSStringlist.GetItems: tstringlist;
begin
  flock.Acquire;
  try
    result:= tstringlist.create;
    result:= fvalue;
  finally
    flock.Release;
  end;
end;

function TSStringlist.IndexofName(const Name: string): integer;
begin
  flock.acquire;
  try
    result:= fvalue.indexofname(name);
  finally
    flock.release;
  end;
end;

function TSStringlist.IndexOfObject(obj: tobject): integer;
begin
  flock.acquire;
  try
    result:= fvalue.IndexOfObject(obj)
  finally
    flock.Release;
  end;
end;

// 16.2.2007, ari pikivirta
procedure TSStringlist.SetItems(sl: tstringlist);
begin
  flock.Acquire;
  try
    fvalue:= sl;
  finally
    flock.Release;
  end;
end;

procedure TSStringlist.Delete(const index: integer);
begin
  flock.Acquire;
  try
    fvalue.Delete(index);
  finally
    flock.Release;
  end;
end;

procedure TSStringlist.Delete(const item: string);
var
  i: integer;
begin
  flock.Acquire;
  try
    i:= fvalue.IndexOf( item );
    if i>-1 then fvalue.Delete(i);
  finally
    flock.Release;
  end;
end;

function TSStringlist.IndexOf(const item: string): integer;
begin
  flock.Acquire;
  try
    result:= fvalue.IndexOf(item)
  finally
    flock.Release;
  end;
end;

function TSStringlist.SaveToFile(const filename: string): boolean;
begin
  flock.Acquire;
  try
    try
      fvalue.SaveToFile(filename);
      result:= true;
    except
      result:= false;
    end;
  finally
    flock.Release;
  end;
end;

function TSStringList.LoadFromFile(const filename: string): boolean;
begin
  flock.Acquire;
  try
    try
      fvalue.LoadFromFile(filename);
      result:= true;
    except
      result:= false;
    end;
  finally
    flock.Release;
  end;
end;

function TSStringlist.SaveToStream(stream: tstream): boolean;
begin
  flock.Acquire;
  try
    try
      fvalue.SaveToStream(stream);
      result:= true;
    except
      result:= false;
    end;
  finally
    flock.Release;
  end;
end;

function TSStringList.LoadFromStream(stream: tstream): boolean;
begin
  flock.Acquire;
  try
    try
      fvalue.LoadFromStream(Stream);
      result:= true;
    except
      result:= false;
    end;
  finally
    flock.Release;
  end;
end;

//------------------------------------------------------------------------------
// THREAD MANAGER
//------------------------------------------------------------------------------

constructor TAPI_threadmanager.Create(AOwner: tcomponent);
begin
  inherited Create(AOwner);
  randomize;
  flock:= tcriticalsection.create;
  fIdList:= tstringlist.create;
  fthreads:= 0;
  Clear;
end;

destructor TAPI_threadmanager.Destroy;
begin
  fIdList.free;
  flock.Free;
  inherited Destroy;
end;

function TAPI_threadmanager.Count: integer;
begin
  flock.acquire;
  try
    result:= fthreads;
  finally
    flock.release;
  end;
end;

procedure TAPI_threadmanager.Delete(Index: integer);
var
  i: integer;
begin
  flock.acquire;
  try
    if (index>-1) and (index<fthreads) then
    begin
      // stop and free thread[index]
      fthread[index].Stop;                  // terminate it
      // move assignments..
      for i:=index to fthreads-2 do         
        fthread[i]:= fthread[i+1];
      fthread[fthreads-1]:= nil;
      // free last thread object
      fthreads:= fthreads-1;
      setlength(fthread, fthreads);
    end;
  finally
    flock.release;
  end;
end;

procedure TAPI_threadmanager.Clear;
begin
  while count>0 do
    Delete(0);
end;

function TAPI_threadmanager.IndexOf(Id: integer): integer;
begin
  flock.acquire;
  try
    result:= fidlist.IndexOf(inttostr(id));
  finally
    flock.release;
  end;
end;

function TAPI_threadmanager.Add(var Id: Integer; fThreadMethod: tthreadmethod; RunOnce:
  boolean = true; Data: Pointer = nil): boolean;
begin
  flock.acquire;
  try
    // add new thread
    fthreads:= fthreads+1;
    setlength(fthread, fthreads);
    // create thread
    fthread[ fthreads-1 ]:= tapi_thread.create(self);
    fthread[ fthreads-1 ].OnStop:= ThreadStop;
    // add id
    id:= fthread[ fthreads-1 ].Id;  // get thread's id
    fidlist.add( inttostr( id ));   // add to manager's list
    // start thread
    fthread[ fthreads-1 ].Start(fthreadmethod, runonce, data);
    // result..
    result:= true;
  finally
    flock.release;
  end;
end;

procedure TAPI_threadmanager.ThreadStop(aMain: TAPI_thread; aThread: TAPISimpleThread; var Data: Pointer; id: integer);
var
  index: integer;
begin
  index:= indexof(amain.id);    // thread id (hopefully unique)
  delete(index);                // automatically delete from list..
end;

procedure TAPI_threadmanager.Stop(Id: integer);
var
  index: integer;
begin
  index:= indexof(id);
  flock.Acquire;
  try
    fthread[index].stop;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_thread]);
end;

end.
