{*********************************************************}
{* FlashFiler: Server thread pool & thread classes       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffllthrd;

interface

uses
  classes,
  windows,
  ffllBase,                                                            {!!.06}
  ffllComp;                                                            {!!.06}

type
  { This is a type of procedure that may be passed to a thread pool for
    processing.  The thread pool grabs an available thread or instantiates
    a new thread.  It then passes the procedure to the thread and the thread
    calls the procedure.  aProcessCookie is whatever the calling object
    wants it to be. }
  TffThreadProcessEvent = procedure(const aProcessCookie: longInt) of object;

  TffThreadPool = class;  { forward declaration }

  { This type of thread is useful for work that must occur on a periodic
    basis.  This thread frees itself when terminated. }
  TffTimerThread = class(TffThread)
  protected  { private }

    FFrequency : DWord;
      {-The number of milliseconds between each firing of the timer event. }

    FTimerEvent : TffThreadProcessEvent;
      {-The routine that is called when the "timer" fires. }

    FTimerEventCookie : longInt;
      {-The cookie passed to the FProcessEvent. }

    FDieEvent : TffEvent;
      {-Event raised when a thread is to die. }

  protected

    procedure Execute; override;

  public

    constructor Create(const aFrequency : DWord;
                             aTimerEvent : TffThreadProcessEvent;
                       const aTimerEventCookie : longInt;
                       const createSuspended : boolean); virtual;
      { Use this method to create an instance of the thread.  Parameters:
        - aFrequency is the number of milliseconds that must elapse before the
          thread calls aProcessEvent.
        - aTimerEvent is the method called when the timer fires.
        - aTimerEventCookie is an optional value that is passed to aTimerEvent.
        - CreateSuspended allows you to control when the thread starts.
          If False then the thread starts immediately.  If True then the thread
          starts once you call the Resume method. }

    destructor Destroy; override;

    procedure DieDieDie;
      { Use this method to terminate the timer thread. }

    property Frequency : DWord
       read FFrequency write FFrequency;
      { The number of milliseconds between each firing of the timer event. }

  end;

  { This is the base class for threads associated with pools.  The pool's
    Process method grabs an available thread or creates a new instance of
    this class.  It then calls the TffPooledThread.Process method. }
  TffPooledThread = class(TffThread)
  protected  { private }

    FDieEvent : TffEvent;
      {-Event raised when a thread is to die. }

    FProcessCookie : longInt;
      {-The cookie passed to the Process method.  Used by the Execute
        method. }

    FProcessEvent : TffThreadProcessEvent;
      {-The callback passed to the Process method.  Used by the Execute
        method. }

    FThreadEventHandles: Array[0..1] of THandle;
      {-When a thread is created, it pauses in its execute method until it
        receives one of two events:

        1. Wake up and do some work.
        2. Wake up and terminate.

        This array stores these two event handles. }

    FThreadPool : TffThreadPool;
      {-The parent thread pool. }

    FWorkEvent : TffEvent;
      {-Event raised when a thread is to do work. }

  protected

    procedure Execute; override;
      { Calls the processEvent stored by the Process method.
        Do not call this function directly.  Instead, use the Process method. }

    procedure ptReturnToPool;
      {-Called by the execute method.  When the thread has finished its work,
        this method has the threadpool return this thread to the list of
        inactive threads.  If there are pending requests, the threadPool will
        assign one to this thread instead of putting the thread back in the
        inactive list. }

  public

    constructor Create(threadPool : TffThreadPool); virtual;
      { Use this method to create the thread and associate it with a thread
        pool. }

    destructor Destroy; override;

    procedure DieDieDie;
      { Use this method to terminate the thread. }

    procedure Process(aProcessEvent : TffThreadProcessEvent;
                      aProcessCookie: longInt);
      { This method is called by the thread pool to perform work.  It saves
        the process event and cookie then raises an event informing the
        thread it has work to do. }

  published
  end;

  { This class is a generic mechanism for having work performed in a separate
    thread.  It maintains a pool of threads.  It may be instructed to create
    an initial number of threads upon startup and to never exceed a certain
    number of threads within the pool.  It maintains the status of each
    thread, placing them in an active or inactive list.

    Any type of object may have work performed through one of the pool's thread
    by supplying a callback function and cookie (optional) to the pool's
    ProcessThreaded method. }
  TffThreadPool = class(TffLoggableComponent)                          {!!.06}
  private

    FActive : TffList;
      {-List of acquired threads. When a thread becomes inactive it is moved
        to FInactive. }

    FInactive : TffList;
      {-List of available threads.  When a thread is acquired, it moves to the
        FActive list. }

    FInitialCount : integer;
      {-The maximum number of threads that can be created by the pool. }

    FInitialized : boolean;
      {-Set to True when the initial threads have been created for the thread
        pool. }

    FMaxCount : integer;
      {-The maximum number of threads to be created by the pool. }

    FPendingQueue : TffThreadQueue;
      {-Queue of pending requests.  Requests wind up here when a thread
        is not available to process the request. }

    FLock : TffPadlock;
      {-Controls access to the threads. }

    FSkipInitial : Boolean;
      {-Used by the EngineManager expert to keep the pool from creating threads
        when InitialCount is set}

  protected

    function thpGetActiveCount : integer;
      {-Return total # of active thread. }

    function thpGetFreeCount : integer;
      {-Return total # of free thread slots.  In other words, the maximum
        number of threads minus the total # of active and inactive threads. }

    function thpGetInactiveCount : integer;
      {-Return total # of inactive threads.}

    function thpGetThreadFromPool : TffPooledThread;
      {-Used to obtain a thread from the inactive pool.  If no thread is
        available then this method returns nil.  If a thread is available,
        the thread is moved from the inactive list to the active list. }

    procedure thpPutInQueue(aProcessEvent : TffThreadProcessEvent;
                            aProcessCookie: longInt);
      {-Used to place a request in queue when a thread is not available to
        process the request.  The request will be picked out of the queue by
        the next free thread. }

    procedure thpReturnThreadToPool(aThread : TffPooledThread);
      {-Called by a thread when it has finished processing.  If any requests
        are in queue then this method has the newly-available thread process
        the request.  Otherwise, this method moves the thread from the active
        list to the inactive list. }

    procedure thpSetInitialCount(const aCount : integer);
      {-Called when the initial thread count is set. }

    procedure thpSetMaxCount(const aCount : integer);
      {-Called when the max thread count is set. }

    property SkipInitial : Boolean
      read FSkipInitial write FSkipInitial;
      {-Used by the EngineManager expert to keep the pool from creating threads
        when InitialCount is set}

  public

    constructor Create(aOwner : TComponent); override;

    destructor Destroy; override;

    procedure Flush(NumToRetain : integer);
      { Use this method to flush inactive threads from the pool.  NumToRetain
        is the number of inactive threads to retain in the pool.  Active threads
        are unaffected by this method. }

    procedure ProcessThreaded(aProcessEvent : TffThreadProcessEvent;
                              aProcessCookie: longInt);
      { Use this method to have a worker thread process a message.  The worker
        thread calls the specified process event, passing it the specified
        process cookie.  If a worker thread is not immediately available, this
        method will add the message to an internal queue.  The next thread that
        becomes available will pick up the request from the queue and process
        the request. }

    property ActiveCount : integer read thpGetActiveCount;
      { The total number of active threads. }

    property FreeCount : integer read thpGetFreeCount;
      { The total number of thread slots that are unfilled.  Usually
        calculated as MaxCount - ActiveCount - InactiveCount. }

    property InactiveCount : integer read thpGetInactiveCount;
      { The total number of inactive threads.  Does not include free thread
        slots that do not contain a thread. }

  published

    property InitialCount : integer
      read FInitialCount write thpSetInitialCount default 5;
      { The initial number of threads to be preloaded by the pool. }

    property MaxCount : integer
      read FMaxCount write thpSetMaxCount default 16;
      { The maximum number of threads that can be created by the pool. }

  end;

  { This type is used to store pending requests in the TffThreadPool. }
  TffThreadRequestItem = class(TffSelfListItem)
  protected
    FProcessCookie : longInt;
    FProcessEvent : TffThreadProcessEvent;
  public
    constructor Create(anEvent : TffThreadProcessEvent;
                       aCookie : longInt);

    property ProcessCookie : longInt read FProcessCookie;
    property ProcessEvent : TffThreadProcessEvent read FProcessEvent;
  end;

implementation

uses
  sysUtils,                                                            {!!.06}
//  ffllcomp,                                                          {Deleted !!.06}
  ffllexcp;

{$I ffconst.inc}
{$I ffllscst.inc}

{===TffTimerThread===================================================}
constructor TffTimerThread.Create(const aFrequency : DWord;
                                        aTimerEvent : TffThreadProcessEvent;
                                  const aTimerEventCookie : longInt;
                                  const createSuspended : boolean);
begin
  { Requirement: aTimerEvent must be assigned. }
  if not assigned(aTimerEvent) then
    RaiseSCErrorCodeFmt(ffsce_ParameterRequired,
                        ['aTimerEvent', ClassName + '.constructor']);

  { Make sure important variables set before the thread is actually started in
    the inherited Create. }
  FDieEvent := TffEvent.Create;
  FFrequency := aFrequency;
  FTimerEvent := aTimerEvent;
  FTimerEventCookie := aTimerEventCookie;
  FreeOnTerminate := False;

  inherited Create(createSuspended);

end;
{--------}
destructor TffTimerThread.Destroy;
begin
  FDieEvent.Free;
  inherited Destroy;
end;
{--------}
procedure TffTimerThread.DieDieDie;
begin
  Terminate;
  FDieEvent.SignalEvent;
end;
{--------}
procedure TffTimerThread.Execute;
var
  aResult : DWORD;
begin

  if Terminated then exit;

  repeat
    aResult := FDieEvent.WaitForQuietly(FFrequency);
    if aResult = WAIT_TIMEOUT then
      FTimerEvent(FTimerEventCookie)
    else
      Terminate;
  until Terminated;
end;
{====================================================================}

{===TffPooledThread==================================================}
constructor TffPooledThread.Create(threadPool : TffThreadPool);
  { Use this method to create the thread and associate it with a thread
    pool. }
begin
  inherited Create(False);
  FDieEvent := TffEvent.Create;
  FProcessCookie := -1;
  FProcessEvent := nil;
  FThreadPool := threadPool;
  FWorkEvent := TffEvent.Create;
  FThreadEventHandles[0] := FWorkEvent.Handle;
  FThreadEventHandles[1] := FDieEvent.Handle;
  FreeOnTerminate := False;  { Freed in TffThreadpool.destroy }
end;
{--------}
destructor TffPooledThread.Destroy;
begin
  FDieEvent.Free;
  FWorkEvent.Free;
  inherited Destroy;
end;
{--------}
procedure TffPooledThread.DieDieDie;
begin
  Terminate;
  FDieEvent.SignalEvent;
end;
{--------}
procedure TffPooledThread.Execute;
var
  WaitResult : DWORD;
begin

  repeat
    { Wait for something to do or until we are killed. }
    WaitResult := WaitForMultipleObjects(2, @FThreadEventHandles,
                                         false, ffcl_INFINITE);       {!!.06}
    if (WaitResult = WAIT_OBJECT_0) then begin
      { Thread has work to do. }
{Begin !!.06}
      try
        if assigned(FProcessEvent) then
          FProcessEvent(FProcessCookie);
      except
        on E:Exception do
          FThreadPool.lcLog('Exception caught in TffPooledThread.Execute: ' +
                            E.Message);
      end;
{End !!.06}
      if not Terminated then
        ptReturnToPool;
    end;
  until Terminated;

end;
{--------}
procedure TffPooledThread.Process(aProcessEvent : TffThreadProcessEvent;
                                  aProcessCookie: longInt);
  { This method is called by the thread pool to perform work.  It saves
    the process event and cookie then resumes the thread. }
begin
  FProcessEvent := aProcessEvent;
  FProcessCookie := aProcessCookie;
  FWorkEvent.SignalEvent;
end;
{--------}
procedure TffPooledThread.ptReturnToPool;
begin
  FThreadPool.thpReturnThreadToPool(Self);
end;
{====================================================================}

{===TffThreadPool====================================================}
constructor TffThreadPool.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FLock := TffPadlock.Create;
  FActive := TffList.Create;
  FActive.Sorted := False;
  FInactive := TffList.Create;
  FInactive.Sorted := False;
  FInitialCount := 5;
  FInitialized := False;
  FMaxCount := 16;
  FPendingQueue := TffThreadQueue.Create;
  FSkipInitial := False;
end;
{--------}
destructor TffThreadPool.Destroy;
var
  anIndex : longInt;
  aThread : TffPooledThread;
  HandleList : TffHandleList;  { list of thread handles }
  PHandleArray : pointer;
begin
  FFNotifyDependents(ffn_Destroy);                                     {!!.11}
  FLock.Lock;
  try
    HandleList := TffHandleList.Create;
    try
      if assigned(FActive) then begin
        { Allocate memory for the array of thread handles. }
        HandleList.Capacity := FActive.Count;
        for anIndex := pred(FActive.Count) downto 0 do begin
          aThread := TffPooledThread(TffIntListItem(FActive[anIndex]).KeyAsInt);
          HandleList.Append(aThread.Handle);
          aThread.DieDieDie;
        end;
      end;

      if assigned(FInactive) then begin
        { Add more memory as needed to array of thread handles. }
        HandleList.Capacity := HandleList.Capacity + FInactive.Count;
        for anIndex := pred(FInactive.Count) downto 0 do begin
          aThread := TffPooledThread(TffIntListItem(FInactive[anIndex]).KeyAsInt);
          HandleList.Append(aThread.Handle);
          aThread.DieDieDie;
        end;
      end;

      { Wait for the threads to terminate. }
      PHandleArray := HandleList.InternalAddress;
      WaitForMultipleObjects(HandleList.Count, pHandleArray, true, 2000);
      { SPW - 7/3/2000 - Note: I tried using the MsgWaitForMultipleObjects (as shown
        below) but after awhile it would wait the entire 5 seconds even though all
        threads had terminated.  Using WaitForMultipleObjects does not appear to
        have that kind of problem.
          MsgWaitForMultipleObjects(HandleIndex, pHandleArray^, true,
                                          2000, QS_ALLINPUT); }
    finally
      { Explicitly remove the handles so that they are not closed before the
        thread has had a chance to close the handle. }
      HandleList.RemoveAll;
      HandleList.Free;
    end;

    { Free the threads. }
    if assigned(FActive) then
      for anIndex := pred(FActive.Count) downto 0 do
        TffPooledThread(TffIntListItem(FActive[anIndex]).KeyAsInt).Free;

    if assigned(FInactive) then
      for anIndex := pred(FInactive.Count) downto 0 do
        TffPooledThread(TffIntListItem(FInactive[anIndex]).KeyAsInt).Free;

    FPendingQueue.Free;

  finally
    FActive.Free;
    FInactive.Free;
    FLock.Unlock;
    FLock.Free;
  end;

  inherited Destroy;
end;
{--------}
procedure TffThreadPool.Flush(NumToRetain : integer);
var
  anIndex : integer;
  aThread : TffPooledThread;
begin
  FLock.Lock;
  try
    for anIndex := pred(FInactive.Count) downto NumToRetain do begin
      aThread := TffPooledThread(TffIntListItem(FInactive[anIndex]).KeyAsInt);
      aThread.DieDieDie;
      FInactive.DeleteAt(anIndex);
    end;
  finally
    FLock.Unlock;
  end;
end;
{--------}
procedure TffThreadPool.ProcessThreaded(aProcessEvent : TffThreadProcessEvent;
                                        aProcessCookie: longInt);
var
  aThread : TffPooledThread;
begin
  { Get an available thread. }
  aThread := thpGetThreadFromPool;

  { If one is available then have it process the request. }
  if assigned(aThread) then
    aThread.Process(aProcessEvent, aProcessCookie)
  else
    { Otherwise put the request in queue for processing by
      the next free thread. }
   thpPutInQueue(aProcessEvent, aProcessCookie);
end;
{--------}
function TffThreadPool.thpGetActiveCount : integer;
begin
  FLock.Lock;
  try
    Result := FActive.Count;
  finally
    FLock.Unlock;
  end;
end;
{--------}
function TffThreadPool.thpGetFreeCount : integer;
begin
  { free count := max - (active count + inactive count) }

  { Note there is a small chance for inaccuracy.  It is totally
    possible that a new thread is activated in between our getting
    the active threads count and getting the inactive threads count.

    Just in case this question is in your mind, we should only lock
    one list at a time.  Otherwise we run the risk of deadlock. }
  FLock.Lock;
  try
    Result := FMaxCount - FActive.Count - FInactive.Count;
  finally
    FLock.Unlock;
  end;
end;
{--------}
function TffThreadPool.thpGetInactiveCount : integer;
begin
  FLock.Lock;
  try
    Result := FInactive.Count;
  finally
    FLock.Unlock;
  end;
end;
{--------}
function TffThreadPool.thpGetThreadFromPool : TffPooledThread;
var
  aListItem : TffIntListItem;
  anIndex : longInt;
begin
  Result := nil;
  aListItem := nil;
  FLock.Lock;
  try
    { Is an inactive thread available? }
    anIndex := pred(FInactive.Count);
    if anIndex >= 0 then begin
      { Yes.  Grab the last one and remove it from the inactive list. }
      aListItem := TffIntListItem(FInactive[anIndex]);
      FInactive.RemoveAt(anIndex);
      Result := TffPooledThread(aListItem.KeyAsInt);
    end;

    { If we didn't have an inactive thread, see if we can add a new thread.
      Note: We do this outside the above try..finally block because GetFreeCount
      must obtain read access to both thread lists. }
    if not assigned(Result) then
      if thpGetFreeCount > 0 then begin
        Result := TffPooledThread.Create(Self);
        aListItem := TffIntListItem.Create(longInt(Result));
      end;

    { Did we obtain a thread? }
    if assigned(aListItem) then
      { Yes.  Add it to the active list. }
      FActive.Insert(aListItem);
  finally
    FLock.Unlock;
  end;


end;
{--------}
procedure TffThreadPool.thpPutInQueue(aProcessEvent : TffThreadProcessEvent;
                                      aProcessCookie: longInt);
var
  anItem : TffThreadRequestItem;
begin
  anItem := TffThreadRequestItem.Create(aProcessEvent, aProcessCookie);
  with FPendingQueue.BeginWrite do
    try
      Enqueue(anItem);
    finally
      EndWrite;
    end;
end;
{--------}
procedure TffThreadPool.thpReturnThreadToPool(aThread : TffPooledThread);
var
  aCookie: longInt;
  anEvent : TffThreadProcessEvent;
  anItem : TffThreadRequestItem;
  aListItem : TffIntListItem;
  PendingRequest : boolean;
begin
  anEvent := nil;
  aCookie := -1;

  { Any pending requests? Note that we are assuming some minor risk here.
    The pending queue should only have something in it if all threads
    were busy. We can afford to check the queue's count without worrying
    about thread-safeness because somebody will pick up the count sooner
    or later. }
  PendingRequest := False;
  if FPendingQueue.Count > 0 then
    with FPendingQueue.BeginWrite do
      try
        PendingRequest := (Count > 0);
        { If we have a pending request then get it. }
        if PendingRequest then begin
          anItem := TffThreadRequestItem(FPendingQueue.Dequeue);
          anEvent := anItem.ProcessEvent;
          aCookie := anItem.ProcessCookie;
          anItem.Free;
        end;
      finally
        EndWrite;
      end;

  { If we had a pending request then handle it. }
  if PendingRequest then
    aThread.Process(anEvent, aCookie)
  else begin
    { Otherwise move this thread to the inactive threads list. }
    FLock.Lock;
    try
      aListItem := TffIntListItem(FActive[FActive.Index(longInt(aThread))]);
      FActive.Remove(longInt(aThread));
      FInactive.Insert(aListItem);
    finally
      FLock.Unlock;
    end;
  end;
end;
{--------}
procedure TffThreadPool.thpSetInitialCount(const aCount : integer);
var
  anIndex : integer;
  anItem : TffIntListItem;
  aThread : TffPooledThread;
begin
  if not (csDesigning in ComponentState) and (not FInitialized) and
    (not FSkipInitial) then begin
    FLock.Lock;
    try
      { Create the initial set of threads. }
      for anIndex := 1 to aCount do begin
        aThread := TffPooledThread.Create(Self);
        anItem := TffIntListItem.Create(longInt(aThread));
        FInactive.Insert(anItem);
      end;
    finally
      FLock.Unlock;
    end;
    FInitialized := True;
  end;
  FInitialCount := aCount;
end;
{--------}
procedure TffThreadPool.thpSetMaxCount(const aCount : integer);
var
  anIndex : integer;
  aThread : TffPooledThread;
  currCount : integer;
  delCount : integer;
begin
  if not (csDesigning in ComponentState) and (not FSkipInitial) then begin
    { If the maximum is now lower than our initial count then get rid
      of some threads. }
    currCount := FMaxCount - thpGetFreeCount;
    if currCount > aCount then begin
      { Figure out how many threads need to be deleted. }
      delCount := currCount - aCount;
      FLock.Lock;
      try
        for anIndex := 1 to delCount do
          { We have to check the count.  It is possible we need to
            delete more threads than are in the inactive list.  Because
            we have the inactive list locked, any active threads that finish
            can't add themselves back to the inactive list.  So we will delete
            what we can. }
          if FInactive.Count > 0 then begin
            aThread := TffPooledThread(TffIntListItem(FInactive[0]).KeyAsInt);
            aThread.DieDieDie;
            FInactive.DeleteAt(0);
          end
          else
            break;
      finally
        FLock.Unlock;
      end;
    end;
  end;
  FMaxCount := aCount;
end;

{====================================================================}

{===TffThreadRequestItem=============================================}
constructor TffThreadRequestItem.Create(anEvent : TffThreadProcessEvent;
                                        aCookie : longInt);
begin
  inherited Create;
  FProcessEvent := anEvent;
  FProcessCookie := aCookie;
end;
{====================================================================}

end.
