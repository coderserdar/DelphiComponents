unit SimpleThreadedQueue;

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs;

type
  TSimpleThreadedQueue<T> = class
  strict private
    FQueue:  TQueue<T>;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Enqueue(const Item: T; Timeout: LongWord = INFINITE): TWaitResult;
    function Dequeue(var Item: T; Timeout: LongWord = INFINITE): TWaitResult;
    procedure Close;
    property Closed: Boolean read FClosed;
  end;

implementation

uses Diagnostics;

constructor TSimpleThreadedQueue<T>.Create;
begin
  inherited Create;
  FQueue := TQueue<T>.Create;
end;

destructor TSimpleThreadedQueue<T>.Destroy;
begin
  Close;
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TSimpleThreadedQueue<T>.Close;
begin
  if FClosed then Exit;
  FClosed := True;
  TMonitor.Enter(FQueue);
  try
    FQueue.Clear;
    TMonitor.PulseAll(FQueue); //notify any waiters Closed is now True
  finally
    TMonitor.Exit(FQueue);
  end;
end;

function TSimpleThreadedQueue<T>.Enqueue(const Item: T; Timeout: LongWord): TWaitResult;
begin
  if Closed then Exit(wrAbandoned);
  if not TMonitor.Enter(FQueue, Timeout) then Exit(wrTimeout);
  try
    if Closed then Exit(wrAbandoned);
    FQueue.Enqueue(Item);
    TMonitor.Pulse(FQueue);
    Result := wrSignaled;
  finally
    TMonitor.Exit(FQueue);
  end;
end;

function TSimpleThreadedQueue<T>.Dequeue(var Item: T; Timeout: LongWord): TWaitResult;
var
  Stopwatch: TStopwatch;
  TimeoutLeft: Int64;
begin
  if Closed then Exit(wrAbandoned);
  Stopwatch := TStopwatch.StartNew;
  if not TMonitor.Enter(FQueue, Timeout) then Exit(wrTimeout);
  try
    while not Closed and (FQueue.Count = 0) do
    begin
      TimeoutLeft := Timeout - Stopwatch.ElapsedMilliseconds;
      if TimeoutLeft < 0 then TimeoutLeft := 0;
      if not TMonitor.Wait(FQueue, LongWord(TimeoutLeft)) then Exit(wrTimeout);
    end;
    if Closed then Exit(wrAbandoned);
    Item := FQueue.Dequeue;
    Result := wrSignaled;
  finally
    TMonitor.Exit(FQueue);
  end;
end;

end.
