unit SimpleThreadedQueueSem;

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs;

type
  TSimpleThreadedQueue<T> = class
  strict private
    FQueue: TQueue<T>;
    FCriticalSection: TCriticalSection;
    FSemaphore: TSemaphore;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Item: T);
    function Dequeue(var Item: T; Timeout: LongWord = INFINITE): TWaitResult;
  end;

implementation

constructor TSimpleThreadedQueue<T>.Create;
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FQueue := TQueue<T>.Create;
  FSemaphore := TSemaphore.Create(nil, 0, MaxInt, '');
end;

destructor TSimpleThreadedQueue<T>.Destroy;
begin
  FreeAndNil(FQueue);
  FCriticalSection.Free;
  FSemaphore.Free;
  inherited Destroy;
end;

procedure TSimpleThreadedQueue<T>.Enqueue(const Item: T);
begin
  FCriticalSection.Enter;
  try
    FQueue.Enqueue(Item);
  finally
    FCriticalSection.Leave;
  end;
  FSemaphore.Release;
end;

function TSimpleThreadedQueue<T>.Dequeue(var Item: T; Timeout: LongWord): TWaitResult;
begin
  Result := FSemaphore.WaitFor(Timeout);
  if Result <> wrSignaled then Exit;
  FCriticalSection.Enter;
  try
    Item := FQueue.Dequeue;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
