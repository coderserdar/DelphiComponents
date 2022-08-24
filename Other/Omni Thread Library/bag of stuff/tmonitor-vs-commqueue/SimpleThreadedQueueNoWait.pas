unit SimpleThreadedQueueNoWait;

interface

uses
  Windows, SysUtils, Classes, SyncObjs;

type
  TSimpleThreadedQueue<T> = class
  strict private type
    TNode<T> = record
      Next: ^TNode<T>;
      Data: T;
    end;
  strict private
    FHeadNode, FTailNode: ^TNode<T>;
    FCriticalSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Item: T);
    function Dequeue(var Item: T): Boolean;
  end;

implementation

constructor TSimpleThreadedQueue<T>.Create;
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FCriticalSection, 4000);
end;

destructor TSimpleThreadedQueue<T>.Destroy;
var
  Node: Pointer;
begin
  FCriticalSection.Enter;
  try
    while FHeadNode <> nil do
    begin
      Node := FHeadNode.Next;
      Finalize(FHeadNode.Data); //can't just call Dispose due to a compiler bug (QC 79818)
      FreeMem(FHeadNode);
      FHeadNode := Node;
    end;
  finally
    FCriticalSection.Leave;
  end;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TSimpleThreadedQueue<T>.Enqueue(const Item: T);
var
  NewNode: ^TNode<T>;
begin
  NewNode := AllocMem(SizeOf(TNode<T>)); //QC 79818 again -> don't call New
  NewNode.Data := Item;
  FCriticalSection.Enter;
  try
    if FHeadNode = nil then
      FHeadNode := Pointer(NewNode)
    else
      FTailNode.Next := Pointer(NewNode);
    FTailNode := Pointer(NewNode);
  finally
    FCriticalSection.Leave;
  end;
end;

function TSimpleThreadedQueue<T>.Dequeue(var Item: T): Boolean;
var
  OldNode: ^TNode<T>;
begin
  FCriticalSection.Enter;
  try
    Result := (FHeadNode <> nil);
    if not Result then Exit;
    OldNode := Pointer(FHeadNode);
    Item := OldNode.Data;
    FHeadNode := Pointer(OldNode.Next);
  finally
    FCriticalSection.Leave;
  end;
  Finalize(OldNode.Data);
  FreeMem(OldNode);
end;

end.
