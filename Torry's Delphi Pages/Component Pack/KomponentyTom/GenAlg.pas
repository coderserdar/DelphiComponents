unit GenAlg;

interface

uses
  SysUtils;

type
  EGenericAlg = class(Exception);

  TGenericStruct = class
  public
    function IsEmpty: Boolean; virtual; abstract;
  end;

  PQueueItem = ^TQueueItem;
  TQueueItem = record
    Data: Pointer;
    Next: PQueueItem;
  end;

  TQueue = class(TGenericStruct)
  private
    Head, Tail: PQueueItem;
  public
    destructor Destroy; override;
    function IsEmpty: Boolean; override;
    procedure Add(Data: Pointer);
    function Remove: Pointer;
  end;

  PStackItem = ^TStackItem;
  TStackItem = record
    Data: Pointer;
    Previous: PStackItem;
  end;

  TStack = class(TGenericStruct)
  private
    Top: PStackItem;
  public
    destructor Destroy; override;
    function IsEmpty: Boolean; override;
    procedure Push(Data: Pointer);
    function Pop: Pointer;
  end;

  PDoubleListItem = ^TDoubleListItem;
  TDoubleListItem = record
    Data: Pointer;
    Next, Previous: PDoubleListItem;
  end;

implementation

{ TQueue }

destructor TQueue.Destroy;
var
  TempItem: PQueueItem;
begin
  while (Head <> nil) do begin
    TempItem := Head^.Next;
    Dispose(Head);
    Head := TempItem;
  end;
  inherited Destroy;
end;

function TQueue.IsEmpty: Boolean;
begin
  Result := (Head = nil);
end;

procedure TQueue.Add(Data: Pointer);
var
  TempItem: PQueueItem;
begin
  New(TempItem);
  TempItem^.Data := Data;
  TempItem^.Next := nil;
  if (Head = nil) then
    Head := TempItem
  else
    Tail^.Next := TempItem;
  Tail := TempItem;
end;

function TQueue.Remove: Pointer;
var
  TempItem: PQueueItem;
begin
  if IsEmpty then
    raise EGenericAlg.Create('Queue is empty');
  Result := Head.Data;
  TempItem := Head;
  Head := Head^.Next;
  Dispose(TempItem);
end;

{ TStack }

destructor TStack.Destroy;
var
  TempItem: PStackItem;
begin
  while (Top <> nil) do begin
    TempItem := Top;
    Top := Top^.Previous;
    Dispose(TempItem);
  end;
  inherited Destroy;
end;

function TStack.IsEmpty: Boolean;
begin
  Result := (Top = nil);
end;

function TStack.Pop: Pointer;
var
  TempItem: PStackItem;
begin
  if IsEmpty then
    raise EGenericAlg.Create('Stack is empty');
  TempItem := Top;
  Result := TempItem^.Data;
  Top := TempItem^.Previous;
  Dispose(TempItem);
end;

procedure TStack.Push(Data: Pointer);
var
  TempItem: PStackItem;
begin
  New(TempItem);
  TempItem^.Data := Data;
  TempItem^.Previous := Top;
  Top := TempItem;
end;

{ TDoubleList }

end.
