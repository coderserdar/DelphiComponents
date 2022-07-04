{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Non-visual components                         }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgTools;

interface
uses Messages, Windows, SysUtils, Classes, vgSystem, TypInfo;

type
{ TItem }
  TItemList = class;

  TItem = class(TComponent)
  private
    FItemList: TItemList;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetItemList(Value: TItemList);
  protected
    procedure ItemEvent(Event: Integer); virtual;
    function GetItemName: string; virtual;
    procedure Notify(Event: Integer; Data: Pointer); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
  public
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property Index: Integer read GetIndex write SetIndex stored False;
    property ItemList: TItemList read FItemList write SetItemList;
  end;

{ TItemList }
  TItemListDesigner = class;

  TItemList = class(TItem)
  private
    FDesigner: TItemListDesigner;
    FItems: TList;
    FUpdateCount: Integer;
    function GetCount: Integer;
    function GetInUpdate: Boolean;
    function GetItem(Index: Integer): Pointer;
  protected
    procedure ItemListEvent(Item: TItem; Event: Integer); virtual;
    procedure GetChildren(Proc: TGetChildProc{$IFDEF _D3_}; Root: TComponent{$ENDIF}); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChildren(Event: Integer; Data: Pointer);
    procedure InsertItem(Item: TItem); virtual;
    procedure RemoveItem(Item: TItem); virtual;
    procedure SetName(const Value: TComponentName); override;
    property UpdateCount: Integer read FUpdateCount;
    property InUpdate: Boolean read GetInUpdate;
  public
    destructor Destroy; override;
    procedure DestroyingChildren;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Clear; virtual;
    function FindItem(const AName: string): TItem;
    function ItemByName(const AName: string): TItem;
    function HasChildren: Boolean;
    function IndexOf(Item: TItem): Integer;
    procedure Sort(Compare: TListSortCompare); virtual;
    property Count: Integer read GetCount;
    property Designer: TItemListDesigner read FDesigner;
    property Items[Index: Integer]: Pointer read GetItem; default;
  end;

{ TItemListDesigner }
  TItemListDesigner = class(TObject)
  private
    FItemList: TItemList;
  public
    constructor Create(AItemList: TItemList);
    destructor Destroy; override;
    procedure Event(Item: TItem; Event: Integer); virtual;
    property ItemList: TItemList read FItemList;
  end;

{ TOwnerList }
  PComponent = ^TComponent;
  TOwnerManager = class;

  TOwnerList = class(TItem)
  private
    FComponent: TComponent;
    FVariable: PComponent;
    FOwners: TList;
    procedure SetComponent(AComponent: TComponent; AVariable: PComponent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure InsertOwner(AOwner: TComponent);
    procedure RemoveOwner(AOwner: TComponent);
    property Component: TComponent read FComponent;
    property Variable: PComponent read FVariable;
  end;

{ TOwnerManager }
  TOwnerManager = class(TItemList)
  private
    function GetItem(Index: Integer): TOwnerList;
  public
    function FindOwnerList(Variable: PComponent): TOwnerList;
    function IndexOfVariable(Variable: PComponent): Integer;
    procedure InsertOwner(Variable: PComponent; ComponentClass: TComponentClass; AOwner: TComponent);
    procedure RemoveOwner(Variable: PComponent; AOwner: TComponent);
    property Items[Index: Integer]: TOwnerList read GetItem;
  end;


{ TvgThread }
  TvgThread = class(TComponent)
  private
    FThread: TThreadEx;
    FSyncMethod: TNotifyEvent;
    FSyncParams: Pointer;
    FStreamedSuspended: Boolean;
    FOnExecute: TNotifyEvent;
    FOnException: TNotifyEvent ;
    procedure InternalSynchronize;
    function GetHandle: THandle;
    function GetOnTerminate: TNotifyEvent;
    procedure SetOnTerminate(Value: TNotifyEvent);
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function GetReturnValue: Integer;
    procedure SetReturnValue(Value: Integer);
    function GetSuspended: Boolean;
    procedure SetSuspended(Value: Boolean);
    function GetTerminated: Boolean;
  protected
    procedure DoExecute(Sender: TObject); virtual;
    procedure DoException(Sender: TObject); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Synchronize(Method: TThreadMethod);
    procedure SynchronizeEx(Method: TNotifyEvent; Params: Pointer);
    procedure Suspend;
    procedure Resume;
    procedure Terminate(Hard: Boolean);
    function WaitFor: Integer;
    property ReturnValue: Integer read GetReturnValue write SetReturnValue;
    property Handle: THandle read GetHandle;
    property Terminated: Boolean read GetTerminated;
  published
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read GetSuspended write SetSuspended default True;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnTerminate: TNotifyEvent read GetOnTerminate write SetOnTerminate;
    property OnException: TNotifyEvent read FOnException write FOnException;
  end;

{ TBroadcaster }
  TBroadcastEvent = procedure (Sender: TObject; var Msg: TMessage;
    Item, Data: TObject; var Handled: Boolean) of object;

  TBroadcaster = class(TComponent)
  private
    FItems: TList;
    FDatas: TList;
    FOnBroadcast: TBroadcastEvent;
    function GetCount: Integer;
    function GetData(Index: Integer): TObject;
    function GetItem(Index: Integer): TObject;
  protected
    { Protected declarations }
    function BroadcastMessage(var Msg: TMessage; Item, Data: TObject): Boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Broadcast(Msg: Cardinal; WParam, LParam: Longint): Longint; virtual;
    function IndexOf(AObject: TObject): Integer;
    procedure InsertObject(AObject, AData: TObject);
    procedure RemoveObject(AObject: TObject);
    property Count: Integer read GetCount;
    property Data[Index: Integer]: TObject read GetData;
    property Item[Index: Integer]: TObject read GetItem;
  published
    { Published declarations }
    property OnBroadcast: TBroadcastEvent read FOnBroadcast write FOnBroadcast;
  end;

{ TNamedCollectionItem }
  TNamedCollectionItem = class(TCollectionItem)
  private
    FName: string;
  protected
    function GetDisplayName: string; {$IFDEF _D3_}override;{$ENDIF}
    procedure SetDisplayName(const Value: string); {$IFDEF _D3_}override;{$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write SetDisplayName;
  end;

{ TNamedItemsCollection }
{$IFDEF _D4_}
  TNamedItemsCollection = class(TOwnedCollection)
{$ELSE}
  TNamedItemsCollection = class(TCollection)
{$ENDIF}
  public
    function Duplicates: Boolean; virtual;
    function FindItem(const Name: string): TNamedCollectionItem;
    function ItemByName(const Name: string): TNamedCollectionItem;
    function IndexOf(const Name: string): Integer;
  end;

const
  { Item events }
  ieItemChanged              =  0;
  ieItemListChanged          = -1;
  ieItemListLast             = ieItemListChanged;

implementation
uses Consts, vgVCLRes, vgUtils;

{ TItem }
destructor TItem.Destroy;
begin
  SetItemList(nil);
  inherited;
end;

function TItem.GetIndex: Integer;
begin
  if Assigned(FItemList) then
    Result := FItemList.IndexOf(Self) else
    Result := -1;
end;

procedure TItem.ItemEvent(Event: Integer);
begin
  if Assigned(FItemList) then FItemList.ItemListEvent(Self, Event);
end;

function TItem.GetItemName: string;
begin
  Result := '';
end;

function TItem.GetParentComponent: TComponent;
begin
  Result := ItemList;
end;

function TItem.HasParent: Boolean;
begin
  Result := Assigned(FItemList);
end;

procedure TItem.Notify(Event: Integer; Data: Pointer);
begin
end;

procedure TItem.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := FItemList.FItems.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      FItemList.FItems.Delete(CurIndex);
      FItemList.FItems.Insert(Value, Self);
      ItemEvent(ieItemListChanged);
    end;
  end;
end;

procedure TItem.SetItemList(Value: TItemList);
begin
  if (FItemList <> Value) then
  begin
    if Assigned(FItemList) then FItemList.RemoveItem(Self);
    if Assigned(Value) then Value.InsertItem(Self);
  end;
end;

procedure TItem.SetParentComponent(AParent: TComponent);
begin
  if (AParent is TItemList) then ItemList := AParent as TItemList;
end;

{ TItemList }
destructor TItemList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TItemList.DestroyingChildren;
var
  I: Integer;
  Item: TItem;
begin
  Destroying;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if Item is TItemList then
      TItemList(Item).DestroyingChildren;
  end;
end;

function TItemList.GetCount: Integer;
begin
  Result := ListCount(FItems);
end;

function TItemList.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TItemList.GetItem(Index: Integer): Pointer;
begin
  Result := ListItem(FItems, Index);
end;

procedure TItemList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TItemList.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TItemList.FindItem(const AName: string): TItem;
var
  I: Integer;
begin
  if Assigned(FItems) then
    for I := 0 to FItems.Count - 1 do
    begin
      Result := FItems.List^[I];
      if AnsiCompareText(Result.GetItemName, AName) = 0 then Exit;
    end;
  Result := nil;
end;

function TItemList.ItemByName(const AName: string): TItem;
begin
  Result := FindItem(AName);
  if not Assigned(Result) then
    raise EInvalidOp.Create(FmtLoadStr(SItemNotFound, [AName]));
end;

procedure TItemList.Clear;
begin
  ListDestroyAll(FItems);
end;

procedure TItemList.ItemListEvent(Item: TItem; Event: Integer);
begin
  if Assigned(FDesigner) then FDesigner.Event(Item, Event);
end;

procedure TItemList.GetChildren(Proc: TGetChildProc{$IFDEF _D3_}; Root: TComponent{$ENDIF});
var
  I: Integer;
  Item: TItem;
begin
  for I := 0 to ListCount(FItems) - 1 do
  begin
    Item := FItems.List^[I];
    Proc(Item);
  end;
end;

procedure TItemList.SetChildOrder(Component: TComponent; Order: Integer);
begin
  if ListIndexOf(FItems, Component) >= 0 then (Component as TItem).Index := Order;
end;

procedure TItemList.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    I := ListIndexOf(FItems, AComponent);
    if I >= 0 then RemoveItem(AComponent as TItem);
  end;
end;

procedure TItemList.NotifyChildren(Event: Integer; Data: Pointer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TItem(Items[I]).Notify(Event, Data);
end;

function TItemList.HasChildren: Boolean;
begin
  Result := Assigned(FItems);
end;

function TItemList.IndexOf(Item: TItem): Integer;
begin
  Result := ListIndexOf(FItems, Item);
end;

procedure TItemList.InsertItem(Item: TItem);
begin
  if ListIndexOf(FItems, Item) < 0 then
  begin
    FreeNotification(Item);
    ListAdd(FItems, Item);
    Item.FItemList := Self;
    ItemListEvent(Item, ieItemListChanged);
  end;
end;

procedure TItemList.RemoveItem(Item: TItem);
begin
  if ListIndexOf(FItems, Item) >= 0 then
  begin
    ListRemove(FItems, Item);
    Item.FItemList := nil;
    ItemListEvent(Item, ieItemListChanged);
  end;
end;

procedure TItemList.SetName(const Value: TComponentName);
var
  I: Integer;
  OldName, ItemName, NamePrefix: TComponentName;
  Item: TItem;
begin
  OldName := Name;
  inherited SetName(Value);
  if (csDesigning in ComponentState) and (Name <> OldName) then
    { In design mode the name of the items should track the item list name }
    for I := 0 to ListCount(FItems) - 1 do
    begin
      Item := FItems.List^[I];
      if Item.Owner = Owner then
      begin
        ItemName := Item.Name;
        NamePrefix := ItemName;
        if Length(NamePrefix) > Length(OldName) then
        begin
          SetLength(NamePrefix, Length(OldName));
          if CompareText(OldName, NamePrefix) = 0 then
          begin
            System.Delete(ItemName, 1, Length(OldName));
            System.Insert(Value, ItemName, 1);
            try
              Item.Name := ItemName;
            except
              on EComponentError do {Ignore rename errors };
            end;
          end;
        end;
      end;
    end;
end;

procedure TItemList.Sort(Compare: TListSortCompare);
begin
  ListSort(FItems, Compare);
end;

{ TItemListDesigner }
constructor TItemListDesigner.Create(AItemList: TItemList);
begin
  FItemList := AItemList;
  FItemList.FDesigner := Self;
end;

destructor TItemListDesigner.Destroy;
begin
  FItemList.FDesigner := nil;
  inherited;
end;

procedure TItemListDesigner.Event(Item: TItem; Event: Integer);
begin
end;

{ TOwnerList }
destructor TOwnerList.Destroy;
begin
  SetComponent(nil, nil);
  inherited;
end;

procedure TOwnerList.InsertOwner(AOwner: TComponent);
begin
  if Assigned(FComponent) and Assigned(AOwner) then
  begin
    FreeNotification(AOwner);
    ListAdd(FOwners, AOwner);
  end;
end;

procedure TOwnerList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(FComponent) then
    if (AComponent = FComponent) then Destroy else RemoveOwner(AComponent);
end;

procedure TOwnerList.RemoveOwner(AOwner: TComponent);
var
  I: Integer;
begin
  I := ListIndexOf(FOwners, AOwner);
  if (I >= 0) then
  begin
    ListDelete(FOwners, I);
    if not Assigned(FOwners) then
    begin
      if Assigned(FVariable) then FVariable^ := nil;
      FComponent.Free;
    end;
  end;
end;

procedure TOwnerList.SetComponent(AComponent: TComponent; AVariable: PComponent);
begin
  if (FComponent <> AComponent) then
  begin
    ListClear(FOwners);
    FComponent := AComponent;
    FVariable := AVariable;
    if Assigned(FComponent) then FreeNotification(FComponent) else FVariable := nil;
  end;
end;

{ TOwnerManager }
function TOwnerManager.GetItem(Index: Integer): TOwnerList;
begin
  Result := inherited Items[Index];
end;

function TOwnerManager.FindOwnerList(Variable: PComponent): TOwnerList;
var
  I: Integer;
begin
  I := IndexOfVariable(Variable);
  if I >= 0 then Result := Items[I] else Result := nil;
end;

function TOwnerManager.IndexOfVariable(Variable: PComponent): Integer;
var
  Item: TOwnerList;
begin
  for Result := 0 to Count - 1 do
  begin
    Item := Items[Result];
    if Item.Variable = Variable then Exit;
  end;
  Result := -1;
end;

procedure TOwnerManager.InsertOwner(Variable: PComponent; ComponentClass: TComponentClass; AOwner: TComponent);
var
  Item: TOwnerList;
begin
  Item := FindOwnerList(Variable);
  if not Assigned(Item) then
  begin
    Item := TOwnerList.Create(Self);
    try
      Item.ItemList := Self;
      Variable^ := ComponentClass.Create(Item);
      try
        Item.SetComponent(Variable^, Variable);
      except
        Variable^.Free;
        Variable^ := nil;
        raise;
      end;
    except
      Item.Free;
      raise;
    end;
  end;
  Item.InsertOwner(AOwner);
end;

procedure TOwnerManager.RemoveOwner(Variable: PComponent; AOwner: TComponent);
var
  Item: TOwnerList;
begin
  Item := FindOwnerList(Variable);
  if Assigned(Item) then Item.RemoveOwner(AOwner);
end;

{ TvgThread }
type
  TThreadHack = class(TThreadEx);

constructor TvgThread.Create(AOwner: TComponent);
begin
  inherited;
  FStreamedSuspended := True;
  FThread := TThreadEx.Create(True);
  FThread.OnExecute := DoExecute;
  FThread.OnException := DoException;
end;

destructor TvgThread.Destroy;
begin
  Terminate(True);
  FThread.Free;
  inherited;
end;

procedure TvgThread.DoExecute(Sender: TObject);
begin
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

procedure TvgThread.DoException(Sender: TObject);
begin
  if Assigned(FOnException) then FOnException(Self);
end;


function TvgThread.GetHandle: THandle;
begin
  Result := FThread.Handle;
end;

function TvgThread.GetTerminated: Boolean;
begin
  Result := TThreadHack(FThread).Terminated;
end;

function TvgThread.GetOnTerminate: TNotifyEvent;
begin
  Result := FThread.OnTerminate;
end;

function TvgThread.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TvgThread.GetReturnValue: Integer;
begin
  Result := TThreadHack(FThread).ReturnValue;
end;

function TvgThread.GetSuspended: Boolean;
begin
  if not (csDesigning in ComponentState) then
    Result := FThread.Suspended else
    Result := FStreamedSuspended;
end;

procedure TvgThread.Execute;
begin
  Terminate(True);
  FThread.Resume;
end;

procedure TvgThread.Loaded;
begin
  inherited;
  SetSuspended(FStreamedSuspended);
end;

procedure TvgThread.SetOnTerminate(Value: TNotifyEvent);
begin
  FThread.OnTerminate := Value;
end;

procedure TvgThread.SetPriority(Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TvgThread.SetReturnValue(Value: Integer);
begin
  TThreadHack(FThread).ReturnValue := Value;
end;

procedure TvgThread.SetSuspended(Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (csLoading in ComponentState) then
      FStreamedSuspended := Value else
      FThread.Suspended := Value;
  end else
    FStreamedSuspended := Value;
end;

procedure TvgThread.Suspend;
begin
  FThread.Suspend;
end;

procedure TvgThread.Synchronize(Method: TThreadMethod);
begin
  TThreadHack(FThread).Synchronize(Method);
end;

procedure TvgThread.InternalSynchronize;
begin
  FSyncMethod(FSyncParams);
end;

procedure TvgThread.SynchronizeEx(Method: TNotifyEvent; Params: Pointer);
begin
  if Assigned(Method) then
  begin
    FSyncMethod := Method; FSyncParams := Params;
    try
      TThreadHack(FThread).Synchronize(InternalSynchronize);
    finally
      FSyncMethod := nil; FSyncParams := nil;
    end;
  end;
end;

procedure TvgThread.Resume;
begin
  FThread.Resume;
end;

procedure TvgThread.Terminate(Hard: Boolean);
var
  FTmp: TThreadEx;
begin
  if Hard then
  begin
    TerminateThread(FThread.Handle, 0);
    FTmp := TThreadEx.Create(True);
    try
      FTmp.Priority := Self.Priority;
      FTmp.OnExecute := DoExecute;
      FTmp.OnTerminate := Self.OnTerminate;
    except
      FTmp.Free;
      raise;
    end;
    FThread.Free;
    FThread := FTmp;
  end else
    FThread.Terminate;
end;

function TvgThread.WaitFor: Integer;
begin
  Terminate(True);
  Result := FThread.WaitFor;
end;

{ TBroadcaster }

constructor TBroadcaster.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList.Create;
  FDatas := TList.Create;
end;

destructor TBroadcaster.Destroy;
begin
  FItems.Free;
  FDatas.Free;
  inherited;
end;

function TBroadcaster.Broadcast(Msg: Cardinal; WParam, LParam: Longint): Longint;
var
  I: Integer;
  Message: TMessage;
begin
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
  for I := 0 to Count - 1 do
    if BroadcastMessage(Message, Item[I], Data[I]) then Break;
  Result := Message.Result;
end;

function TBroadcaster.BroadcastMessage(var Msg: TMessage; Item, Data: TObject): Boolean;
begin
  Result := False;
  if Assigned(FOnBroadcast) then FOnBroadcast(Self, Msg, Item, Data, Result);
end;

function TBroadcaster.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBroadcaster.GetData(Index: Integer): TObject;
begin
  Result := FDatas[Index];
end;

function TBroadcaster.GetItem(Index: Integer): TObject;
begin
  Result := FItems[Index];
end;

function TBroadcaster.IndexOf(AObject: TObject): Integer;
begin
  Result := FItems.IndexOf(AObject);
end;

procedure TBroadcaster.InsertObject(AObject, AData: TObject);
var
  I: Integer;
begin
  I := FItems.IndexOf(AObject);
  if I < 0 then
  begin
    FItems.Add(AObject);
    FDatas.Add(AData);
    if (AObject is TComponent) then
      FreeNotification(TComponent(AObject));
  end;
end;

procedure TBroadcaster.RemoveObject(AObject: TObject);
var
  I: Integer;
begin
  I := FItems.IndexOf(AObject);
  if I >= 0 then
  begin
    FItems.Delete(I);
    FDatas.Delete(I);
  end;
end;

procedure TBroadcaster.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then RemoveObject(AComponent);
end;

{ TNamedCollectionItem }

procedure TNamedCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TNamedCollectionItem then
    Name := TNamedCollectionItem(Source).Name
  else
    inherited;
end;

function TNamedCollectionItem.GetDisplayName: string;
begin
  if FName = '' then
  {$IFDEF _D3_}
    Result := inherited GetDisplayName
  {$ELSE}
    Result := ClassName
  {$ENDIF}
  else
    Result := FName;
end;

procedure TNamedCollectionItem.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TNamedItemsCollection) then
    with TNamedItemsCollection(Collection) do
      if not Duplicates and (IndexOf(Value) >= 0) then
        raise Exception.Create(FmtLoadStr(SDuplicateItem, [Value]));
  FName := Value;
  inherited;
end;

{ TNamedItemsCollection }
function TNamedItemsCollection.Duplicates: Boolean;
begin
  Result := False;
end;

function TNamedItemsCollection.FindItem(const Name: string): TNamedCollectionItem;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := TNamedCollectionItem(Items[I]) else
    Result := nil;
end;

function TNamedItemsCollection.ItemByName(const Name: string): TNamedCollectionItem;
begin
  Result := FindItem(Name);
  if not Assigned(Result) then
    raise EInvalidOp.Create(FmtLoadStr(SItemNotFound, [Name]));
end;

function TNamedItemsCollection.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TNamedCollectionItem(Items[Result]).Name, Name) = 0 then Exit;
  Result := -1;
end;

end.

