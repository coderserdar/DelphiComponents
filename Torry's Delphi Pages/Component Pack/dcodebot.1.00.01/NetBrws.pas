
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit NetBrws;

interface

{$I STD.INC}

uses  
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TNetScope = (nsConnected, nsGlobal, nsRemembered, nsContext);
  TNetResourceType = (nrAny, nrDisk, nrPrint);
  TNetDisplay = (ndDomain, ndGeneric, ndServer, ndShare, ndFile, ndGroup,
    ndNetwork, ndRoot, ndShareAdmin, ndDirectory, ndTree, ndNDSContainer);
  TNetUsage = set of (nuConnectable, nuContainer);

  TNetworkItems = class;

  TNetworkItem = class
  private
    FScope: TNetScope;
    FResourceType: TNetResourceType;
    FDisplay: TNetDisplay;
    FUsage: TNetUsage;
    FLocalName: string;
    FRemoteName: string;
    FComment: string;
    FProvider: string;
    FSubItems: TNetworkItems;
  public
    constructor Create;
    destructor Destroy; override;
    property Scope: TNetScope read FScope;
    property ResourceType: TNetResourceType read FResourceType;
    property Display: TNetDisplay read FDisplay;
    property Usage: TNetUsage read FUsage;
    property LocalName: string read FLocalName;
    property RemoteName: string read FRemoteName;
    property Comment: string read FComment;
    property Provider: string read FProvider;
    property SubItems: TNetworkItems read FSubItems;
  end;

  TNetworkItems = class
  private
    FList: TList;
    procedure SetItem(Index: Integer; Value: TNetworkItem);
    function GetItem(Index: Integer): TNetworkItem;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Item: TNetworkItem);
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TNetworkItem read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  TNetworkBrowser = class(TComponent)
  private
    FItems: TNetworkItems;
    FScope: TNetScope;
    FResourceType: TNetResourceType;
    FUsage: TNetUsage;
    FActive: Boolean;
    procedure Refresh;
    procedure SetActive(Value: Boolean);
    procedure SetScope(Value: TNetScope);
    procedure SetResourceType(Value: TNetResourceType);
    procedure SetUsage(Value: TNetUsage);
    procedure EnumerateNet(NetItems: TNetworkItems; NetResource: PNetResource);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property Items: TNetworkItems read FItems;
  published
    property Scope: TNetScope read FScope write SetScope default nsGlobal;
    property ResourceType: TNetResourceType read FResourceType write SetResourceType default nrAny;
    property Usage: TNetUsage read FUsage write SetUsage default [];
    property Active: Boolean read FActive write SetActive default False;
  end;

implementation

type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array [0..0] of TNetResource;

{ TNetworkItem }

constructor TNetworkItem.Create;
begin
  inherited;
  FSubItems := TNetworkItems.Create;
end;

destructor TNetworkItem.Destroy;
begin
  if FSubItems <> nil then
    FSubItems.Free;
  inherited;
end;

{ TNetworkItems }

constructor TNetworkItems.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TNetworkItems.Destroy;
begin
  Clear;
  if FList <> nil then
    FList.Free;
  inherited;
end;

procedure TNetworkItems.SetItem(Index: Integer; Value: TNetworkItem);
begin
  if (FList.Items[Index] <> nil) and (FList.Items[Index] <> Value) then
    TNetworkItem(FList.Items[Index]).Free;
  FList.Items[Index] := Value;
end;

function TNetworkItems.GetItem(Index: Integer): TNetworkItem;
begin
  Result := TNetworkItem(FList.Items[Index]);
end;

procedure TNetworkItems.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

procedure TNetworkItems.Add(Item: TNetworkItem);
begin
  FList.Add(Item);
end;

procedure TNetworkItems.Delete(Index: Integer);
begin
  if FList.Items[Index] <> nil then
    TNetworkItem(FList.Items[Index]).Free;
  FList.Delete(Index);
end;

function TNetworkItems.GetCount: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
    Result := 0;
end;

{ TNetworkBrowser }

constructor TNetworkBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TNetworkItems.Create;
  FScope := nsGlobal;
  FResourceType := nrAny;
  FUsage := [];
end;

destructor TNetworkBrowser.Destroy;
begin
  if FItems <> nil then
    FItems.Free;
  inherited;
end;

procedure TNetworkBrowser.EnumerateNet(NetItems: TNetworkItems; NetResource: PNetResource);
var
  EnumResult: Integer;
  EnumHandle: THandle;
  BufferSize, NumEntries: LongWord;
  NetResources: PNetResourceArray;
  NewItem: TNetworkItem;
  NetScope, NetType, dwUsage: Integer;
begin
  BufferSize := 16384;
  NumEntries := $FFFFFFFF;
  case FScope of
    nsConnected: NetScope := RESOURCE_CONNECTED;
    nsGlobal: NetScope := RESOURCE_GLOBALNET;
    nsRemembered: NetScope := RESOURCE_REMEMBERED;
    nsContext: NetScope := RESOURCE_CONTEXT;
    else
      NetScope := RESOURCE_GLOBALNET;
  end;
  case FResourceType of
    nrAny: NetType := RESOURCETYPE_ANY;
    nrDisk: NetType := RESOURCETYPE_DISK;
    nrPrint: NetType := RESOURCETYPE_PRINT;
    else
      NetType := RESOURCETYPE_ANY;
  end;
  dwUsage := 0;
  if nuConnectable in FUsage then
    dwUsage := dwUsage or RESOURCEUSAGE_CONNECTABLE;
  if nuContainer in FUsage then
    dwUsage := dwUsage or RESOURCEUSAGE_CONTAINER;
  if WNetOpenEnum(NetScope, NetType, dwUsage, NetResource, EnumHandle) <> NO_ERROR then
    Exit;
  GetMem(NetResources, BufferSize);
  repeat
    EnumResult := WNetEnumResource(EnumHandle, NumEntries, NetResources, BufferSize);
    if EnumResult = NO_ERROR then
      for i := 0 to NumEntries - 1 do
      begin
        NewItem := TNetworkItem.Create;
        case NetResources[i].dwScope of
          RESOURCE_CONNECTED: NewItem.FScope := nsConnected;
          RESOURCE_GLOBALNET: NewItem.FScope := nsGlobal;
          RESOURCE_REMEMBERED: NewItem.FScope := nsRemembered;
          RESOURCE_CONTEXT: NewItem.FScope := nsContext;
          else
            NewItem.FScope := nsGlobal;
        end;
        case NetResources[i].dwType of
          RESOURCETYPE_ANY: NewItem.FResourceType := nrAny;
          RESOURCETYPE_DISK: NewItem.FResourceType := nrDisk;
          RESOURCETYPE_PRINT: NewItem.FResourceType := nrPrint;
        else
          NewItem.FResourceType := nrAny;
        end;
        case NetResources[i].dwDisplayType of
          RESOURCEDISPLAYTYPE_GENERIC:
            NewItem.FDisplay := ndGeneric;
          RESOURCEDISPLAYTYPE_DOMAIN:
            NewItem.FDisplay := ndDomain;
          RESOURCEDISPLAYTYPE_SERVER:
            NewItem.FDisplay := ndServer;
          RESOURCEDISPLAYTYPE_SHARE:
            NewItem.FDisplay := ndShare;
          RESOURCEDISPLAYTYPE_FILE:
            NewItem.FDisplay := ndFile;
          RESOURCEDISPLAYTYPE_GROUP:
            NewItem.FDisplay := ndGroup;
          RESOURCEDISPLAYTYPE_NETWORK:
            NewItem.FDisplay := ndNetwork;
          RESOURCEDISPLAYTYPE_ROOT:
            NewItem.FDisplay := ndRoot;
          RESOURCEDISPLAYTYPE_SHAREADMIN:
            NewItem.FDisplay := ndShareAdmin;
          RESOURCEDISPLAYTYPE_DIRECTORY:
            NewItem.FDisplay := ndDirectory;
          RESOURCEDISPLAYTYPE_TREE:
            NewItem.FDisplay := ndTree;
          RESOURCEDISPLAYTYPE_NDSCONTAINER:
            NewItem.FDisplay := ndNDSContainer
          else
            NewItem.FDisplay := ndGeneric;
        end;
        NewItem.FUsage := [];
        if NetResources[i].dwUsage and RESOURCEUSAGE_CONNECTABLE <> 0 then
          Include(NewItem.FUsage, nuConnectable);
        if NetResources[i].dwUsage and RESOURCEUSAGE_CONTAINER <> 0 then
          Include(NewItem.FUsage, nuContainer);
        NewItem.FLocalName := NetResources[i].lpLocalName;
        NewItem.FRemoteName := NetResources[i].lpRemoteName;
        NewItem.FComment := NetResources[i].lpComment;
        NewItem.FProvider := NetResources[i].lpProvider;
        NetItems.Add(NewItem);
        { if container, call recursively }
        if (nuContainer in NewItem.FUsage) and (FScope <> nsContext) then
          EnumerateNet(NewItem.FSubItems, @NetResources[i])
      end;
  until EnumResult = ERROR_NO_MORE_ITEMS;
  FreeMem(NetResources);
  WNetCloseEnum(EnumHandle);
end;

procedure TNetworkBrowser.Refresh;
begin
  FItems.Clear;
  if FActive then EnumerateNet(FItems, nil);
end;

procedure TNetworkBrowser.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Refresh;
  end;
end;

procedure TNetworkBrowser.SetScope(Value: TNetScope);
begin
  if Value <> FScope then
  begin
    FScope := Value;
    Refresh;
  end;
end;

procedure TNetworkBrowser.SetResourceType(Value: TNetResourceType);
begin
  if Value <> FResourceType then
  begin
    FResourceType := Value;
    Refresh;
  end;
end;

procedure TNetworkBrowser.SetUsage(Value: TNetUsage);
begin
  if Value <> FUsage then
  begin
    FUsage := Value;
    Refresh;
  end;
end;

procedure TNetworkBrowser.Open;
begin
  Active := True;
end;

procedure TNetworkBrowser.Close;
begin
  Active := False;
end;

end.
