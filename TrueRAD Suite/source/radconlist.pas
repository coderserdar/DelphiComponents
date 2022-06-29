
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radconlist;

interface

uses Classes, RadCommon;

type
  TradConnectionList = class;

  TradConnection = class(TComponent)
  private
    FConnectionList: TradConnectionList;
    FSource: TComponent;
    FSourceInfo: TradClassInfo;
    FTarget: TComponent;
    FTargetInfo: TradClassInfo;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetConnectionList(Value: TradCOnnectionList);
    procedure SetSource(Value: TComponent);
    procedure SetTarget(Value: TComponent);
    function GetSourceInfo: TradClassInfo;
    function GetTargetInfo: TradClassInfo;
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetParentComponent(AParent: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function Connect: Boolean; virtual; abstract;
    procedure Unconnect; virtual; abstract;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property ConnectionList: TradConnectionList read FConnectionList write SetConnectionList;
    property Index: Integer read GetIndex write SetIndex stored False;
    property SourceInfo: TradClassInfo read GetSourceInfo;
    property TargetInfo: TradClassInfo read GetTargetInfo;
  published
    property Source: TComponent read FSource write SetSource;
    property Target: TComponent read FTarget write SetTarget;
  end;

  TradValueOptions = (voConstant, voComponentProperty, voEventProperty);

  TradConnectionList = class(TComponent)
  private
    FConnections: TList;
    procedure AddConnection(Connection: TradConnection);
    function GetConnection(Index: Integer): TradConnection;
    function GetConnectionCount: Integer;
    procedure RemoveConnection(Connection: TradConnection);
    procedure SetConnection(Index: Integer; Value: TradConnection);
  protected
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connections[Index: Integer]: TradConnection read GetConnection write SetConnection; default;
    property ConnectionCount: Integer read GetConnectionCount;
  end;

implementation

uses SysUtils, radvclpatch;

//------------------------------------------------------------------------------
// TradConnection implementation
//------------------------------------------------------------------------------
destructor TradConnection.Destroy;
begin
    if ConnectionList <> nil then
        ConnectionList.RemoveConnection(Self);
    inherited Destroy;
end;

//------------------------------------------------------------------------------
function TradConnection.GetIndex: Integer;
begin
    if ConnectionList <> nil then
        Result := ConnectionList.FConnections.IndexOf(Self)
    else
        Result := -1;
end;

//------------------------------------------------------------------------------
procedure TradConnection.SetIndex(Value: Integer);
var
    CurIndex, Count: Integer;
begin
    CurIndex := GetIndex;
    if CurIndex >= 0 then begin
        Count := ConnectionList.FConnections.Count;
        if Value < 0 then
            Value := 0;
        if Value >= Count then
            Value := Count - 1;
        if Value <> CurIndex then begin
            ConnectionList.FConnections.Delete(CurIndex);
            ConnectionList.FConnections.Insert(Value, Self);
        end;
    end;
end;

//------------------------------------------------------------------------------
function TradConnection.HasParent: Boolean;
begin
    if ConnectionList <> nil then
        Result := True
    else
        Result := inherited HasParent;
end;

//------------------------------------------------------------------------------
function TradConnection.GetParentComponent: TComponent;
begin
    if ConnectionList <> nil then
        Result := ConnectionList
    else
        Result := inherited GetParentComponent;
end;

//------------------------------------------------------------------------------
procedure TradConnection.SetParentComponent(AParent: TComponent);
begin
    if not (csLoading in ComponentState) and (AParent is TradConnectionList) then
        ConnectionList := TradConnectionList(AParent);
end;

//------------------------------------------------------------------------------
procedure TradConnection.SetConnectionList(Value: TradCOnnectionList);
begin
    if Value <> ConnectionList then begin
        if ConnectionList <> nil then
            ConnectionList.RemoveConnection(Self);
        if Value <> nil then
            Value.AddConnection(Self);
    end;
end;

//------------------------------------------------------------------------------
procedure TradConnection.ReadState(Reader: TReader);
begin
    inherited ReadState(Reader);
    if Reader.Parent is TradConnectionList then
        ConnectionList := TradConnectionList(Reader.Parent);
end;

//------------------------------------------------------------------------------
function TradConnection.GetSourceInfo: TradClassInfo;
begin
    if Assigned(FSource) then begin
        if Assigned(FSourceInfo) and (FSourceInfo.Owner <> FSource) then
            FSourceInfo := nil;
        if not Assigned(FSourceInfo) then
            FSourceInfo := TradClassRegistry.GetInfo(FSource);
        Result := FSourceInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradConnection.GetTargetInfo: TradClassInfo;
begin
    if Assigned(FTarget) then begin
        if Assigned(FTargetInfo) and (FTargetInfo.Owner <> FTarget) then
            FTargetInfo := nil;
        if not Assigned(FTargetInfo) then
            FTargetInfo := TradClassRegistry.GetInfo(FTarget);
        Result := FTargetInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
procedure TradConnection.SetSource(Value: TComponent);
begin
    if Assigned(Value) and not Assigned(TradClassRegistry.FindClass(Value.ClassName)) then
        raise Exception.Create('This is a not TrueRAD component!');

    if Assigned(FSource) then begin
        FSource := nil;
        Unconnect;
    end;

    FSource := Value;
    if Assigned(Value) then FSource.FreeNotification(Self);
end;

//------------------------------------------------------------------------------
procedure TradConnection.SetTarget(Value: TComponent);
begin
    if Assigned(Value) and not Assigned(TradClassRegistry.FindClass(Value.ClassName)) then
        raise Exception.Create('This is a not TrueRAD component!');

    if Assigned(FTarget) then begin
        FTarget := nil;
        Unconnect;
    end;

    FTarget := Value;
    if Assigned(Value) then FTarget.FreeNotification(Self);
end;

//------------------------------------------------------------------------------
procedure TradConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FSource then begin
            FSource := nil;
            Unconnect;
        end;
        if AComponent = FTarget then begin
            FTarget := nil;
            Unconnect;
        end;
    end;
end;

//------------------------------------------------------------------------------
// TradConnectionList implementation
//------------------------------------------------------------------------------
constructor TradConnectionList.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FConnections := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TradConnectionList.Destroy;
begin
    while FConnections.Count > 0 do
        TradConnection(FConnections.Last).Free;
    FConnections.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
    i: Integer;
    Connection: TradConnection;
begin
    for i := 0 to FConnections.Count - 1 do begin
        Connection := FConnections[i];
        if Connection.Owner = Root then
            Proc(Connection);
    end;
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.SetChildOrder(Component: TComponent; Order: Integer);
begin
    if FConnections.IndexOf(Component) >= 0 then
        (Component as TradConnection).Index := Order;
end;

//------------------------------------------------------------------------------
function TradConnectionList.GetConnection(Index: Integer): TradConnection;
begin
    Result := FConnections[Index];
end;

//------------------------------------------------------------------------------
function TradConnectionList.GetConnectionCount: Integer;
begin
    Result := FConnections.Count;
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.SetConnection(Index: Integer; Value: TradConnection);
begin
    TradConnection(FConnections[Index]).Assign(Value);
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if (AComponent is TradConnection) then
            RemoveConnection(TradConnection(AComponent));
    end;
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.AddConnection(Connection: TradConnection);
begin
    FConnections.Add(Connection);
    Connection.FConnectionList := Self;
    Connection.FreeNotification(Self);
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.RemoveConnection(Connection: TradConnection);
begin
    if FConnections.Remove(Connection) >= 0 then
        Connection.FConnectionList := nil;
end;

//------------------------------------------------------------------------------
procedure TradConnectionList.Loaded;
var
    i: Integer;
begin
    inherited Loaded;
    if not (csDesigning in ComponentState) then
        for i := 0 to FConnections.Count - 1 do
            TradConnection(FConnections[i]).Connect;
end;

end.
