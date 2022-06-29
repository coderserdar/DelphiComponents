
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit rade2m;

interface

uses Classes, radcommon, radconlist;

type
  TradE2MConnection = class;

  TradParameterConnection = class(TCollectionItem)
  private
    FParameterName: String;
    FComponent: TComponent;
    FComponentInfo: TradClassInfo;
    FComponentProperty: String;
    FComponentPropertyIndex: Variant;
    FEventParameter: String;
    FConstant: String;
    FOptions: TradValueOptions;
    procedure SetComponentProperty(Value: String);
    function GetConnection: TradE2MConnection;
    function GetParameterInfo: TradParameterInfo;
    function GetComponentInfo: TradClassInfo;
    function GetComponentPropertyInfo: TradPropertyInfo;
    function GetEventParameterInfo: TradParameterInfo;
    procedure SetComponent(Value: TComponent);
    procedure ReadComponentPropertyIndexes(Reader: TReader);
    procedure WriteComponentPropertyIndexes(Writer: TWriter);
  protected
    function GetDisplayName: string; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    property Connection: TradE2MConnection read GetConnection;
    property ComponentInfo: TradClassInfo read GetComponentInfo;
    property ParameterInfo: TradParameterInfo read GetParameterInfo;
    property ComponentPropertyInfo: TradPropertyInfo read GetComponentPropertyInfo;
    property EventParameterInfo: TradParameterInfo read GetEventParameterInfo;
  published
    property ParameterName: String read FParameterName write FParameterName;
    property Component: TComponent read FComponent write SetComponent;
    property ComponentProperty: String read FComponentProperty write SetComponentProperty;
    property ComponentPropertyIndex: Variant read FComponentPropertyIndex write FComponentPropertyIndex stored False;
    property EventParameter: String read FEventParameter write FEventParameter;
    property Constant: String read FConstant write FConstant;
    property Options: TradValueOptions read FOptions write FOptions;
  end;

  TradParameterConnections = class(TCollection)
  private
    FMethod: String;
    FConnector: TradE2MConnection;
    procedure SetLink(Index: Integer; Value: TradParameterConnection);
    function GetLink(Index: Integer): TradParameterConnection;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AConnector: TradE2MConnection);
    function Add: TradParameterConnection;
    procedure Init(Method: TradMethodInfo);
    property Method: String read FMethod write FMethod;
    property Connection: TradE2MConnection read FConnector;
    property Items[Index: Integer]: TradParameterConnection read GetLink write SetLink; default;
  end;

  TradE2MConnection = class(TradConnection)
  private
    FSourceEvent: String;
    FTargetMethod: String;
    FTargetMethodParameters: TradParameterConnections;
    FSourceListener: TradListener;
    function GetSourceEventInfo: TradEventInfo;
    function GetTargetMethodInfo: TradMethodInfo;
    procedure SetTargetMethod(Value: String);
    procedure OnSourceEvent(Sender: TObject; Event: TradEventInfo);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: Boolean; override;
    procedure Unconnect; override;
    property SourceEventInfo: TradEventInfo read GetSourceEventInfo;
    property TargetMethodInfo: TradMethodInfo read GetTargetMethodInfo;
  published
    property SourceEvent: String read FSourceEvent write FSourceEvent;
    property TargetMethod: String read FTargetMethod write SetTargetMethod;
    property TargetMethodParameters: TradParameterConnections read FTargetMethodParameters write FTargetMethodParameters;
  end;

implementation

uses SysUtils;

//------------------------------------------------------------------------------
// TradParameterConnection implementation
//------------------------------------------------------------------------------
procedure TradParameterConnection.SetComponentProperty(Value: String);
begin
    FComponentProperty := Value;
    if not (csLoading in Connection.ComponentState) then begin
        if ComponentPropertyInfo <> nil then
            ComponentPropertyIndex := varArrayCreate([0, ComponentPropertyInfo.IndexCount - 1], varVariant)
        else
            ComponentPropertyIndex := varEmpty;
    end;
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetConnection: TradE2MConnection;
begin
    if Assigned(Collection) and (Collection is TradParameterConnections) then
        Result := TradParameterConnections(Collection).Connection
    else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetParameterInfo: TradParameterInfo;
var
    Param: TradParameterInfo;
    Con: TradE2MConnection;
begin
    Param := nil;
    Con := GetConnection;
    if Assigned(Con) and Assigned(Con.TargetMethodInfo) then
        Param := Con.TargetMethodInfo.Find(FParameterName);
    Result := Param;
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetComponentInfo: TradClassInfo;
begin
    if Assigned(FComponent) then begin
        if Assigned(FComponentInfo) and (FComponentInfo.Owner <> FComponent) then
            FComponentInfo := nil;
        if not Assigned(FComponentInfo) then
            FComponentInfo := TradClassRegistry.GetInfo(FComponent);
        Result := FComponentInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetComponentPropertyInfo: TradPropertyInfo;
var
    Prop: TradPropertyInfo;
begin
    Prop := nil;
    if Assigned(ComponentInfo) then
        Prop := ComponentInfo.FindProperty(FComponentProperty);
    Result := Prop;
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetEventParameterInfo: TradParameterInfo;
var
    Param: TradParameterInfo;
    Con: TradE2MConnection;
begin
    Param := nil;
    Con := GetConnection;
    if Assigned(Con) and Assigned(Con.SourceEventInfo) then
        Param := Con.SourceEventInfo.Find(FEventParameter);
    Result := Param;
end;

//------------------------------------------------------------------------------
procedure TradParameterConnection.SetComponent(Value: TComponent);
var
    Con: TradE2MConnection;
begin
    if Assigned(Value) and not Assigned(TradClassRegistry.FindClass(Value.ClassName)) then
        raise Exception.Create('This is a not TrueRAD component!');

    Con := GetConnection;

    if Assigned(FComponent) and Assigned(Con) then begin
        FComponentProperty := '';
        Con.Unconnect;
    end;

    FComponent := Value;

    if Assigned(Value) and Assigned(Con) then
        Value.FreeNotification(Con);
end;

//------------------------------------------------------------------------------
function TradParameterConnection.GetDisplayName: string;
begin
    if FParameterName = '' then
        Result := inherited GetDisplayName
    else
        Result := FParameterName;
end;

//------------------------------------------------------------------------------
procedure TradParameterConnection.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineProperty('ComponentPropertyIndexes', ReadComponentPropertyIndexes, WriteComponentPropertyIndexes, varIsArray(FComponentPropertyIndex));
end;

//------------------------------------------------------------------------------
procedure TradParameterConnection.ReadComponentPropertyIndexes(Reader: TReader);
var
    Indexes: array of Variant;
    i: Integer;
begin
    Reader.ReadListBegin;
    while not Reader.EndOfList do begin
        SetLength(Indexes, Length(Indexes) + 1);
        Indexes[Length(Indexes) - 1] := Reader.ReadString;
    end;
    Reader.ReadListEnd;
    FComponentPropertyIndex := varArrayCreate([0, Length(Indexes) - 1], varVariant);
    for i := 0 to Length(Indexes) - 1 do
        FComponentPropertyIndex[i] := Indexes[i];
end;

//------------------------------------------------------------------------------
procedure TradParameterConnection.WriteComponentPropertyIndexes(Writer: TWriter);
var
    i: Integer;
begin
    Writer.WriteListBegin;
    for i := 0 to varArrayHighBound(FComponentPropertyIndex, 1) do
        Writer.WriteString(FComponentPropertyIndex[i]);
    Writer.WriteListEnd;
end;

//------------------------------------------------------------------------------
// TradParameterConnections implementation
//------------------------------------------------------------------------------
constructor TradParameterConnections.Create(AConnector: TradE2MConnection);
begin
    inherited Create(TradParameterConnection);
    FConnector := AConnector;
end;

//------------------------------------------------------------------------------
function TradParameterConnections.Add: TradParameterConnection;
begin
    Result := TradParameterConnection(inherited Add);
end;

//------------------------------------------------------------------------------
procedure TradParameterConnections.Init(Method: TradMethodInfo);
var
    i: Integer;
    NewItem: TradParameterConnection;
begin
    BeginUpdate;
    try
        Clear;
        if Assigned(Method) then begin
            for i := 0 to Method.ArgCount - 1 do begin
                NewItem := TradParameterConnection(Add);
                NewItem.ParameterName := Method.Args[i].Name;
            end;
        end;
    finally
        EndUpdate;
    end;
end;

//------------------------------------------------------------------------------
function TradParameterConnections.GetOwner: TPersistent;
begin
    Result := FConnector;
end;

//------------------------------------------------------------------------------
procedure TradParameterConnections.SetLink(Index: Integer; Value: TradParameterConnection);
begin
    Items[Index].Assign(Value);
end;

//------------------------------------------------------------------------------
function TradParameterConnections.GetLink(Index: Integer): TradParameterConnection;
begin
    Result := TradParameterConnection(inherited Items[Index]);
end;

//------------------------------------------------------------------------------
// TradE2MConnection implementation
//------------------------------------------------------------------------------
constructor TradE2MConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTargetMethodParameters := TradParameterConnections.Create(Self);
    FSourceListener := TradListener.Create;
    FSourceListener.OnFiring := OnSourceEvent;
end;

//------------------------------------------------------------------------------
destructor TradE2MConnection.Destroy;
begin
    FTargetMethodParameters.Free;
    FTargetMethodParameters := nil;
    FSourceListener.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
function TradE2MConnection.GetSourceEventInfo: TradEventInfo;
begin
    { TODO -oBasil Tunegov -cSuggestion : Сделать кеширование }
    if Assigned(SourceInfo) then
        Result := SourceInfo.FindEvent(FSourceEvent)
    else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradE2MConnection.GetTargetMethodInfo: TradMethodInfo;
begin
    if Assigned(TargetInfo) then
        Result := TargetInfo.FindMethod(FTargetMethod)
    else
        Result := nil;
end;

//------------------------------------------------------------------------------
procedure TradE2MConnection.SetTargetMethod(Value: String);
begin
    FTargetMethod := Value;
    if Value <> FTargetMethodParameters.Method then begin
        FTargetMethodParameters.Method := Value;
        FTargetMethodParameters.Init(TargetMethodInfo);
    end;
end;

//------------------------------------------------------------------------------
procedure TradE2MConnection.Notification(AComponent: TComponent; Operation: TOperation);
var
    i: Integer;
begin
    inherited Notification(AComponent, Operation);
    if (Operation = opRemove) and Assigned(FTargetMethodParameters) then begin
        for i := 0 to FTargetMethodParameters.Count - 1 do begin
            if AComponent = FTargetMethodParameters.Items[i].Component then begin
                FTargetMethodParameters.Items[i].FComponent := nil;
                FTargetMethodParameters.Items[i].ComponentProperty := '';
                { TODO -oBasil Tunegov -cBugBug : Закоментировано, т.к. возникает Access Violation при удалении соединения на design time. }
                //Unconnect;
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
function TradE2MConnection.Connect: Boolean;
begin
    Result := False;

    if not Assigned(SourceInfo) or not Assigned(TargetInfo) or
       not Assigned(SourceEventInfo) or not Assigned(TargetMethodInfo) then {or
       ((FValueOptions = voComponentProperty) and (not Assigned(ValueComponentInfo) or not Assigned(ValueComponentPropertyInfo))) or
       ((FValueOptions = voEventProperty) and not Assigned(ValueEventParamInfo)) then}
    begin
        Exit;
    end;

    if not (csDesigning in ComponentState) then begin
        FSourceListener.EventName := FSourceEvent;
        SourceInfo.AddListener(FSourceListener);
    end;
    
    Result := True;
end;

//------------------------------------------------------------------------------
procedure TradE2MConnection.Unconnect;
begin
    if Assigned(SourceInfo) then
        SourceInfo.RemoveListener(FSourceListener);

    if not Assigned(SourceInfo) then
        SourceEvent := '';
    if not Assigned(TargetInfo) then
        TargetMethod := '';
end;

//------------------------------------------------------------------------------
procedure TradE2MConnection.OnSourceEvent(Sender: TObject; Event: TradEventInfo);
var
    i: Integer;
    j: Integer;
begin
    for i := 0 to FTargetMethodParameters.Count - 1 do begin
        with FTargetMethodParameters.Items[i] do begin
            if varIsArray(FComponentPropertyIndex) then begin
                for j := 0 to varArrayHighBound(FComponentPropertyIndex, 1) do
                    ComponentPropertyInfo.Indexes[j].Value := FComponentPropertyIndex[j];
            end;
            case Options of
                voConstant: ParameterInfo.Value := Constant;
                voComponentProperty: ParameterInfo.Value := ComponentInfo.GetProperty(ComponentPropertyInfo);
                voEventProperty: ParameterInfo.Value := EventParameterInfo.Value;
            else
                raise Exception.Create('Internal error in TradE2MConnection.OnSourceEvent');
            end;
        end;
    end;

    TargetInfo.InvokeMethod(TargetMethodInfo);

    for i := 0 to FTargetMethodParameters.Count - 1 do begin
        with FTargetMethodParameters.Items[i] do begin
            case Options of
                voConstant: ; // Do nothing
                voComponentProperty: ComponentInfo.SetProperty(ComponentPropertyInfo, ParameterInfo.Value);
                voEventProperty: EventParameterInfo.Value := ParameterInfo.Value;
            else
                raise Exception.Create('Internal error in TradE2MConnection.OnSourceEvent');
            end;
        end;
    end;
end;

end.
