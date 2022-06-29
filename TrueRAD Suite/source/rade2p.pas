
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit rade2p;

interface

uses Classes, radcommon, radconlist;

type
  TradE2PConnection = class(TradConnection)
  private
    FSourceEvent: String;
    FSourceEventInfo: TradEventInfo;
    FTargetProperty: String;
    FTargetPropertyInfo: TradPropertyInfo;
    FTargetPropertyIndex: Variant;
    FValueComponent: TComponent;
    FValueComponentInfo: TradClassInfo;
    FValueComponentProperty: String;
    FValueComponentPropertyInfo: TradPropertyInfo;
    FValueComponentPropertyIndex: Variant;
    FValueEventParameter: String;
    FValueConstant: String;
    FValueOptions: TradValueOptions;
    FSourceListener: TradListener;
    procedure SetTargetProperty(Value: String);
    procedure SetValueComponentProperty(Value: String);
    function GetSourceEventInfo: TradEventInfo;
    function GetTargetPropertyInfo: TradPropertyInfo;
    function GetValueComponentInfo: TradClassInfo;
    function GetValueComponentPropertyInfo: TradPropertyInfo;
    function GetValueEventParameterInfo: TradParameterInfo;
    procedure SetValueComponent(Value: TComponent);
    procedure OnSourceEvent(Sender: TObject; Event: TradEventInfo);
    procedure ReadTargetPropertyIndexes(Reader: TReader);
    procedure WriteTargetPropertyIndexes(Writer: TWriter);
    procedure ReadValueComponentPropertyIndexes(Reader: TReader);
    procedure WriteValueComponentPropertyIndexes(Writer: TWriter);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: Boolean; override;
    procedure Unconnect; override;
    property ValueComponentInfo: TradClassInfo read GetValueComponentInfo;
    property SourceEventInfo: TradEventInfo read GetSourceEventInfo;
    property TargetPropertyInfo: TradPropertyInfo read GetTargetPropertyInfo;
    property ValueComponentPropertyInfo: TradPropertyInfo read GetValueComponentPropertyInfo;
    property ValueEventParameterInfo: TradParameterInfo read GetValueEventParameterInfo;
  published
    property SourceEvent: String read FSourceEvent write FSourceEvent;
    property TargetProperty: String read FTargetProperty write SetTargetProperty;
    property TargetPropertyIndex: Variant read FTargetPropertyIndex write FTargetPropertyIndex stored False;
    property ValueComponent: TComponent read FValueComponent write SetValueComponent;
    property ValueComponentProperty: String read FValueComponentProperty write SetValueComponentProperty;
    property ValueComponentPropertyIndex: Variant read FValueComponentPropertyIndex write FValueComponentPropertyIndex stored False;
    property ValueEventParameter: String read FValueEventParameter write FValueEventParameter;
    property ValueConstant: String read FValueConstant write FValueConstant;
    property ValueOptions: TradValueOptions read FValueOptions write FValueOptions;
  end;

implementation

uses SysUtils;

//------------------------------------------------------------------------------
// TradE2PConnection implementation
//------------------------------------------------------------------------------
constructor TradE2PConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSourceListener := TradListener.Create;
    FSourceListener.OnFiring := OnSourceEvent;
end;

//------------------------------------------------------------------------------
destructor TradE2PConnection.Destroy;
begin
    FSourceListener.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.SetTargetProperty(Value: String);
begin
    FTargetProperty := Value;
    if not (csLoading in ComponentState) then begin
        if TargetPropertyInfo <> nil then
            TargetPropertyIndex := varArrayCreate([0, TargetPropertyInfo.IndexCount - 1], varVariant)
        else
            TargetPropertyIndex := varEmpty;
    end;
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.SetValueComponentProperty(Value: String);
begin
    FValueComponentProperty := Value;
    if not (csLoading in ComponentState) then begin
        if ValueComponentPropertyInfo <> nil then
            ValueComponentPropertyIndex := varArrayCreate([0, ValueComponentPropertyInfo.IndexCount - 1], varVariant)
        else
            ValueComponentPropertyIndex := varEmpty;
    end;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.GetValueComponentInfo: TradClassInfo;
begin
    if Assigned(FValueComponent) then begin
        if Assigned(FValueComponentInfo) and (FValueComponentInfo.Owner <> FValueComponent) then
            FValueComponentInfo := nil;
        if not Assigned(FValueComponentInfo) then
            FValueComponentInfo := TradClassRegistry.GetInfo(FValueComponent);
        Result := FValueComponentInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.GetSourceEventInfo: TradEventInfo;
begin
    if Assigned(SourceInfo) then begin
        if Assigned(FSourceEventInfo) and ((FSourceEventInfo.Source <> SourceInfo) or (FSourceEventInfo.Name <> FSourceEvent)) then
            FSourceEventInfo := nil;
        if not Assigned(FSourceEventInfo) then
            FSourceEventInfo := SourceInfo.FindEvent(FSourceEvent);
        Result := FSourceEventInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.GetTargetPropertyInfo: TradPropertyInfo;
begin
    if Assigned(TargetInfo) then begin
        if Assigned(FTargetPropertyInfo) and ((FTargetPropertyInfo.Source <> TargetInfo) or (FTargetPropertyInfo.Name <> FTargetProperty)) then
            FTargetPropertyInfo := nil;
        if not Assigned(FTargetPropertyInfo) then
            FTargetPropertyInfo := TargetInfo.FindProperty(FTargetProperty);
        Result := FTargetPropertyInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.GetValueComponentPropertyInfo: TradPropertyInfo;
begin
    if Assigned(ValueComponentInfo) then begin
        if Assigned(FValueComponentPropertyInfo) and ((FValueComponentPropertyInfo.Source <> ValueComponentInfo) or (FValueComponentPropertyInfo.Name <> FValueComponentProperty)) then
            FValueComponentPropertyInfo := nil;
        if not Assigned(FValueComponentPropertyInfo) then
            FValueComponentPropertyInfo := ValueComponentInfo.FindProperty(FValueComponentProperty);
        Result := FValueComponentPropertyInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.GetValueEventParameterInfo: TradParameterInfo;
begin
    if Assigned(SourceEventInfo) then
        Result := SourceEventInfo.Find(FValueEventParameter)
    else
        Result := nil;
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.SetValueComponent(Value: TComponent);
begin
    if Assigned(Value) and not Assigned(TradClassRegistry.FindClass(Value.ClassName)) then
        raise Exception.Create('This is a not TrueRAD component!');

    if Assigned(FValueComponent) then begin
        FValueComponent := nil;
        Unconnect;
    end;

    FValueComponent := Value;
    if Assigned(Value) then FValueComponent.FreeNotification(Self);
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FValueComponent then begin
            FValueComponent := nil;
            Unconnect;
        end;
    end;
end;

//------------------------------------------------------------------------------
function TradE2PConnection.Connect: Boolean;
begin
    Result := False;

    if not Assigned(SourceInfo) or not Assigned(TargetInfo) or
       not Assigned(SourceEventInfo) or not Assigned(TargetPropertyInfo) or
       ((FValueOptions = voComponentProperty) and (not Assigned(ValueComponentInfo) or not Assigned(ValueComponentPropertyInfo))) or
       ((FValueOptions = voEventProperty) and not Assigned(ValueEventParameterInfo)) then
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
procedure TradE2PConnection.Unconnect;
begin
    if Assigned(SourceInfo) and Assigned(SourceEventInfo) then
        SourceInfo.RemoveListener(FSourceListener);

    if SourceInfo = nil then begin
        SourceEvent := '';
        ValueEventParameter := '';
    end;

    if TargetInfo = nil then TargetProperty := '';
    if ValueComponentInfo = nil then ValueComponentProperty := '';
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.OnSourceEvent(Sender: TObject; Event: TradEventInfo);
var
    i: Integer;
begin
    if varIsArray(FTargetPropertyIndex) then begin
        for i := 0 to varArrayHighBound(FTargetPropertyIndex, 1) do
            TargetPropertyInfo.Indexes[i].Value := FTargetPropertyIndex[i];
    end;
    if varIsArray(FValueComponentPropertyIndex) then begin
        for i := 0 to varArrayHighBound(FValueComponentPropertyIndex, 1) do
            ValueComponentPropertyInfo.Indexes[i].Value := FValueComponentPropertyIndex[i];
    end;
    case FValueOptions of
        voConstant: TargetInfo.SetProperty(TargetPropertyInfo, ValueConstant);
        voComponentProperty: TargetInfo.SetProperty(TargetPropertyInfo, ValueComponentInfo.GetProperty(ValueComponentPropertyInfo));
        voEventProperty: TargetInfo.SetProperty(TargetPropertyInfo, ValueEventParameterInfo.Value);
    else
        raise Exception.Create('Internal error in TradE2PConnection.OnSourceEvent');
    end;
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineProperty('TargetPropertyIndexes', ReadTargetPropertyIndexes, WriteTargetPropertyIndexes, varIsArray(FTargetPropertyIndex));
    Filer.DefineProperty('ValueComponentPropertyIndexes', ReadValueComponentPropertyIndexes, WriteValueComponentPropertyIndexes, varIsArray(FValueComponentPropertyIndex));
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.ReadTargetPropertyIndexes(Reader: TReader);
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
    FTargetPropertyIndex := varArrayCreate([0, Length(Indexes) - 1], varVariant);
    for i := 0 to Length(Indexes) - 1 do
        FTargetPropertyIndex[i] := Indexes[i];
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.WriteTargetPropertyIndexes(Writer: TWriter);
var
    i: Integer;
begin
    Writer.WriteListBegin;
    for i := 0 to varArrayHighBound(FTargetPropertyIndex, 1) do
        Writer.WriteString(FTargetPropertyIndex[i]);
    Writer.WriteListEnd;
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.ReadValueComponentPropertyIndexes(Reader: TReader);
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
    FValueComponentPropertyIndex := varArrayCreate([0, Length(Indexes) - 1], varVariant);
    for i := 0 to Length(Indexes) - 1 do
        FValueComponentPropertyIndex[i] := Indexes[i];
end;

//------------------------------------------------------------------------------
procedure TradE2PConnection.WriteValueComponentPropertyIndexes(Writer: TWriter);
var
    i: Integer;
begin
    Writer.WriteListBegin;
    for i := 0 to varArrayHighBound(FValueComponentPropertyIndex, 1) do
        Writer.WriteString(FValueComponentPropertyIndex[i]);
    Writer.WriteListEnd;
end;

end.
