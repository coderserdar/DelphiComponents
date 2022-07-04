
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radp2p;

interface

uses Classes, radcommon, radconlist;

type
  TradP2PConnection = class(TradConnection)
  private
    FSourceProperty: String;
    FSourcePropertyInfo: TradPropertyInfo;
    FSourcePropertyIndex: Variant;
    FTargetProperty: String;
    FTargetPropertyInfo: TradPropertyInfo;
    FTargetPropertyIndex: Variant;
    FSourceEvent: String;
    FSourceEventInfo: TradEventInfo;
    FTargetEvent: String;
    FTargetEventInfo: TradEventInfo;
    FSourceListener: TradListener;
    FTargetListener: TradListener;
    FAligning: Boolean;
    procedure SetSourceProperty(Value: String);
    procedure SetTargetProperty(Value: String);
    function GetSourcePropertyInfo: TradPropertyInfo;
    function GetTargetPropertyInfo: TradPropertyInfo;
    function GetSourceEventInfo: TradEventInfo;
    function GetTargetEventInfo: TradEventInfo;
    procedure SetSourceValue;
    procedure SetTargetValue;
    procedure SourcePropertyChange(Sender: TObject; Event: TradEventInfo);
    procedure TargetPropertyChange(Sender: TObject; Event: TradEventInfo);
    procedure ReadSourcePropertyIndexes(Reader: TReader);
    procedure WriteSourcePropertyIndexes(Writer: TWriter);
    procedure ReadTargetPropertyIndexes(Reader: TReader);
    procedure WriteTargetPropertyIndexes(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: Boolean; override;
    procedure Unconnect; override;
    property SourcePropertyInfo: TradPropertyInfo read GetSourcePropertyInfo;
    property TargetPropertyInfo: TradPropertyInfo read GetTargetPropertyInfo;
    property SourceEventInfo: TradEventInfo read GetSourceEventInfo;
    property TargetEventInfo: TradEventInfo read GetTargetEventInfo;
  published
    property SourceProperty: String read FSourceProperty write SetSourceProperty;
    property SourcePropertyIndex: Variant read FSourcePropertyIndex write FSourcePropertyIndex stored False;
    property TargetProperty: String read FTargetProperty write SetTargetProperty;
    property TargetPropertyIndex: Variant read FTargetPropertyIndex write FTargetPropertyIndex stored False;
    property SourceEvent: String read FSourceEvent write FSourceEvent;
    property TargetEvent: String read FTargetEvent write FTargetEvent;
  end;

implementation

uses SysUtils;

//------------------------------------------------------------------------------
// TradP2PConnection implementation
//------------------------------------------------------------------------------
constructor TradP2PConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSourceListener := TradListener.Create;
    FSourceListener.OnFiring := SourcePropertyChange;
    FTargetListener := TradListener.Create;
    FTargetListener.OnFiring := TargetPropertyChange;
    FAligning := False;
end;

//------------------------------------------------------------------------------
destructor TradP2PConnection.Destroy;
begin
    FSourceListener.Free;
    FTargetListener.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.SetSourceProperty(Value: String);
begin
    FSourceProperty := Value;
    if not (csLoading in ComponentState) then begin
        if SourcePropertyInfo <> nil then
            SourcePropertyIndex := varArrayCreate([0, SourcePropertyInfo.IndexCount - 1], varVariant)
        else
            SourcePropertyIndex := varEmpty;
    end;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.SetTargetProperty(Value: String);
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
function TradP2PConnection.GetSourcePropertyInfo: TradPropertyInfo;
begin
    if Assigned(SourceInfo) then begin
        if Assigned(FSourcePropertyInfo) and ((FSourcePropertyInfo.Source <> SourceInfo) or (FSourcePropertyInfo.Name <> FSourceProperty)) then
            FSourcePropertyInfo := nil;
        if not Assigned(FSourcePropertyInfo) then
            FSourcePropertyInfo := SourceInfo.FindProperty(FSourceProperty);
        Result := FSourcePropertyInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradP2PConnection.GetTargetPropertyInfo: TradPropertyInfo;
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
function TradP2PConnection.GetSourceEventInfo: TradEventInfo;
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
function TradP2PConnection.GetTargetEventInfo: TradEventInfo;
begin
    if Assigned(TargetInfo) then begin
        if Assigned(FTargetEventInfo) and ((FTargetEventInfo.Source <> TargetInfo) or (FTargetEventInfo.Name <> FTargetEvent)) then
            FTargetEventInfo := nil;
        if not Assigned(FTargetEventInfo) then
            FTargetEventInfo := TargetInfo.FindEvent(FTargetEvent);
        Result := FTargetEventInfo;
    end else
        Result := nil;
end;

//------------------------------------------------------------------------------
function TradP2PConnection.Connect: Boolean;
begin
    Result := False;

    if not Assigned(SourceInfo) or not Assigned(TargetInfo) or
       not Assigned(SourcePropertyInfo) or not Assigned(TargetPropertyInfo) then
    begin
        Exit;
    end;

    if not (csDesigning in ComponentState) then begin
        FAligning := False;

        if Assigned(SourceEventInfo) then begin
            FSourceListener.EventName := FSourceEvent;
            SourceInfo.AddListener(FSourceListener);
        end;

        if Assigned(TargetEventInfo) then begin
            FTargetListener.EventName := FTargetEvent;
            TargetInfo.AddListener(FTargetListener);
        end;

        SetTargetValue;
    end;

    Result := True;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.Unconnect;
begin
    if Assigned(SourceInfo) then
        SourceInfo.RemoveListener(FSourceListener);

    if Assigned(TargetInfo) then
        TargetInfo.RemoveListener(FTargetListener);

    if SourceInfo = nil then begin
        SourceProperty := '';
        SourceEvent := '';
    end;

    if TargetInfo = nil then begin
        TargetProperty := '';
        TargetEvent := '';
    end;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.SetSourceValue;
var
    i: Integer;
begin
    try
        if FAligning = False then begin
            FAligning := True;
            if varIsArray(FSourcePropertyIndex) then begin
                for i := 0 to varArrayHighBound(FSourcePropertyIndex, 1) do
                    SourcePropertyInfo.Indexes[i].Value := FSourcePropertyIndex[i];
            end;
            if varIsArray(FTargetPropertyIndex) then begin
                for i := 0 to varArrayHighBound(FTargetPropertyIndex, 1) do
                    TargetPropertyInfo.Indexes[i].Value := FTargetPropertyIndex[i];
            end;
            SourceInfo.SetProperty(SourcePropertyInfo, TargetInfo.GetProperty(TargetPropertyInfo));
            FAligning := False;
        end;
    except
        on E: Exception do begin
            FAligning := False;
            raise Exception.CreateFmt('Exception "%s" occured in connection %s', [E.Message, Name]);
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.SetTargetValue;
var
    i: Integer;
begin
    try
        if FAligning = False then begin
            FAligning := True;
            if varIsArray(FSourcePropertyIndex) then begin
                for i := 0 to varArrayHighBound(FSourcePropertyIndex, 1) do
                    SourcePropertyInfo.Indexes[i].Value := FSourcePropertyIndex[i];
            end;
            if varIsArray(FTargetPropertyIndex) then begin
                for i := 0 to varArrayHighBound(FTargetPropertyIndex, 1) do
                    TargetPropertyInfo.Indexes[i].Value := FTargetPropertyIndex[i];
            end;
            TargetInfo.SetProperty(TargetPropertyInfo, SourceInfo.GetProperty(SourcePropertyInfo));
            FAligning := False;
        end;
    except
        on E: Exception do begin
            FAligning := False;
            raise Exception.CreateFmt('Exception "%s" occured in connection %s', [E.Message, Name]);
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.SourcePropertyChange(Sender: TObject; Event: TradEventInfo);
begin
    if csDesigning in ComponentState then Exit;
    if Sender = SourceInfo.Owner then SetTargetValue;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.TargetPropertyChange(Sender: TObject; Event: TradEventInfo);
begin
    if csDesigning in ComponentState then Exit;
    if Sender = TargetInfo.Owner then SetSourceValue;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineProperty('SourcePropertyIndexes', ReadSourcePropertyIndexes, WriteSourcePropertyIndexes, varIsArray(FSourcePropertyIndex));
    Filer.DefineProperty('TargetPropertyIndexes', ReadTargetPropertyIndexes, WriteTargetPropertyIndexes, varIsArray(FTargetPropertyIndex));
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.ReadSourcePropertyIndexes(Reader: TReader);
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
    FSourcePropertyIndex := varArrayCreate([0, Length(Indexes) - 1], varVariant);
    for i := 0 to Length(Indexes) - 1 do
        FSourcePropertyIndex[i] := Indexes[i];
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.WriteSourcePropertyIndexes(Writer: TWriter);
var
    i: Integer;
begin
    Writer.WriteListBegin;
    for i := 0 to varArrayHighBound(FSourcePropertyIndex, 1) do
        Writer.WriteString(FSourcePropertyIndex[i]);
    Writer.WriteListEnd;
end;

//------------------------------------------------------------------------------
procedure TradP2PConnection.ReadTargetPropertyIndexes(Reader: TReader);
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
procedure TradP2PConnection.WriteTargetPropertyIndexes(Writer: TWriter);
var
    i: Integer;
begin
    Writer.WriteListBegin;
    for i := 0 to varArrayHighBound(FTargetPropertyIndex, 1) do
        Writer.WriteString(FTargetPropertyIndex[i]);
    Writer.WriteListEnd;
end;

end.
