unit acDBSlider;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DBCtrls, db, stdctrls, dbconsts,
  acSlider, sConst;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBSlider = class(TsSlider)
  private
    FValueCheck,
    FValueUncheck: string;
    StateChanging: boolean;
    FOnChange: TNotifyEvent;
    FDataLink: TFieldDataLink;
    FNullValue: boolean;
    function GetField: TField;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetFieldState: boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function CanChange: boolean; override;
    procedure ValueChanged; override;
    function GetReadOnly: Boolean; reintroduce;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure WndProc(var Message: TMessage); override;
    function UseRightToLeftAlignment: Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;

    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property NullValue: boolean read FNullValue write FNullValue default False;
    property ValueChecked: string read FValueCheck write SetValueCheck;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses acntUtils, sGraphUtils;


function TsDBSlider.CanChange: boolean;
begin
  if not StateChanging then
    if inherited CanChange and Assigned(FDataLink) then
      Result := FDataLink.Edit
    else
      Result := False
  else
    Result := True;
end;


procedure TsDBSlider.CMExit(var Message: TCMExit);
begin
  try
    if Assigned(FDataLink) then
      FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;


procedure TsDBSlider.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;


constructor TsDBSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FNullValue := False;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;


procedure TsDBSlider.DataChange(Sender: TObject);
var
  NewState: boolean;
begin
  if not StateChanging then begin
    StateChanging := True;
    NewState := GetFieldState;
    if NewState <> SliderOn then
      SliderOn := NewState;

    StateChanging := False;
  end;
end;


destructor TsDBSlider.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;


function TsDBSlider.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;


function TsDBSlider.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


function TsDBSlider.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource else Result := nil;
end;


function TsDBSlider.GetField: TField;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.Field
  else
    Result := nil;
end;


function TsDBSlider.GetFieldState: boolean;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := FNullValue
    else
      if FDataLink.Field.DataType = ftBoolean then
        Result := FDataLink.Field.AsBoolean
      else begin
        Result := False;
        Text := FDataLink.Field.Text;
        if ValueMatch(FValueCheck, Text) then
          Result := True
      end
  else
    Result := False;
end;


function TsDBSlider.GetReadOnly: Boolean;
begin
  if Assigned(FDataLink) then
    Result := not FDataLink.CanModify
  else
    Result := False;
end;


procedure TsDBSlider.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Assigned(FDataLink) then
    case Key of
      #8, ' ':
        FDataLink.Edit;

      #27:
        FDataLink.Reset;
    end;
end;


procedure TsDBSlider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TsDBSlider.SetDataField(const Value: string);
begin
  if Assigned(FDataLink) then
    FDataLink.FieldName := Value;
end;


procedure TsDBSlider.SetDataSource(Value: TDataSource);
begin
  if Assigned(FDataLink) then begin
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
      FDataLink.DataSource := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;


procedure TsDBSlider.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;


procedure TsDBSlider.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;


procedure TsDBSlider.ValueChanged;
begin
  if not StateChanging then
    if Assigned(FDataLink) then
      if FDataLink.Edit then begin
        StateChanging := True;
        inherited ValueChanged;
        FDataLink.Modified;
        StateChanging := False;
      end;
end;


function TsDBSlider.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;


procedure TsDBSlider.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if Assigned(FDataLink) then
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := SliderOn
    else begin
      S := iff(SliderOn, FValueCheck, FValueUncheck);
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName(S, Pos);
    end;
end;


function TsDBSlider.UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;


function TsDBSlider.ValueMatch(const ValueList, Value: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length(ValueList) do
    if AnsiCompareText(ExtractFieldName(ValueList, Pos), Value) = 0 then begin
      Result := True;
      Break;
    end;
end;


procedure TsDBSlider.WndProc(var Message: TMessage);
var
  FSaveState: boolean;
begin
  with Message do
    case Msg of
      CM_CHANGED: begin
        inherited;
        if not (csDesigning in COmponentState) and Assigned(FOnChange) then
          FOnChange(Self);

        Exit;
      end
    end;

  if (Message.Msg = WM_PRINT) and (csPaintCopy in ControlState) and not (csDesigning in ComponentState) then begin
    SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_PRINTING;
    FSaveState := SliderOn;
    FSliderOn := GetFieldState;
    PaintHandler(TWMPaint(Message));
    FSliderOn := FSaveState;
    SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_PRINTING;
  end
  else
    inherited;
end;

end.

