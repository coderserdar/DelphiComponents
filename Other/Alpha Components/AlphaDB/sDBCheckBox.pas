unit sDBCheckBox;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DBCtrls, db, stdctrls, dbconsts,
  sCheckbox, sConst;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBCheckBox = class(TsCheckBox)
  private
    FValueCheck,
    FValueUncheck: string;
    FOnChange: TNotifyEvent;
    FDataLink: TFieldDataLink;
    FNullValue: TCheckBoxState;
    FPaintControl: TPaintControl;
    function GetField: TField;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetFieldState: TCheckBoxState;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetValueCheck(const Value: string);
    procedure SetValueUncheck(const Value: string);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function ValueMatch(const ValueList, Value: string): Boolean;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Toggle; override;
    function ActState: TCheckBoxState; override;
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

    property State;
    property Checked;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property NullValue: TCheckBoxState read FNullValue write FNullValue default cbGrayed;
    property ValueChecked: string read FValueCheck write SetValueCheck;
    property ValueUnchecked: string read FValueUncheck write SetValueUncheck;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses acntUtils, sGraphUtils, sCommonData;


function TsDBCheckBox.ActState: TCheckBoxState;
begin
  Result := GetFieldState;
end;


procedure TsDBCheckBox.CMExit(var Message: TCMExit);
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


procedure TsDBCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;


constructor TsDBCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  State := cbUnchecked;
  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;
  FNullValue := cbGrayed;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FPaintControl := TPaintControl.Create(Self, 'BUTTON');
  FPaintControl.Ctl3DButton := True;
  ControlStyle := ControlStyle + [csReplicatable];
end;


procedure TsDBCheckBox.DataChange(Sender: TObject);
var
  NewState: TCheckBoxState;
begin
  NewState := GetFieldState;
  if NewState <> State then begin
    State := NewState;
    if Assigned(OnValueChanged) then
      OnValueChanged(self);
  end;
end;


destructor TsDBCheckBox.Destroy;
begin
  FreeAndNil(FPaintControl);
  FreeAndNil(FDataLink);
  inherited Destroy;
end;


function TsDBCheckBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;


function TsDBCheckBox.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


function TsDBCheckBox.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource else Result := nil;
end;


function TsDBCheckBox.GetField: TField;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.Field
  else
    Result := nil;
end;


function TsDBCheckBox.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
    if FDataLink.Field.IsNull then
      Result := NullValue
    else
      if FDataLink.Field.DataType = ftBoolean then
        Result := CheckBoxStates[integer(FDataLink.Field.AsBoolean)]
      else begin
        Result := cbGrayed;
        Text := FDataLink.Field.Text;
        if ValueMatch(FValueCheck, Text) then
          Result := cbChecked
        else
          if ValueMatch(FValueUncheck, Text) then
            Result := cbUnchecked;
      end
  else
    Result := cbUnchecked;
end;


function TsDBCheckBox.GetReadOnly: Boolean;
begin
  if Assigned(FDataLink) then
    Result := not FDataLink.CanModify
  else
    Result := False;
end;


procedure TsDBCheckBox.KeyPress(var Key: Char);
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


procedure TsDBCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TsDBCheckBox.SetDataField(const Value: string);
begin
  if Assigned(FDataLink) then
    FDataLink.FieldName := Value;
end;


procedure TsDBCheckBox.SetDataSource(Value: TDataSource);
begin
  if Assigned(FDataLink) then begin
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
      FDataLink.DataSource := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;


procedure TsDBCheckBox.SetValueCheck(const Value: string);
begin
  FValueCheck := Value;
  DataChange(Self);
end;


procedure TsDBCheckBox.SetValueUncheck(const Value: string);
begin
  FValueUncheck := Value;
  DataChange(Self);
end;


procedure TsDBCheckBox.Toggle;
begin
  if Assigned(FDataLink) then
    if FDataLink.Edit then begin
      StopTimer(SkinData);
      inherited Toggle;
      UpdateData(Self);
      FDataLink.Modified;
      SkinData.Invalidate;
    end;
end;


function TsDBCheckBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;


procedure TsDBCheckBox.UpdateData(Sender: TObject);
var
  Pos: Integer;
  S: string;
begin
  if Assigned(FDataLink) then
    if State = cbGrayed then
      FDataLink.Field.Clear
    else
      if FDataLink.Field.DataType = ftBoolean then
        FDataLink.Field.AsBoolean := Checked
      else begin
        S := iff(Checked, FValueCheck, FValueUncheck);
        Pos := 1;
        FDataLink.Field.Text := ExtractFieldName(S, Pos);
      end;
end;


function TsDBCheckBox.UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;


function TsDBCheckBox.ValueMatch(const ValueList, Value: string): Boolean;
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


procedure TsDBCheckBox.WMPaint(var Message: TWMPaint);
begin
  if not (csPaintCopy in ControlState) or SkinData.Skinned then
    inherited
  else begin
    SendMessage(FPaintControl.Handle, BM_SETCHECK, Ord(GetFieldState), 0);
    SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
  end;
end;


procedure TsDBCheckBox.WndProc(var Message: TMessage);
begin
  with Message do
    case Msg of
      WM_CREATE, WM_WINDOWPOSCHANGED, CM_FONTCHANGED, CM_TEXTCHANGED:
        FPaintControl.DestroyHandle;

      CM_CHANGED: begin
        inherited;
        if not (csDesigning in COmponentState) and Assigned(FOnChange) then
          FOnChange(Self);

        Exit;
      end;

      BM_GETCHECK:
        if (csPaintCopy in ControlState) {and TStyleManager.IsCustomStyleActive and (seClient in StyleElements)} then begin
          Message.Result :=  Ord(GetFieldState);
          Exit;
        end;
    end;

  if (Message.Msg = WM_PRINT) and (csPaintCopy in ControlState) and not (csDesigning in ComponentState) then
    if SkinData.Skinned then begin
      SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_PRINTING;
      PaintState := Ord(GetFieldState);
      PaintHandler(TWMPaint(Message));
      PaintState := -1;
      SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_PRINTING;
    end
    else begin
      SendMessage(FPaintControl.Handle, BM_SETCHECK, Ord(GetFieldState), 0);
      SendMessage(FPaintControl.Handle, WM_PAINT, ACNativeInt(TWMPaint(Message).DC), 0);
    end
  else
    inherited;
end;

end.

