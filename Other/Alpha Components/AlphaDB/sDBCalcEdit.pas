unit sDBCalcEdit;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, DBCtrls, DB,
  sMaskEdit, sCustomComboEdit, sCurrEdit, sCalcUnit;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBCalcEdit = class(TsCalcEdit)
  private
    FDataLink: TFieldDataLink;
    DataChanging,
    FNullIfZero: boolean;
    procedure CalcWindowClose(Sender: TObject);

    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);

    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
  protected
    // FTextChangedByUser: boolean;
    function CanEdit: boolean;
    function CanCheckValue: boolean;
    function GetValue: Extended; override;
    // procedure SetValue(AValue: Extended); override;
    function EditCanModify: boolean; override;
    function GetDisplayText: string; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CheckValue(NewValue: Extended): Extended; override;
    procedure PopupWindowShow; override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  public
    procedure Change; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetReadOnly: boolean; override;
    procedure SetReadOnly(Value: boolean); override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property DirectInput;
    property DisplayFormat;
    property NullIfZero: boolean read FNullIfZero write FNullIfZero default False;
  end;


implementation

uses
{$IFNDEF DELPHI5}MaskUtils, {$ENDIF}
  sGlyphUtils, acntUtils, sConst;


constructor TsDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // FTextChangedByUser := False;
  FDataLink := TFieldDataLink.Create;
  FDefBmpID := iBTN_CALC;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FNullIfZero := False;
end;


procedure TsDBCalcEdit.DataChange(Sender: TObject);
var
  NewValue: Extended;
begin
  if not (csDestroying in ComponentState) then
    if not DataChanging then begin
      DataChanging := True;
      if (FDataLink <> nil) and (FDataLink.Field <> nil) then begin
        NewValue := FDataLink.Field.AsFloat;
{
        if FDataLink.DataSet.State = dsInsert then begin
          Modified := True;
          NewValue := CheckValue(NewValue);
        end;
}
      end
      else
        NewValue := 0;

      if (FValue <> NewValue) or Modified then begin
        FValue := NewValue;
//        Text := FormatDisplayText(FValue);
        if (FDataLink <> nil) and (FDataLink.Field <> nil) then
          Text := FDataLink.Field.asString
        else
          Text := ZeroChar;

        SkinData.Invalidate;
      end;
      Modified := False;
      // FTextChangedByUser := False;
      DataChanging := False;
    end;
end;


procedure TsDBCalcEdit.EditingChange(Sender: TObject);
begin
  //
end;


procedure TsDBCalcEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  if (Value <> 0) or not FNullIfZero then begin
    FDataLink.Edit;
    FDataLink.Field.AsFloat := Value;
    Text := FDataLink.Field.asString;
  end
  else
    FDataLink.Field.Clear;
{
  WasModified := Modified;
  ValidateEdit;

  if WasModified or (FDataLink <> nil) and (FDataLink.DataSource <> nil) and (dsInsert = FDataLink.DataSource.State) then begin
    s := Text;
    if pos(CharMinus, s) = 1 then begin
      Delete(s, 1, 1);
      Minus := -1
    end
    else
      Minus := 1;

    if dsInsert = FDataLink.DataSource.State then begin
      FValue := CheckValue(StrToFloat(TextToValText(ZeroChar + s)) * Minus);
      if FValue <> FDataLink.Field.AsFloat then // Update data in the new record in DB if changed after checking
        FDataLink.Field.AsFloat := FValue;
    end;
  end;

  V := Value;
  if (V <> 0) or not FNullIfZero then begin
    FDataLink.Edit;
    FDataLink.Field.AsFloat := V;
    Text := FDataLink.Field.asString;
  end
  else
    FDataLink.Field.Clear;
}
end;


procedure TsDBCalcEdit.WMPaste(var Message: TMessage);
begin
  inherited;
  Modified := True;
  // FTextChangedByUser := True;
end;


function TsDBCalcEdit.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


procedure TsDBCalcEdit.SetDataSource(const Value: TDataSource);
begin
  if not(FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


function TsDBCalcEdit.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


procedure TsDBCalcEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;


procedure TsDBCalcEdit.KeyPress(var Key: Char);
begin
  if CanEdit then begin
    case Key of
      #27: begin // Esc
        DataChange(Self);
        DataChanged;
        SelectAll;
        Modified := False;
        // FTextChangedByUser := False;
        Key := #0;
        Exit;
      end;
    end;
  end;
  inherited;
  if CanEdit then begin
    if CharInSet(Key, [#32 .. #255]) and (FDataLink.Field <> nil) and not CharInSet(Key, ['0' .. '9']) and (Key <> {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator) and (Key <> '-') then begin
      MessageBeep(16);
      Key := #0;
    end;
    if Key = {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator then
      FDataLink.Edit;

    case Key of
      ^H, ^V, ^X, '0' .. '9', '-': begin
        FDataLink.Edit;
        Modified := True;
        // FTextChangedByUser := True;
      end;
    end;
  end
  else
    Key := #0;
end;


procedure TsDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not CanEdit then
    Key := 0
  else
    if not readonly and ((Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift))) then
      FDataLink.Edit;
end;


procedure TsDBCalcEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


function TsDBCalcEdit.EditCanModify: boolean;
begin
  Result := FDataLink.Edit;
end;


function TsDBCalcEdit.CanCheckValue: boolean;
begin
  Result := CheckOnExit and (Modified { FTextChangedByUser } or (FDataLink <> nil) and (FDataLink.DataSource <> nil) and (dsInsert = FDataLink.DataSource.State));
end;


function TsDBCalcEdit.CanEdit: boolean;
begin
  Result := Assigned(FDataLink) and Assigned(FDataLink.DataSource) and not readonly and not(csDesigning in ComponentState) and FDataLink.Active and FDataLink.CanModify and
    (FDataLink.Editing or FDataLink.DataSource.AutoEdit)
end;


procedure TsDBCalcEdit.Change;
begin
  if not DataChanging then
    if Assigned(FDataLink) and not Formatting then
      FDataLink.Modified;

  inherited;
end;


function TsDBCalcEdit.CheckValue(NewValue: Extended): Extended;
begin
  if CanCheckValue then
    Result := inherited CheckValue(NewValue)
  else
    Result := NewValue;
end;


procedure TsDBCalcEdit.PopupWindowShow;
begin
  if CanEdit then begin
    inherited;
    TsCalcForm(FPopupWindow).OnResultClick := CalcWindowClose;
  end;
end;


procedure TsDBCalcEdit.CalcWindowClose(Sender: TObject);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.CanModify then begin
    EditCanModify;
    if FDataLink.Field.AsString <> Text then begin // Value was changed in calc window
      Text := Text;
      UpdateData(Self);
    end;
  end;
end;


destructor TsDBCalcEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;


procedure TsDBCalcEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  try
    if CheckOnExit and (FDataLink <> nil) and (FDataLink.DataSource <> nil) and (FDataLink.DataSource.State = dsInsert) then
      FDataLink.Modified;

    FDataLink.UpdateRecord;
  finally
  end;
end;


function TsDBCalcEdit.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;


function TsDBCalcEdit.GetValue: Extended;
begin
  Result := FValue;
end;


function TsDBCalcEdit.GetDisplayText: string;
begin
  if (FDataLink <> nil) and (FDataLink.Field <> nil) and (FDataLink.Field.IsNull and ((FDataLink = nil) or (FDataLink.DataSource = nil) or (dsInsert <> FDataLink.DataSource.State))) then
    Result := ''
  else
    if CanCheckValue then
      Result := Text
    else
      Result := FormatFloat(DisplayFormat, Value);
end;


procedure TsDBCalcEdit.SetReadOnly(Value: boolean);
begin
  FDataLink.ReadOnly := Value;
end;

end.
