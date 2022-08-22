unit acDBDecimalSpinEdit;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, DBCtrls, DB,
  sSpinEdit;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBDecimalSpinEdit = class(TsDecimalSpinEdit)
  private
    FDataLink: TFieldDataLink;
    FNullIfZero: boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function  GetDataField: string;
    function  GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure UpdateData(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
  public
    procedure Change; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property NullIfZero: boolean read FNullIfZero write FNullIfZero default False;
  end;


implementation




procedure TsDBDecimalSpinEdit.Change;
var
  rValue: Extended;
begin
  if Assigned(FDataLink) then begin
	  FDataLink.Modified;
    if not FDataLink.Editing then begin
      rValue := Value;
      FDataLink.Edit;
      Value := rValue;
  	  FDataLink.Modified;
    end;
  end;
  inherited Change;
end;


procedure TsDBDecimalSpinEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  finally
    inherited;
  end;
end;


procedure TsDBDecimalSpinEdit.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  if not ReadOnly then
    Change;
end;


constructor TsDBDecimalSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FNullIfZero := False;
end;


procedure TsDBDecimalSpinEdit.DataChange(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    if (FDataLink <> nil) and (FDataLink.Field <> nil) then
      if DecimalPlaces = 0 then
        Value := FDataLink.Field.AsInteger
      else
        Value := FDataLink.Field.AsFloat
    else
      Value := 0;
end;


destructor TsDBDecimalSpinEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;


procedure TsDBDecimalSpinEdit.DownClick(Sender: TObject);
begin
  inherited;
  if not ReadOnly then
    Change;
end;


procedure TsDBDecimalSpinEdit.EditingChange(Sender: TObject);
begin
//
end;


function TsDBDecimalSpinEdit.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


function TsDBDecimalSpinEdit.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


procedure TsDBDecimalSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ReadOnly then
    Key := 0;
end;


procedure TsDBDecimalSpinEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if ReadOnly or not FDataLink.CanModify then
    Key := #0
  else
    case Key of
      ^H, ^V, ^X, '0'..'9', '-', '+':
        FDataLink.Edit;

      #27: begin // Esc
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
    end;
end;


procedure TsDBDecimalSpinEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TsDBDecimalSpinEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;


procedure TsDBDecimalSpinEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


procedure TsDBDecimalSpinEdit.UpClick(Sender: TObject);
begin
  inherited;
  if not ReadOnly then
    Change;
end;


procedure TsDBDecimalSpinEdit.UpdateData(Sender: TObject);
var
  V: Extended;
begin
  V := Value;
  if (V <> 0) or not FNullIfZero then
    if DecimalPlaces = 0 then
      FDataLink.Field.AsInteger := Trunc(V)
    else
      FDataLink.Field.AsFloat := V
  else
    FDataLink.Field.Clear;
end;

end.
