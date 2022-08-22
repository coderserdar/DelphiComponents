unit acDBComboEdit;
{$I sDefs.inc}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Mask, DBCtrls, DB, Windows, Messages,
  sCustomComboEdit;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBComboEdit = class(TsCustomComboEdit)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    function GetField: TField;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    function GetReadOnly: Boolean; reintroduce;
    procedure SetReadOnly(const Value: Boolean); reintroduce;
  protected
    function CanEdit: boolean;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopupWindowShow; override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;
  published
    property CheckOnExit;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;


implementation


function TsDBComboEdit.CanEdit: boolean;
begin
  Result := Assigned(FDataLink) and Assigned(FDataLink.DataSource) and
              not ReadOnly and
                not (csDesigning in ComponentState) and
                  FDataLink.Active and
                    FDataLink.CanModify and
                      (FDataLink.Editing or FDataLink.DataSource.AutoEdit)
end;


procedure TsDBComboEdit.Change;
begin
  if Assigned(FDataLink) then
    FDataLink.Modified;

  SkinData.BGChanged := True;
  inherited;
end;


procedure TsDBComboEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then
      SetFocus;

    raise;
  end;
  CheckCursor;
  DoExit;
end;


procedure TsDBComboEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;


constructor TsDBComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;


procedure TsDBComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Text := FDataLink.Field.AsString
  else
    if csDesigning in ComponentState then begin
      EditMask := '';
      EditText := Name;
    end
    else
      Text := '';
end;


destructor TsDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


function TsDBComboEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;


procedure TsDBComboEdit.EditingChange(Sender: TObject);
begin
end;


function TsDBComboEdit.GetDataField: string;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.FieldName
  else
    Result := '';
end;


function TsDBComboEdit.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBComboEdit.GetField: TField;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.Field
  else
    Result := nil;
end;


function TsDBComboEdit.GetReadOnly: Boolean;
begin
  if Assigned(FDataLink) then
    Result := FDataLink.ReadOnly
  else
    Result := True;
end;


procedure TsDBComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not CanEdit then
    Key := 0
  else
    if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) or (GetKeyState(VK_Up) and 128 = 128) or (GetKeyState(VK_Down) and 128 = 128) then
      FDataLink.Edit;
end;


procedure TsDBComboEdit.KeyPress(var Key: Char);
begin
  inherited;
  if CanEdit then
    case Key of
      #27: begin
        Reset;
        Key := #0;
      end
      else
        FDataLink.Edit;
    end
  else
    Key := #0;
end;


procedure TsDBComboEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;


procedure TsDBComboEdit.PopupWindowShow;
begin
  if CanEdit then
    inherited;
end;


procedure TsDBComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;


procedure TsDBComboEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;


procedure TsDBComboEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


procedure TsDBComboEdit.SetReadOnly(const Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;


procedure TsDBComboEdit.UpdateData(Sender: TObject);
begin
  if Text <> '' then
    FDataLink.Field.AsString := Text
  else
    FDataLink.Field.Clear;
end;


procedure TsDBComboEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;


procedure TsDBComboEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

end.
