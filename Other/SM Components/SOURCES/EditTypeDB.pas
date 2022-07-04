{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  Dataware TEditTyped component
}

unit EditTypeDB;

interface

{$I SMVersion.inc}

uses
  Windows, Messages, Classes, Controls, DBCtrls, DB, EditType
  {$IFDEF SMForDelphi6} , MaskUtils {$ENDIF}
  ;

type
  TDBEditTyped = class(TEditTyped)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FCanvas: TControlCanvas;
    FFocused: Boolean;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    { Protected declarations }
    procedure Change; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; 
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Button;
    property Field: TField read GetField;
  published
    { Published declarations }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

procedure Register;

implementation

uses SysUtils, Mask;

procedure Register;
begin
  RegisterComponents('SMComponents', [TDBEditTyped]);
end;

procedure ResetMaxLength(DBEdit: TDBEditTyped);
var
  fld: TField;
begin
  with DBEdit do
    if (MaxLength > 0) and
       Assigned(DataSource) and
       Assigned(DataSource.DataSet) then
    begin
      fld := DataSource.DataSet.FindField(DataField);
      if Assigned(fld) and
         (fld.DataType = ftString) and
         (fld.Size = MaxLength) then
        MaxLength := 0;
    end;
end;

{ TDBEditTyped }
constructor TDBEditTyped.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable];
  inherited ReadOnly := True;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TDBEditTyped.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TDBEditTyped.Loaded;
begin
  inherited Loaded;
  ResetMaxLength(Self);
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TDBEditTyped.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBEditTyped.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TDBEditTyped.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Key {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#32..#255]) and
     (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TDBEditTyped.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TDBEditTyped.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TDBEditTyped.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TDBEditTyped.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TDBEditTyped.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBEditTyped.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBEditTyped.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBEditTyped.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then ResetMaxLength(Self);
  FDataLink.FieldName := Value;
end;

function TDBEditTyped.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBEditTyped.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDBEditTyped.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBEditTyped.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      EditText := '';  {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    if (FDataLink.Field.EditMask <> '') then
      EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType = ftString) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      if (EditMask = '') then
        EditText := FDataLink.Field.DisplayText
      else
        EditText := FormatMaskText(EditMask, FDataLink.Field.DisplayText);
      {if FDataLink.Editing then Modified := True;}
    end;
  end
  else
  begin
    Alignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TDBEditTyped.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TDBEditTyped.UpdateData(Sender: TObject);
begin
  ValidateEdit;
{  if (FDataLink.Field.EditMask <> '') then
    FDataLink.Field.Text := Text
  else
    FDataLink.Field.Text := EditText;
}    FDataLink.Field.Value := Value
end;

procedure TDBEditTyped.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBEditTyped.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBEditTyped.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
end;

procedure TDBEditTyped.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

procedure TDBEditTyped.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;

end.
