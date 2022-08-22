unit acDBComboBoxEx;
{$I sDefs.inc}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DBCtrls, DB,
  sDBComboBox;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBComboBoxEx = class(TsDBComboBox)
  private
    FValues: TStrings;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure SetValues(const Value: TStrings);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Values: TStrings read FValues write SetValues;
  end;


implementation

type
  THackedDBComboBox = class(TCustomComboBox)
  public
    FDataLink: TFieldDataLink;
  end;


constructor TsDBComboBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  FValues := TStringList.Create;
  THackedDBComboBox(Self).FDataLink.OnDataChange := DataChange;
  THackedDBComboBox(Self).FDataLink.OnUpdateData := UpdateData;
end;


procedure TsDBComboBoxEx.DataChange(Sender: TObject);
var
  ind: Integer;
begin
  if (Style = csSimple) or not DroppedDown then
    if Field <> nil then begin
      ind := FValues.IndexOf(Field.Text);
      if ind < 0 then
        ind := Items.IndexOf(Field.Text);

      ItemIndex := ind;
    end
    else
      if csDesigning in ComponentState then
        Text := Name
      else
        ItemIndex := -1;
end;


destructor TsDBComboBoxEx.Destroy;
begin
  FValues.Free;
  inherited;
end;


procedure TsDBComboBoxEx.SetValues(const Value: TStrings);
begin
  if Assigned(FValues) then
    FValues.Assign(Value)
  else
    FValues := Value;
end;


procedure TsDBComboBoxEx.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then begin
    if ItemIndex >= 0 then
      if ItemIndex < FValues.Count then
        Field.Value := FValues[ItemIndex]
      else
        Field.Value := Items[ItemIndex]
  end
  else
    Field.Text := '';
end;

end.
