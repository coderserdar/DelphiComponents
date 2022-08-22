unit acDBListBoxEx;
{$I sDefs.inc}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB, StdCtrls, DBCtrls,
  sDBListBox;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBListBoxEx = class(TsDBListBox)
  private
    FValues: TStrings;
    procedure SetValues(const Value: TStrings);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
  protected
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Values: TStrings read FValues write SetValues;
  end;


implementation

type
  THackedDBListBox = class(TCustomListBox)
  public
    FDataLink: TFieldDataLink;
  end;


constructor TsDBListBoxEx.Create(AOwner: TComponent);
begin
  inherited;
  FValues := TStringList.Create;
  THackedDBListBox(Self).FDataLink.OnDataChange := DataChange;
  THackedDBListBox(Self).FDataLink.OnUpdateData := UpdateData;
end;


procedure TsDBListBoxEx.DataChange(Sender: TObject);
var
  ind: Integer;
begin
  if Field <> nil then begin
    ind := FValues.IndexOf(Field.Text);
    if ind < 0 then
      ind := Items.IndexOf(Field.Text);

    ItemIndex := ind;
  end
  else
    ItemIndex := -1;
end;


destructor TsDBListBoxEx.Destroy;
begin
  FValues.Free;
  inherited;
end;


procedure TsDBListBoxEx.SetValues(const Value: TStrings);
begin
  if Assigned(FValues) then
    FValues.Assign(Value)
  else
    FValues := Value;
end;


procedure TsDBListBoxEx.UpdateData(Sender: TObject);
begin
  if Assigned(Field) then begin
    if ItemIndex >= 0 then
      if ItemIndex < FValues.Count then
        Field.Value := FValues[ItemIndex]
      else
        Field.Value := Items[ItemIndex];
  end
  else
    Field.Text := '';
end;

end.
