unit AddIndexFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  NewTableFrm, TinyDB, BaseFrm;

type
  TAddIndexFormData = record
    NewTableForm: TNewTableForm;
    IndexInfo: TIndexItemInfo;
    AvailableFields: string;
  end;

  TAddIndexForm = class(TBaseForm)
    GroupBox1: TGroupBox;
    IndexNameLabel: TLabel;
    IndexFieldsLabel: TLabel;
    IndexNameEdit: TEdit;
    IndexFieldEdit: TEdit;
    OkButton: TButton;
    CloseButton: TButton;
    FieldsLabel: TLabel;
    FieldsListBox: TListBox;
    SortRadioGroup: TRadioGroup;
    GroupBox2: TGroupBox;
    PrimaryCheckBox: TCheckBox;
    UniqueCheckBox: TCheckBox;
    CaseCheckBox: TCheckBox;
    procedure OkButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FieldsListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FData: TAddIndexFormData;
    FModifyMode: Boolean;

    function CheckValid: Boolean;
    procedure ResetCtrls;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TAddIndexFormData);
    procedure GetData(var Value: TAddIndexFormData);
    property ModifyMode: Boolean read FModifyMode write FModifyMode;
  end;

var
  AddIndexForm: TAddIndexForm;

function ShowAddIndexForm(var Value: TAddIndexFormData; ModifyMode: Boolean = False): Boolean;

implementation

uses MainFrm, Misc, LangMgr;

{$R *.DFM}

function ShowAddIndexForm(var Value: TAddIndexFormData; ModifyMode: Boolean = False): Boolean;
var
  Frm: TAddIndexForm;
begin
  Frm := TAddIndexForm.Create(Application);
  Frm.ModifyMode := ModifyMode;
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  if Result and ModifyMode then Frm.GetData(Value);
  Frm.Free;
end;

procedure TAddIndexForm.TransLanguage;
begin
  inherited;
  SortRadioGroup.Items[0] := AppLangMgr.Trans('Ascending');
  SortRadioGroup.Items[1] := AppLangMgr.Trans('Descending');
end;

procedure TAddIndexForm.SetData(Value: TAddIndexFormData);
var
  List: TStrings;
  I: Integer;
begin
  FData := Value;
  List := TStringList.Create;
  List.CommaText := Value.AvailableFields;
  FieldsListBox.Items.Clear;
  for I := 0 to List.Count - 1 do
    FieldsListBox.Items.Add(List[I]);
  if FModifyMode then
  begin
    Caption := AppLangMgr.Trans('Modify Field');
    OkButton.Caption := AppLangMgr.Trans('OK');
    CloseButton.Caption := AppLangMgr.Trans('Cancel');
    IndexNameEdit.Text := Value.IndexInfo.IndexName;
    IndexFieldEdit.Text := Value.IndexInfo.IndexFields;
    PrimaryCheckBox.Checked := (tiPrimary in Value.IndexInfo.IndexOptions);
    UniqueCheckBox.Checked := (tiUnique in Value.IndexInfo.IndexOptions);
    CaseCheckBox.Checked := not (tiCaseInsensitive in Value.IndexInfo.IndexOptions);
    SortRadioGroup.ItemIndex := Ord(tiDescending in Value.IndexInfo.IndexOptions);
  end;
  List.Free;
end;

procedure TAddIndexForm.GetData(var Value: TAddIndexFormData);
begin
  Value.IndexInfo.IndexName := IndexNameEdit.Text;
  Value.IndexInfo.IndexFields := IndexFieldEdit.Text;
  Value.IndexInfo.IndexOptions := [];
  if PrimaryCheckBox.Checked then
    Value.IndexInfo.IndexOptions := Value.IndexInfo.IndexOptions + [tiPrimary];
  if UniqueCheckBox.Checked then
    Value.IndexInfo.IndexOptions := Value.IndexInfo.IndexOptions + [tiUnique];
  if not CaseCheckBox.Checked then
    Value.IndexInfo.IndexOptions := Value.IndexInfo.IndexOptions + [tiCaseInsensitive];
  if SortRadioGroup.ItemIndex = 1 then
    Value.IndexInfo.IndexOptions := Value.IndexInfo.IndexOptions + [tiDescending];
end;

function TAddIndexForm.CheckValid: Boolean;
var
  List: TStrings;
  I: Integer;
begin
  Result := True;
  if not IsValidDBName(IndexNameEdit.Text) then
  begin
    if IndexNameEdit.Text <> '' then
      Application.MessageBox(PChar(AppLangMgr.Trans('Invalid identifier.')), PChar(Application.Title), 48);
    IndexNameEdit.SetFocus;
    Result := False;
    Exit;
  end;
  List := TStringList.Create;
  List.CommaText := IndexFieldEdit.Text;
  try
    if List.Count = 0 then
    begin
      IndexFieldEdit.SetFocus;
      Result := False;
      Exit;
    end;
    for I := 0 to List.Count - 1 do
    begin
      if FieldsListBox.Items.IndexOf(List[I]) = -1 then
      begin
        Application.MessageBox(PChar(AppLangMgr.Trans('Invalid field name in index fields.')), PChar(Application.Title), 48);
        IndexFieldEdit.SetFocus;
        Result := False;
        Exit;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TAddIndexForm.ResetCtrls;
begin
  IndexNameEdit.Text := '';
  IndexFieldEdit.Text := '';
  PrimaryCheckBox.Checked := False;
  UniqueCheckBox.Checked := False;
  CaseCheckBox.Checked := True;
  SortRadioGroup.ItemIndex := 0;
end;

procedure TAddIndexForm.OkButtonClick(Sender: TObject);
var
  Item: TIndexItemInfo;
begin
  if CheckValid then
  begin
    if FModifyMode then
    begin
      ModalResult := mrOk;
    end
    else
    begin
      GetData(FData);
      Item := FData.IndexInfo;
      FData.NewTableForm.AddIndexToListView(Item);
      ResetCtrls;
      IndexNameEdit.SetFocus;
    end;
  end;
end;

procedure TAddIndexForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAddIndexForm.FieldsListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  List: TStrings;
begin
  I := FieldsListBox.ItemIndex;
  if I = -1 then Exit;

  List := TStringList.Create;
  List.CommaText := IndexFieldEdit.Text;
  if List.IndexOf(FieldsListBox.Items[I]) = -1 then
  begin
    if IndexFieldEdit.Text <> '' then
      IndexFieldEdit.Text := IndexFieldEdit.Text + ',';
    IndexFieldEdit.Text := IndexFieldEdit.Text + FieldsListBox.Items[I];
  end;
  List.Free;
end;

end.
