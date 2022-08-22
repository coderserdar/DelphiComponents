unit FilterFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, TinyDB, Db,
  BaseFrm;

type
  TFilterFormData = record
    TinyTable: TTinyTable;
  end;

  TFilterForm = class(TBaseForm)
    ConGroupBox: TGroupBox;
    FieldComboBox: TComboBox;
    ValueEdit: TEdit;
    AddButton: TButton;
    OprComboBox: TComboBox;
    ConMemo: TMemo;
    OkButton: TButton;
    CancelButton: TButton;
    GroupBox2: TGroupBox;
    CaseInsCheckBox: TCheckBox;
    NoPartCheckBox: TCheckBox;
    CancelFilterButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure CancelFilterButtonClick(Sender: TObject);
  private
    { Private declarations }
    FData: TFilterFormData;

    function GetTinyTable: TTinyTable;
    procedure Init;
    procedure FillFieldComboBox(ComboBox: TComboBox);
    procedure DoFilter;
    procedure OnFilterProgressEvent(Sender: TObject; Percent: Integer);
  public
    { Public declarations }
    procedure SetData(Value: TFilterFormData);
    property TinyTable: TTinyTable read GetTinyTable;
  end;

var
  FilterForm: TFilterForm;

function ShowFilterForm(Value: TFilterFormData): Boolean;

implementation

uses MainFrm;

{$R *.DFM}

function ShowFilterForm(Value: TFilterFormData): Boolean;
var
  Frm: TFilterForm;
begin
  Frm := TFilterForm.Create(Application);
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  Frm.Free;
end;

procedure TFilterForm.SetData(Value: TFilterFormData);
begin
  FData := Value;
  Init;
end;

function TFilterForm.GetTinyTable: TTinyTable;
begin
  Result := FData.TinyTable;
end;

procedure TFilterForm.Init;
begin
  FillFieldComboBox(FieldComboBox);
end;

procedure TFilterForm.FillFieldComboBox(ComboBox: TComboBox);
var
  I: Integer;
begin
  ComboBox.Items.Clear;
  for I := 0 to TinyTable.Fields.Count - 1 do
    ComboBox.Items.Add(TinyTable.Fields[I].FieldName);
  if ComboBox.Items.Count > 0 then
    ComboBox.ItemIndex := 0;
end;

procedure TFilterForm.DoFilter;
var
  FilterOptions: TFilterOptions;
begin
  FilterOptions := [];
  if CaseInsCheckBox.Checked then FilterOptions := FilterOptions + [foCaseInsensitive];
  if NoPartCheckBox.Checked then FilterOptions := FilterOptions + [foNoPartialCompare];
  TinyTable.FilterOptions := FilterOptions;
  TinyTable.Filter := ConMemo.Lines.Text;
  TinyTable.OnFilterProgress := OnFilterProgressEvent;
  Screen.Cursor := crHourGlass;
  MainForm.BeginProgress;
  try
    TinyTable.Filtered := True;
  finally
    TinyTable.OnFilterProgress := nil;
    MainForm.EndProgress;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFilterForm.OnFilterProgressEvent(Sender: TObject; Percent: Integer);
begin
  MainForm.DoProgress(Percent);
end;

procedure TFilterForm.OkButtonClick(Sender: TObject);
begin
  if (ConMemo.Lines.Text = '') and (ValueEdit.Text <> '') then
    AddButtonClick(nil);
  DoFilter;
  ModalResult := mrOk;
end;

procedure TFilterForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFilterForm.AddButtonClick(Sender: TObject);
var
  S: string;
  FieldType: TFieldType;
begin
  if FieldComboBox.ItemIndex = -1 then Exit;
  if OprComboBox.ItemIndex = -1 then Exit;

  FieldType := TinyTable.Fields[FieldComboBox.ItemIndex].DataType;
  S := FieldComboBox.Text + ' ' + OprComboBox.Text + ' ';
  if FieldType in [ftString, ftFixedChar, ftWideString, ftMemo, ftGraphic, ftBlob, ftFmtMemo, ftDate, ftTime, ftDateTime] then
    S := S + '''' + ValueEdit.Text + ''''
  else
    S := S + ValueEdit.Text;

  if ConMemo.Lines.Text = '' then
    ConMemo.Lines.Add(S)
  else
    ConMemo.Lines.Add('AND ' + S);

  ValueEdit.Text := '';
  ValueEdit.SetFocus;
end;

procedure TFilterForm.CancelFilterButtonClick(Sender: TObject);
begin
  TinyTable.Filtered := False;
  ModalResult := mrOk;
end;

end.
