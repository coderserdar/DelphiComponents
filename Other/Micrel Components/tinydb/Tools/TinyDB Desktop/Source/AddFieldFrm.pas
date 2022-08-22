unit AddFieldFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Db, NewTableFrm,
  ExtCtrls, TinyDB, BaseFrm;

type
  TAddFieldFormData = record
    NewTableForm: TNewTableForm;
    FieldInfo: TFieldItemInfo;
  end;

  TAddFieldForm = class(TBaseForm)
    GroupBox1: TGroupBox;
    FieldNameLabel: TLabel;
    FieldTypeLabel: TLabel;
    FieldSizeLabel: TLabel;
    FieldNameEdit: TEdit;
    DataSizeEdit: TEdit;
    FieldTypeComboBox: TComboBox;
    OkButton: TButton;
    CloseButton: TButton;
    Panel1: TPanel;
    CommentsLabel: TLabel;
    DataProcessLabel: TLabel;
    DPModeComboBox: TComboBox;
    Bevel1: TBevel;
    procedure OkButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FieldTypeComboBoxClick(Sender: TObject);
  private
    { Private declarations }
    FData: TAddFieldFormData;
    FModifyMode: Boolean;

    procedure InitFieldComboBox;
    function CheckValid: Boolean;
    function GetDataSize(FieldType: TFieldType): Integer;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TAddFieldFormData);
    procedure GetData(var Value: TAddFieldFormData);
    property ModifyMode: Boolean read FModifyMode write FModifyMode;
  end;

var
  AddFieldForm: TAddFieldForm;

function ShowAddFieldForm(var Value: TAddFieldFormData; ModifyMode: Boolean = False): Boolean;

implementation

uses MainFrm, Misc, LangMgr;

{$R *.DFM}

function ShowAddFieldForm(var Value: TAddFieldFormData; ModifyMode: Boolean): Boolean;
var
  Frm: TAddFieldForm;
begin
  Frm := TAddFieldForm.Create(Application);
  Frm.ModifyMode := ModifyMode;
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  if Result and ModifyMode then Frm.GetData(Value);
  Frm.Free;
end;

procedure TAddFieldForm.TransLanguage; 
begin
  inherited;
  DPModeComboBox.Items[0] := AppLangMgr.Trans('fdDefault');
  DPModeComboBox.Items[1] := AppLangMgr.Trans('fdOriginal');
  DPModeComboBox.BoundsRect := Rect(DataProcessLabel.Left + DataProcessLabel.Width + 4,
    DPModeComboBox.Top, DPModeComboBox.BoundsRect.Right, DPModeComboBox.BoundsRect.Bottom);
  DPModeComboBox.ItemIndex := 0;
end;

procedure TAddFieldForm.SetData(Value: TAddFieldFormData);
begin
  FData := Value;
  if FModifyMode then
  begin
    Caption := AppLangMgr.Trans('Modify Field');
    OkButton.Caption := AppLangMgr.Trans('OK');
    CloseButton.Caption := AppLangMgr.Trans('Cancel');
    FieldNameEdit.Text := Value.FieldInfo.FieldName;
    FieldTypeComboBox.ItemIndex := MainForm.FieldList.IndexOfObject(TObject(Value.FieldInfo.FieldType));
    FieldTypeComboBoxClick(nil);
    DataSizeEdit.Text := IntToStr(Value.FieldInfo.DataSize);
    DPModeComboBox.ItemIndex := Integer(Value.FieldInfo.DPMode);
  end;
end;

procedure TAddFieldForm.GetData(var Value: TAddFieldFormData);
begin
  Value.FieldInfo.Idx := FData.FieldInfo.Idx;
  Value.FieldInfo.FieldName := FieldNameEdit.Text;
  Value.FieldInfo.FieldType := TFieldType(MainForm.FieldList.Objects[FieldTypeComboBox.ItemIndex]);
  Value.FieldInfo.DataSize := StrToInt(DataSizeEdit.Text);
  Value.FieldInfo.DPMode := TFieldDataProcessMode(DPModeComboBox.ItemIndex);
  Value.FieldInfo.Comments := MainForm.GetFieldCommentsByType(Value.FieldInfo.FieldType);
end;

procedure TAddFieldForm.InitFieldComboBox;
var
  I: Integer;
begin
  FieldTypeComboBox.Items.Clear;
  for I := 0 to MainForm.FieldList.Count - 1 do
    FieldTypeComboBox.Items.Add(MainForm.FieldList.Names[I]);
end;

function TAddFieldForm.CheckValid: Boolean;
begin
  Result := True;
  if not IsValidDBName(FieldNameEdit.Text) then
  begin
    if FieldNameEdit.Text <> '' then
      Application.MessageBox(PChar(AppLangMgr.Trans('Invalid identifier.')), PChar(Application.Title), 48);
    FieldNameEdit.SetFocus;
    Result := False;
    Exit;
  end;
  if FieldTypeComboBox.ItemIndex = -1 then
  begin
    FieldTypeComboBox.SetFocus;
    Result := False;
    Exit;
  end;
  if DataSizeEdit.Enabled then
  begin
    if not IsInt(DataSizeEdit.Text) then
    begin
      if DataSizeEdit.Text = '' then
        Application.MessageBox(PChar(AppLangMgr.Trans('Please input field size.')), PChar(Application.Title), 48)
      else
        Application.MessageBox(PChar(AppLangMgr.Trans('Invalid number.')), PChar(Application.Title), 48);
      DataSizeEdit.SetFocus;
      Result := False;
      Exit;
    end;
  end;
end;

function TAddFieldForm.GetDataSize(FieldType: TFieldType): Integer;
begin
  case FieldType of
    ftBoolean: Result := SizeOf(WordBool);
    ftDateTime,
    ftCurrency,
    ftFloat: Result := SizeOf(Double);
    ftTime,
    ftDate,
    ftAutoInc,
    ftInteger: Result := SizeOf(Integer);
    ftSmallint: Result := SizeOf(SmallInt);
    ftWord: Result := SizeOf(Word);
    ftLargeint: Result := SizeOf(Largeint);
  else
    Result := 0;
  end;
end;

procedure TAddFieldForm.FormCreate(Sender: TObject);
begin
  InitFieldComboBox;
  DPModeComboBox.ItemIndex := 0;
end;

procedure TAddFieldForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TAddFieldForm.OkButtonClick(Sender: TObject);
var
  Item: TFieldItemInfo;
begin
  if CheckValid then
  begin
    if FModifyMode then
    begin
      ModalResult := mrOk;
    end else
    begin
      GetData(FData);
      Item := FData.FieldInfo;
      FData.NewTableForm.AddFieldToListView(Item);
      FieldNameEdit.Text := '';
      FieldNameEdit.SetFocus;
    end;
  end;
end;

procedure TAddFieldForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAddFieldForm.FieldTypeComboBoxClick(Sender: TObject);
var
  I: Integer;
  FieldType: TFieldType;
begin
  I := FieldTypeComboBox.ItemIndex;
  if I = -1 then Exit;

  FieldType := TFieldType(MainForm.FieldList.Objects[I]);
  DataSizeEdit.Enabled := FieldType in [ftString, ftWideString, ftFixedChar];
  DataSizeEdit.Color := Iif(DataSizeEdit.Enabled, clWindow, $00DCDCDC);

  if DataSizeEdit.Enabled then
  begin
    if FieldType = ftString then DataSizeEdit.Text := '32'
    else DataSizeEdit.Text := ''
  end else
    DataSizeEdit.Text := IntToStr(GetDataSize(FieldType));

  CommentsLabel.Caption := MainForm.GetFieldCommentsByType(FieldType);
end;

end.
