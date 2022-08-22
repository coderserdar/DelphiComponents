unit NewDBFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TinyDB, BaseFrm;

type
  TNewDBFormData = record
    FileName: string;
    Compress: Boolean;
    CompAlgo: string;
    CompLevel: Integer;
    Encrypt: Boolean;
    EncAlgo: string;
    Password: string;
  end;

  TNewDBForm = class(TBaseForm)
    OkButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    GroupBox1: TGroupBox;
    CompressCheckBox: TCheckBox;
    CompLevelComboBox: TComboBox;
    CompLevelLabel: TLabel;
    GroupBox2: TGroupBox;
    EncryptAlgoLabel: TLabel;
    EncryptCheckBox: TCheckBox;
    EncAlgoComboBox: TComboBox;
    PasswordLabel: TLabel;
    PasswordEdit: TEdit;
    Password2Label: TLabel;
    Password2Edit: TEdit;
    DatabaseNameLabel: TLabel;
    FileNameEdit: TEdit;
    BrowseButton: TButton;
    SaveDialog: TSaveDialog;
    CompAlgoLabel: TLabel;
    CompAlgoComboBox: TComboBox;
    procedure BrowseButtonClick(Sender: TObject);
    procedure CompressCheckBoxClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
    function CheckFileName: Boolean;
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure GetData(var Value: TNewDBFormData);
  end;

var
  NewDBForm: TNewDBForm;

function ShowNewDBForm(var Value: TNewDBFormData): Boolean;

implementation

uses Misc, LangMgr;

{$R *.DFM}

function ShowNewDBForm(var Value: TNewDBFormData): Boolean;
var
  Frm: TNewDBForm;
begin
  Frm := TNewDBForm.Create(Application);
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TNewDBForm.TransLanguage;
begin
  inherited;
  CompLevelComboBox.Items[0] := AppLangMgr.Trans('clMaximum');
  CompLevelComboBox.Items[1] := AppLangMgr.Trans('clNormal');
  CompLevelComboBox.Items[2] := AppLangMgr.Trans('clFast');
  CompLevelComboBox.Items[3] := AppLangMgr.Trans('clSuperFast');
end;

procedure TNewDBForm.FormCreate(Sender: TObject);
begin
  TTinyDatabase.GetCompressAlgoNames(CompAlgoComboBox.Items);
  TTinyDatabase.GetEncryptAlgoNames(EncAlgoComboBox.Items);
  CompressCheckBoxClick(nil);
  EncryptCheckBoxClick(nil);
end;

procedure TNewDBForm.GetData(var Value: TNewDBFormData);
begin
  with Value do
  begin
    FileName := FileNameEdit.Text;
    Compress := CompressCheckBox.Checked;
    CompAlgo := CompAlgoComboBox.Text;
    CompLevel := CompLevelComboBox.ItemIndex;
    if CompLevel < 0 then CompLevel := 0;
    Encrypt := EncryptCheckBox.Checked;
    EncAlgo := EncAlgoComboBox.Text;
    Password := PasswordEdit.Text;
  end;
end;

function TNewDBForm.CheckFileName: Boolean;
begin
  Result := True;
  if FileNameEdit.Text = '' then
  begin
    Result := False;
  end
  else
  begin
    if ExtractFilePath(FileNameEdit.Text) = '' then
    begin
      FileNameEdit.Text := ExtractFilePath(Application.ExeName) + FileNameEdit.Text;
      Result := False;
    end;
    if ExtractFileExt(FileNameEdit.Text) = '' then
    begin
      FileNameEdit.Text := FileNameEdit.Text + '.tdb';
      Result := False;
    end;
  end;
  if not Result then FileNameEdit.SetFocus;
end;

function TNewDBForm.CheckValid: Boolean;
begin
  Result := CheckFileName;
  if not Result then Exit;

  Result := True;
  if CompressCheckBox.Checked then
  begin
    if CompAlgoComboBox.ItemIndex = -1 then
    begin
      CompAlgoComboBox.SetFocus;
      Result := False;
      Exit;
    end;
    if CompLevelComboBox.ItemIndex = -1 then
    begin
      CompLevelComboBox.SetFocus;
      Result := False;
      Exit;
    end;
  end;
  if EncryptCheckBox.Checked then
  begin
    if EncAlgoComboBox.ItemIndex = -1 then
    begin
      EncAlgoComboBox.SetFocus;
      Result := False;
      Exit;
    end;
    if PasswordEdit.Text <> Password2Edit.Text then
    begin
      PasswordEdit.SetFocus;
      Application.MessageBox(PChar(AppLangMgr.Trans('The two passwords should be same.')), PChar(Application.Title), 48);
      Result := False;
      Exit;
    end;
  end;
end;

procedure TNewDBForm.BrowseButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    FileNameEdit.Text := SaveDialog.FileName;
  end;
end;

procedure TNewDBForm.CompressCheckBoxClick(Sender: TObject);
begin
  CompAlgoComboBox.Enabled := CompressCheckBox.Checked;
  CompAlgoComboBox.Color := Iif(CompAlgoComboBox.Enabled, clWindow, $00DCDCDC);
  CompLevelComboBox.Enabled := CompressCheckBox.Checked;
  CompLevelComboBox.Color := Iif(CompLevelComboBox.Enabled, clWindow, $00DCDCDC);
  if CompressCheckBox.Checked then
  begin
    if CompAlgoComboBox.ItemIndex = -1 then CompAlgoComboBox.ItemIndex := 0;
    if CompLevelComboBox.ItemIndex = -1 then CompLevelComboBox.ItemIndex := Integer(clNormal);
  end;
end;

procedure TNewDBForm.EncryptCheckBoxClick(Sender: TObject);
var
  Checked: Boolean;
begin
  Checked := EncryptCheckBox.Checked;
  EncAlgoComboBox.Enabled := Checked;
  EncAlgoComboBox.Color := Iif(Checked, clWindow, $00DCDCDC);
  PasswordEdit.Enabled := Checked;
  PasswordEdit.Color := Iif(Checked, clWindow, $00DCDCDC);
  Password2Edit.Enabled := Checked;
  Password2Edit.Color := Iif(Checked, clWindow, $00DCDCDC);
  if EncryptCheckBox.Checked then
    if EncAlgoComboBox.ItemIndex = -1 then EncAlgoComboBox.ItemIndex := 0;
end;

procedure TNewDBForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TNewDBForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TNewDBForm.HelpButtonClick(Sender: TObject);
begin
//
end;

end.
