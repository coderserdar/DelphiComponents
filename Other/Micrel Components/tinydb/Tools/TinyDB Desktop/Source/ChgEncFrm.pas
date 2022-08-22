unit ChgEncFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, BaseFrm;

type
  TChgEncFormData = record
    Encrypt: Boolean;
    EncAlg: string;
    Password: string;
  end;

  TChgEncForm = class(TBaseForm)
    OkButton: TButton;
    CancelButton: TButton;
    GroupBox2: TGroupBox;
    EncAlgoLabel: TLabel;
    PasswordLabel: TLabel;
    Password2Label: TLabel;
    EncryptCheckBox: TCheckBox;
    EncAlgoComboBox: TComboBox;
    PasswordEdit: TEdit;
    Password2Edit: TEdit;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure SetData(Value: TChgEncFormData);
    procedure GetData(var Value: TChgEncFormData);
  end;

var
  ChgEncForm: TChgEncForm;

function ShowChgEncForm(var Value: TChgEncFormData): Boolean;

implementation

uses Misc, TinyDB, LangMgr;

{$R *.DFM}

function ShowChgEncForm(var Value: TChgEncFormData): Boolean;
var
  Frm: TChgEncForm;
begin
  Frm := TChgEncForm.Create(Application);
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TChgEncForm.GetData(var Value: TChgEncFormData);
begin
  with Value do
  begin
    Encrypt := EncryptCheckBox.Checked;
    EncAlg := EncAlgoComboBox.Text;
    Password := PasswordEdit.Text;
  end;
end;

procedure TChgEncForm.SetData(Value: TChgEncFormData);
begin
  EncAlgoComboBox.ItemIndex := EncAlgoComboBox.Items.IndexOf(Value.EncAlg);
  EncryptCheckBox.Checked := Value.Encrypt;
  PasswordEdit.Text := Value.Password;
  Password2Edit.Text := Value.Password;
end;

function TChgEncForm.CheckValid: Boolean;
begin
  Result := True;
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

procedure TChgEncForm.FormCreate(Sender: TObject);
begin
  TTinyDatabase.GetEncryptAlgoNames(EncAlgoComboBox.Items);
end;

procedure TChgEncForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TChgEncForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TChgEncForm.EncryptCheckBoxClick(Sender: TObject);
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
  if not Checked then
  begin
    EncAlgoComboBox.ItemIndex := -1;
  end;
end;

end.
