unit ChgPwdFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TChgPwdFormData = record
    CheckPwd: Boolean;
    Password: string;
  end;

  TChgPwdForm = class(TForm)
    GroupBox1: TGroupBox;
    PromptLabel: TLabel;
    PasswordEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    CheckPwdCheckBox: TCheckBox;
    Label1: TLabel;
    Password2Edit: TEdit;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckPwdCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure SetData(Value: TChgPwdFormData);
    procedure GetData(var Value: TChgPwdFormData);
  end;

var
  ChgPwdForm: TChgPwdForm;

function ShowChgPwdForm(var Value: TChgPwdFormData): Boolean;

implementation

uses StrRes;

{$R *.DFM}

function ShowChgPwdForm(var Value: TChgPwdFormData): Boolean;
var
  Frm: TChgPwdForm;
begin
  Frm := TChgPwdForm.Create(Application);
  Frm.SetData(Value);
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

function Iif(Value: Boolean; Value1, Value2: Variant): Variant;
begin
  if Value then Result := Value1
  else Result := Value2;
end;

procedure TChgPwdForm.SetData(Value: TChgPwdFormData);
begin
  CheckPwdCheckBox.Checked := Value.CheckPwd;
end;

procedure TChgPwdForm.GetData(var Value: TChgPwdFormData);
begin
  Value.CheckPwd := CheckPwdCheckBox.Checked;
  Value.Password := PasswordEdit.Text;
end;

function TChgPwdForm.CheckValid: Boolean;
begin
  Result := True;
  if CheckPwdCheckBox.Checked then
  begin
    if PasswordEdit.Text <> Password2Edit.Text then
    begin
      PasswordEdit.SetFocus;
      Application.MessageBox(PChar(SPwdDifferent), PChar(Application.Title), 48);
      Result := False;
      Exit;
    end;
  end;
end;

procedure TChgPwdForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
    ModalResult := mrOk;
end;

procedure TChgPwdForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TChgPwdForm.CheckPwdCheckBoxClick(Sender: TObject);
var
  Checked: Boolean;
begin
  Checked := CheckPwdCheckBox.Checked;
  PasswordEdit.Enabled := Checked;
  PasswordEdit.Color := Iif(Checked, clWindow, $00DCDCDC);
  Password2Edit.Enabled := Checked;
  Password2Edit.Color := Iif(Checked, clWindow, $00DCDCDC);
  if not Checked then
  begin
    PasswordEdit.Text := '';
    Password2Edit.Text := '';
  end;
end;

end.
