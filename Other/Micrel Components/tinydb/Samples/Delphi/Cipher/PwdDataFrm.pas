unit PwdDataFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TPwdDataFormData = record
    Password: string;
    Note: string;
  end;

  TPwdDataForm = class(TForm)
    GroupBox1: TGroupBox;
    NoteMemo: TMemo;
    GroupBox2: TGroupBox;
    PasswordEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure SetData(Value: TPwdDataFormData);
    procedure GetData(var Value: TPwdDataFormData);
  end;

var
  PwdDataForm: TPwdDataForm;

function ShowPwdDataForm(var Value: TPwdDataFormData): Boolean;

implementation

{$R *.DFM}

function ShowPwdDataForm(var Value: TPwdDataFormData): Boolean;
var
  Frm: TPwdDataForm;
begin
  Frm := TPwdDataForm.Create(Application);
  Frm.SetData(Value);
  Result := (Frm.ShowModal = mrOk);
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TPwdDataForm.GetData(var Value: TPwdDataFormData);
begin
  Value.Password := PasswordEdit.Text;
  Value.Note := NoteMemo.Lines.Text;
end;

procedure TPwdDataForm.SetData(Value: TPwdDataFormData);
begin
  PasswordEdit.Text := Value.Password;
  NoteMemo.Lines.Text := Value.Note;
end;

function TPwdDataForm.CheckValid: Boolean;
begin
  Result := True;
end;

procedure TPwdDataForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
    ModalResult := mrOk;
end;

procedure TPwdDataForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
