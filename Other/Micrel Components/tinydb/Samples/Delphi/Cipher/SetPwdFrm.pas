unit SetPwdFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSetPwdForm = class(TForm)
    GroupBox1: TGroupBox;
    PromptLabel: TLabel;
    PasswordEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    Label1: TLabel;
    Password2Edit: TEdit;
    Panel1: TPanel;
    Image1: TImage;
    TipLabel: TLabel;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
    function CheckValid: Boolean;
  public
    { Public declarations }
    procedure GetData(var Value: string);
  end;

var
  SetPwdForm: TSetPwdForm;

function ShowSetPwdForm(var Value: string; Tip, Title: string): Boolean;

implementation

uses StrRes;

{$R *.DFM}

function ShowSetPwdForm(var Value: string; Tip, Title: string): Boolean;
var
  Frm: TSetPwdForm;
begin
  Frm := TSetPwdForm.Create(Application);
  Frm.Caption := Title;
  Frm.TipLabel.Caption := Tip;
  Result := Frm.ShowModal = mrOk;
  if Result then Frm.GetData(Value);
  Frm.Free;
end;

procedure TSetPwdForm.GetData(var Value: string);
begin
  Value := PasswordEdit.Text;
end;

function TSetPwdForm.CheckValid: Boolean;
begin
  Result := True;
  if PasswordEdit.Text <> Password2Edit.Text then
  begin
    PasswordEdit.SetFocus;
    Application.MessageBox(PChar(SPwdDifferent), PChar(Application.Title) , 48);
    Result := False;
    Exit;
  end;
end;

procedure TSetPwdForm.OkButtonClick(Sender: TObject);
begin
  if CheckValid then
    ModalResult := mrOk;
end;

procedure TSetPwdForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
