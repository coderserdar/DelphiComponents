unit DemoPasswordDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TPasswordForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Image2: TImage;
    Button1: TButton;
    Label4: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PasswordForm: TPasswordForm;

implementation
{$R *.dfm}


procedure TPasswordForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TPasswordForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TPasswordForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked Then
     Edit2.PasswordChar := '*'
  Else
     Edit2.PasswordChar := #0;
end;

procedure TPasswordForm.FormShow(Sender: TObject);
begin
  Edit2.SetFocus;
end;

end.
