unit DemoProtectDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TProtectForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Image2: TImage;
    Button1: TButton;
    Label4: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Edit1: TEdit;
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
  ProtectForm: TProtectForm;

implementation
{$R *.dfm}


procedure TProtectForm.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TProtectForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TProtectForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked Then
     Begin
       Edit1.PasswordChar := '*';
       Edit2.PasswordChar := '*';
     End
  Else
     Begin
       Edit1.PasswordChar := #0;
       Edit2.PasswordChar := #0;
     End;
end;

procedure TProtectForm.FormShow(Sender: TObject);
begin
  Edit2.SetFocus;
end;

end.
