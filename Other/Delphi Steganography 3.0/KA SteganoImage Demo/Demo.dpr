program Demo;

uses
  Forms,
  DemoUnit1 in 'DemoUnit1.pas' {Form1},
  DemoProtectDialog in 'DemoProtectDialog.pas' {ProtectForm},
  DemoPasswordDialog in 'DemoPasswordDialog.pas' {PasswordForm},
  DemoNextImageDialog in 'DemoNextImageDialog.pas' {NextImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'KA Steganography Image Demo';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TProtectForm, ProtectForm);
  Application.CreateForm(TPasswordForm, PasswordForm);
  Application.CreateForm(TNextImageForm, NextImageForm);
  Application.Run;
end.
