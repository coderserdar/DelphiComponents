program bmtest;

uses
  Forms,
  bmtestf in 'bmtestf.pas' {Form1};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
