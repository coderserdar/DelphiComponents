program LHADemo;

uses
  Forms,
  Lhamain in 'LHAMAIN.PAS' {Form1};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.Title := 'TCompLHA Demonstration';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
