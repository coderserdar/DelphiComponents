program makeexe;

uses
  Forms,
  makeexef in 'makeexef.pas' {Form1};

{$R *.RES}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  Application.Title := 'MakeEXE';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
