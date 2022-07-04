program mbXPLibDemo;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'XP Lib Pack v1.3 Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
