program TraceDemo;

uses
  Forms,
  TraceDemo1 in 'TraceDemo1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
