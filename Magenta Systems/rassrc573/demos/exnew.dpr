program exnew;

uses
  Forms,
  exnew0 in 'exnew0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
