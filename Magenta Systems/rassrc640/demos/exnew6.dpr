program exnew6;

uses
  Forms,
  exnew1 in 'exnew1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
