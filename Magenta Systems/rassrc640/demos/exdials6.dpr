program exdials6;

uses
  Forms,
  exdials1 in 'exdials1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
