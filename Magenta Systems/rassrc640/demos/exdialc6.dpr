program exdialc6;

uses
  Forms,
  exdialc1 in 'exdialc1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
