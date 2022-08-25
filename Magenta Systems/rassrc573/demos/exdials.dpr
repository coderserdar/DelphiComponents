program exdials;

uses
  Forms,
  exdials0 in 'exdials0.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
