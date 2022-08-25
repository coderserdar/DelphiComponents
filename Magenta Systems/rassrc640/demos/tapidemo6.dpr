program tapidemo6;

uses
  Forms,
  tapimain6 in 'tapimain6.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
