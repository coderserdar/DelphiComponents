program Project4;

uses
  Forms,
  Unit4 in 'Unit4.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
