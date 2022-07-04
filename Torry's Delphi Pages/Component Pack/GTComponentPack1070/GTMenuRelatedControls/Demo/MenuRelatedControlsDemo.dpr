program MenuRelatedControlsDemo;

uses
  Forms,
  f_main in 'f_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
