program Compdemo;

uses
  Forms,
  COMPMAIN in 'COMPMAIN.PAS' {Form1};

{$R *.RES}

begin
  Application.Title := 'TCompress Demonstration';
  Application.HelpFile := 'COMPRESS.HLP';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
