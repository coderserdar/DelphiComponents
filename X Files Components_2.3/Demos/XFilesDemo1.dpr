program XFilesDemo1;

uses
  Forms,
  XFilesUnit1 in 'XFilesUnit1.pas' {XFilesForm1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'X-Files Demo #1';
  Application.CreateForm(TXFilesForm1, XFilesForm1);
  Application.Run;
end.
