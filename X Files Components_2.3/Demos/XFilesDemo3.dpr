program XFilesDemo3;

uses
  Forms,
  XFilesUnit3 in 'XFilesUnit3.pas' {XFilesForm3};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'X-Files Demo #3';
  Application.CreateForm(TXFilesForm3, XFilesForm3);
  Application.Run;
end.
