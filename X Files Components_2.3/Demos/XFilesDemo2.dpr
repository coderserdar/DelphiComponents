program XFilesDemo2;

uses
  Forms,
  XFilesUnit2 in 'XFilesUnit2.pas' {XFilesForm2};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'X-Files Demo #2';
  Application.CreateForm(TXFilesForm2, XFilesForm2);
  Application.Run;
end.
