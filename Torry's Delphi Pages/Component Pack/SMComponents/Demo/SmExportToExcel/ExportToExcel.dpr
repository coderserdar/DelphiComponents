program ExportToExcel;

uses
  Forms,
  Main in 'Main.pas' {ExportExcel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TExportExcel, ExportExcel);
  Application.Run;
end.
