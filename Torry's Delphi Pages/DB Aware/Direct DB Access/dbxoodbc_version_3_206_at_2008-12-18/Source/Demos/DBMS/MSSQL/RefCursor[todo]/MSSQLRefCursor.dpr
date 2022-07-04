program MSSQLRefCursor;

uses
//  ExceptDlg,
  Windows,
  //{
  DbxOpenOdbc,
  DbxOpenOdbc3,
  DbxOpenOdbcStatic,
  {}
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'MSSQLRefCursor';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  TerminateProcess(OpenProcess(PROCESS_TERMINATE, False, GetCurrentProcessID()), 0);
end.
