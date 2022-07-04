program Project1;

  { Delphi 2006/2007 }

uses
  Windows, // for Delphi 2006 and DbxOpenOdbcStatic
  // ExceptDlg,
  // MidasLib,
  DbxOpenOdbcStatic, { optional }
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  {$if CompilerVersion >= 18.50}
  Application.MainFormOnTaskbar := True;
  {$ifend}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
