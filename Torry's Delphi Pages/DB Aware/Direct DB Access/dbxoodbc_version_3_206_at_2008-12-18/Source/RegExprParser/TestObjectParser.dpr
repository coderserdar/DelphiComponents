program TestObjectParser;

uses
  Forms,
  XPMan,
  uTestObjectParser in 'uTestObjectParser.pas' {frmMain},
  RegExpr in '..\RegExpr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ObjectParser';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
