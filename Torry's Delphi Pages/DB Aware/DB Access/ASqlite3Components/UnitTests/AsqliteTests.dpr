program AsqliteTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  ASGSQLite3 in '../ASGSQLite3.pas',
  AMDSqlite3 in '../AMDSqlite3.pas' {FMD},
  ASGRout3 in '../ASGRout3.pas',
  NullTests in 'NullTests.pas',
  ModTestU in 'ModTestU.pas' {modTest: TDataModule};
{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

