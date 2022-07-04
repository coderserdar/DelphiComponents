{*******************************************************}
{                                                       }
{       CD Player                                       }
{       Extension Library example of                    }
{       TELTrayIcon, TELInstanceChecker                 }
{       ELPackStrings, ELUnpackStrings                  }
{       See Readme.txt for comments                     }
{                                                       }
{       (c) 2000 - 2001, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

program CDPlayer;

uses
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  CD_Routines in 'CD_Routines.pas',
  dlgExitWindowsUnit in 'dlgExitWindowsUnit.pas' {dlgExitWindows};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CD PLayer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.ShowMainForm := False;
  Application.Run;
end.
