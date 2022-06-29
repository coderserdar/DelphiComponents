{**********************************************************}
{                                                          }
{  CD Player                                               }
{  Devrace Extension Library example of                    }
{  TELTrayIcon, TELInstanceChecker                         }
{  ELPackStrings, ELUnpackStrings                          }
{  See Readme.txt for comments                             }
{                                                          }
{  Copyright (c) 2000 - 2001, Balabuyev Yevgeny            }
{  Contact: yebalabuyev@devrace.com                        }
{                                                          }
{ -------------------------------------------------------- }
{  ExtLib home page      : http://www.devrace.com/extlib   }
{  ExtLib support e-mail : extlib@devrace.com              }
{ -------------------------------------------------------- }
{                                                          }
{  Please see the file License.txt for full license        }
{  information                                             }
{                                                          }
{**********************************************************}

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
