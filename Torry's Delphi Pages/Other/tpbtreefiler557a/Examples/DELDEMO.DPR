{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

program Deldemo;

uses
  Forms,
  Deldemo1 in 'DELDEMO1.PAS' {DemoForm},
  Deldemo2 in 'DELDEMO2.PAS' {NetTypeDlg},
  Deldemo3 in 'DELDEMO3.PAS' {EditDlg},
  Deldemo4 in 'DELDEMO4.PAS' {AboutBox},
  Deldemo5 in 'DELDEMO5.PAS' {StatForm};

{$IFDEF Windows}
{$R *.R16}
{$ELSE}
{$R *.R32}
{$ENDIF}

begin
  Application.CreateForm(TDemoForm, DemoForm);
  Application.CreateForm(TNetTypeDlg, NetTypeDlg);
  Application.CreateForm(TEditDlg, EditDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TStatForm, StatForm);
  Application.Run;
end.
