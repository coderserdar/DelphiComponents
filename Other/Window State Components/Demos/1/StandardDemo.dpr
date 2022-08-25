{
 * StandardDemo.dpr
 *
 * Project file for the Window State Components StandardDemo demo program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program StandardDemo;

uses
  Forms,
  FmDemo in 'FmDemo.pas' {DemoForm},
  FmDemoDlg in 'FmDemoDlg.pas' {DemoDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
