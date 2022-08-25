{
 * MDIDemo.pas
 *
 * Project file for the Window State Components MDIChild demo program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program MDIDemo;

uses
  Forms,
  FmMDIMain in 'FmMDIMain.pas' {Form1},
  FmMDIChild in 'FmMDIChild.pas' {Form2};

{$R *.res}     

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

