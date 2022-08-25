{
 * UserDemo.dpr
 *
 * Project file for the Window State Components UserDemo demo program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program UserDemo;

uses
  Forms,
  FmUserDemo in 'FmUserDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
