{
 * FmMDIMain.pas
 *
 * MDI main form for the Window State Components MDIChild demo program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmMDIMain;

interface

uses
  Controls, ExtCtrls, ComCtrls, ToolWin, Menus, Classes, Forms,
  PJWdwState;

type

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    PJWdwState1: TPJWdwState;
    File1: TMenuItem;
    Exit1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure Exit1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.

