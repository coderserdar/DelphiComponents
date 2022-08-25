{
 * FmDemoDlg.pas
 *
 * Subsidiary dialog box form for the Window State Components StandardDemo demo
 * program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemoDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PJWdwState, StdCtrls;

type
  TDemoDlg = class(TForm)
    PJRegWdwState1: TPJRegWdwState;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PJRegWdwState1ReadWdwState(Sender: TObject; var Left, Top,
      Width, Height, State: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

{
  The TPJRegWdwState component is set up with the following non-default property
  values:
    IgnoreState = True
    Options = [woIgnoreState, woIgnoreSize]
    OnReadWdwState = PJRegWdwState1ReadWdwState
    SubKey = 'Software\DelphiDabbler\Demos\WindowState\Dlg'
}

procedure TDemoDlg.FormShow(Sender: TObject);
begin
  // PJRegWdwState1.AutoSaveRestore is false so we must call
  // TPJRegWdwState.Restore when the form is shown ...
  PJRegWdwState1.Restore;
end;

procedure TDemoDlg.FormHide(Sender: TObject);
begin
  // ... and TPJRegWdwState.Save when the form is hidden to store the window
  // info
  PJRegWdwState1.Save;
end;

procedure TDemoDlg.PJRegWdwState1ReadWdwState(Sender: TObject; var Left,
  Top, Width, Height, State: Integer);
begin
  // Display size and position info read from registry
  Label1.Caption := Format(
    'Left=%d, Top=%d'#10'Width=%d, Height=%d',
    [Left, Top, Width, Height]
  )
end;

end.
