{
 * FmMDIChild.pas
 *
 * MDI child form for the Window State Components MDIChild demo program.
 *
 * $Rev: 1043 $
 * $Date: 2013-01-07 17:36:25 +0000 (Mon, 07 Jan 2013) $
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmMDIChild;

interface

uses
  Classes, Controls, StdCtrls, Forms,
  PJWdwState, ExtCtrls;

type
  TForm2 = class(TForm)
    PJWdwState1: TPJWdwState;
    Label1: TLabel;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.

