unit u_msgdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, API_abform, API_grbutton, API_base;

type
  Tf_msgdlg = class(TForm)
    API_abform1: TAPI_abform;
    API_grbutton1: TAPI_grbutton;
    Timer1: TTimer;
    Memo1: TMemo;
    Bevel1: TBevel;
    procedure BitBtn2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    timeout: integer;
  end;

var
  f_msgdlg: Tf_msgdlg;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure Tf_msgdlg.BitBtn2Click(Sender: TObject);
begin
  close;
end;

//------------------------------------------------------------------------------
procedure Tf_msgdlg.Timer1Timer(Sender: TObject);
begin
  timeout:= timeout - 1;
  if timeout<0 then
    BitBtn2Click(self);
end;

end.
