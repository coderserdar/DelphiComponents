unit u_msg2dlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Mask, API_abform, API_base;

type
  Tf_msg2dlg = class(TForm)
    Bevel1: TBevel;
    Memo1: TMemo;
    API_abform1: TAPI_abform;
    procedure BitBtn2Click(Sender: TObject);
  private
  public
  end;

var
  f_msg2dlg: Tf_msg2dlg;

implementation

{$R *.dfm}

procedure Tf_msg2dlg.BitBtn2Click(Sender: TObject);
begin
  close;
end;

end.
