unit u_confirmdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, API_abform, API_grbutton, API_base;

type
  Tf_confirmdlg = class(TForm)
    Bevel1: TBevel;
    Memo1: TMemo;
    API_abform1: TAPI_abform;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
  public
    mode: integer;
    answer: boolean;
  end;

var
  f_confirmdlg: Tf_confirmdlg;

implementation

{$R *.dfm}

procedure Tf_confirmdlg.BitBtn2Click(Sender: TObject);
begin
  close;
end;

procedure Tf_confirmdlg.FormShow(Sender: TObject);
begin
  case mode of
    0:
      begin
        api_grbutton1.Caption:='Yes';
        api_grbutton2.caption:='No';
      end;
    1:
      begin
        api_grbutton1.caption:='Apply';
        api_grbutton2.caption:='Cancel';
      end;
  end;
  answer:=false;
end;

procedure Tf_confirmdlg.BitBtn1Click(Sender: TObject);
begin
  answer:=true;
  close;
end;

end.
