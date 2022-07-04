unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_skin, API_progressbar, StdCtrls, API_edit,
  API_base;

type
  TForm2 = class(TForm)
    API_grbutton1: TAPI_grbutton;
    API_progressbar1: TAPI_progressbar;
    Timer1: TTimer;
    API_edit1: TAPI_edit;
    API_skin1: TAPI_skin;
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.API_grbutton1Click(Sender: TObject);
begin
  close;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  (*
  api_progressbar1.Position:= api_progressbar1.position + 1;
  if api_progressbar1.position>99 then
    api_progressbar1.position:= 0;
  *)
end;

end.
