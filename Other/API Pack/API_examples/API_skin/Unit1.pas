unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_skin, StdCtrls, API_grbutton, API_base;

type
  TForm1 = class(TForm)
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_skin1: TAPI_skin;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_skin1.BlurColor:= $0000ff;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  api_skin1.BlurColor:= $00ff00;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  api_skin1.BlurColor:= $ff0000;
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  api_skin1.BlurColor:= clyellow;
end;

end.
