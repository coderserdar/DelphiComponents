unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, API_msg2dlg, ExtCtrls, API_base;

type
  TForm1 = class(TForm)
    tAPI_msg2dlg1: tAPI_msg2dlg;
    tAPI_grbutton1: tAPI_grbutton;
    tAPI_grbutton2: tAPI_grbutton;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure tAPI_grbutton1Click(Sender: TObject);
    procedure tAPI_grbutton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.tAPI_grbutton1Click(Sender: TObject);
begin
  tapi_msg2dlg1.Caption:='testi';
  tapi_msg2dlg1.Msg.Clear;
  tapi_msg2dlg1.Msg.add('This will be closed when the other btn is clicked.');
  tapi_msg2dlg1.show;
end;

procedure TForm1.tAPI_grbutton2Click(Sender: TObject);
begin
  tapi_msg2dlg1.hide;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: integer;
begin
  i:= tapi_msg2dlg1.msg.count;
  if i<2 then
    tapi_msg2dlg1.Msg.Add('1') else
    tapi_msg2dlg1.msg[i-1]:= inttostr( strtoint( tapi_msg2dlg1.msg[i-1] ) +1 );
  tapi_msg2dlg1.Update;
end;

end.
