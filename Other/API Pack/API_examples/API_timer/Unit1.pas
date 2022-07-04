unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_timer, StdCtrls, API_edit, API_label;

type
  TForm1 = class(TForm)
    API_timer1: TAPI_timer;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_edit1: TAPI_edit;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    Label1: TLabel;
    API_label1: TAPI_label;
    Label2: TLabel;
    Timer1: TTimer;
    API_grbutton5: TAPI_grbutton;
    procedure API_grbutton5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_timer1Notify(sender: TComponent; ElapsedMS,
      IntervalMS: Double; var ResetInterval: Boolean);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
  private
    { Private declarations }
    eventcounter: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  api_timer1.Start;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  api_edit1.text:= floattostr(api_timer1.ElapsedMs);
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  api_timer1.Stop;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  api_edit1.text:= floattostr(api_timer1.InstervalMs);
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  api_timer1.Active:= not api_timer1.Active;
end;

procedure TForm1.API_timer1Notify(sender: TComponent; ElapsedMS,
  IntervalMS: Double; var ResetInterval: Boolean);
begin
  eventcounter:= eventcounter + 1;
  label2.Caption:= inttostr(eventcounter);
  resetinterval:= false;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if api_timer1.Active then api_label1.Caption:= 'Timer Active'
    else api_label1.caption:= 'Timer Stopped';
end;

end.
