unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_glpanel, API_timer, API_msgdlg;

type
  TForm1 = class(TForm)
    API_glpanel1: TAPI_glpanel;
    API_grbutton1: TAPI_grbutton;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    API_msgdlg1: TAPI_msgdlg;
    procedure Timer1Timer(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_glpanel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure API_glpanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure API_glpanel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
    model: TAPI_3DSModel;
    oldx: integer;
    oldy: integer;
    anglex: real;
    angley: real;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with api_glpanel1 do
  begin
    startrender;

    startitem;
      rotateitem(anglex,1,0,0);
      rotateitem(angley,0,1,0);
      drawaxis;
      scaleitem(0.1,0.1,0.1);
      model.Draw;
    enditem;

    endrender;
  end;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  opendialog1.Filter:='3DMax models (*.3DS)|*.3DS';
  opendialog1.HistoryList.Clear;
  opendialog1.FileName:='';
  if opendialog1.Execute then
  begin
    // load 3ds model
    if not model.LoadFromFile(opendialog1.FileName) then
    begin
      api_msgdlg1.Caption:='Error';
      api_msgdlg1.Msg.text:='Failed to load 3DS model';
      api_msgdlg1.Execute;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  model:=TAPI_3dsmodel.Create;
  anglex:=0;
  angley:=0;

  with api_glpanel1 do
  begin
    viewpoint.X0:=10;
    viewpoint.Y0:=10;
    viewpoint.z0:=10;
    viewpoint.X1:=0;
    viewpoint.Y1:=0;
    viewpoint.Z1:=0;
    viewpoint.X2:=1;
    viewpoint.Y2:=0;
    viewpoint.Z2:=0;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  model.Free;
end;

procedure TForm1.API_glpanel1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if oldx=0 then
  begin
    oldx:=x;
    oldy:=y;
  end;
end;

procedure TForm1.API_glpanel1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssleft in shift) then
  begin
    anglex:=anglex+(x-oldx)/10;
    angley:=angley+(y-oldy)/10;
  end;
end;

procedure TForm1.API_glpanel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  oldx:=0;
  oldy:=0;
end;

end.
