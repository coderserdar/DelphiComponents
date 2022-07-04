unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, ExtCtrls, API_glpanel, StdCtrls;

type
  TForm1 = class(TForm)
    API_glpanel1: TAPI_glpanel;
    API_grbutton2: TAPI_grbutton;
    CheckBox1: TCheckBox;
    Timer1: TTimer;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure API_grbutton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
  public
    starttime: tdatetime;
    cube: cardinal;
    sphere: cardinal;
    texture: cardinal;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with api_glpanel1 do
  begin

    // set viewpoint
    viewpoint.X0:=1;
    viewpoint.y0:=2;
    viewpoint.Z0:=15;

    // create cube
    startlist(cube);
      makecube(-2,-2,-2,clwhite,2,2,2,clwhite);
    endlist;

    // create sphere
    startlist(sphere);
      makesphere(3,24,24);
    endlist;

    // load texture
    loadtexture('sample.jpg', texture);

  end;

  starttime:=now;
  windowstate:=Wsmaximized;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  elapsedtime: real;
begin
  elapsedtime:=millisecondspan(now,starttime);

  with api_glpanel1 do
  begin
    wireframe:=checkbox1.checked;

    startrender;

      // additional rotate
      rotateitem(elapsedtime/70,0,1,0);

      // draw axis
      if checkbox2.checked then
        drawaxis;

      // cube
      startitem;
        moveitem(-5,0,0);
        rotateitem(elapsedtime/20,0,0,1);
        drawtextureditem(cube, texture);
      enditem;

      // sphere
      startitem;
        moveitem(5,0,0);
        rotateitem(elapsedtime/12,1,0,0);
        rotateitem(elapsedtime/16,0,1,0);
        rotateitem(elapsedtime/25,0,0,1);
        drawtextureditem(sphere, texture);
      enditem;

    endrender;
  end;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  if checkbox3.checked then
  begin
    // set light and enable lighting
    api_glpanel1.setlight(1, 0,0,0, 0.5,0.5,0.5, 1,1,1, true);
  end else
  begin
    // disable all lighting
    api_glpanel1.DisableLighting;
  end;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  if checkbox4.checked then
  begin
    // create and enable fog
    api_glpanel1.StartFog(2, api_glpanel1.Background, 0.5, 0.5, 18);
  end else
  begin
    // disable fog
    api_glpanel1.EndFog;
  end;
end;

end.
