unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_grbutton, ExtCtrls, API_glpanel, StdCtrls;

type
  TForm1 = class(TForm)
    API_glpanel1: TAPI_glpanel;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_grbutton3: TAPI_grbutton;
    API_grbutton4: TAPI_grbutton;
    API_grbutton5: TAPI_grbutton;
    API_grbutton6: TAPI_grbutton;
    ScrollBar1: TScrollBar;
    Timer1: TTimer;
    procedure API_grbutton6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_grbutton3Click(Sender: TObject);
    procedure API_grbutton4Click(Sender: TObject);
    procedure API_grbutton5Click(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    triangle: cardinal;
    plane: cardinal;
    cube: cardinal;
    sphere: cardinal;
    cylinder: cardinal;
    drawturn: integer;
    angle: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton6Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  // create items
  with api_glpanel1 do
  begin

    // triangle
    startlist(triangle);
      maketriangle(-2,-2,0,clred,2,-2,0,clgreen,0,2,0,clblue);
    endlist;

    // plane
    startlist(plane);
      makeplane(-2,-2,0,clwhite,2,-2,0,clred,2,2,0,clgreen,-2,2,0,clblue);
    endlist;

    // cube
    startlist(cube);
      makecube(-2,-2,-2,clgreen,2,2,2,clyellow);
    endlist;

    // sphere
    startlist(sphere);
      setcolor(clyellow);
      makesphere(2.5,16,16);
    endlist;

    // cylinder
    startlist(cylinder);
      setcolor(clsilver);
      makecylinder(2,2.5,5,16,16);
    endlist;

  end;

  drawturn:=0;
  angle:=0;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  drawturn:=0;
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  drawturn:=1;
end;

procedure TForm1.API_grbutton3Click(Sender: TObject);
begin
  drawturn:=2;
end;

procedure TForm1.API_grbutton4Click(Sender: TObject);
begin
  drawturn:=3;
end;

procedure TForm1.API_grbutton5Click(Sender: TObject);
begin
  drawturn:=4;
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  angle:=scrollpos;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with api_glpanel1 do
  begin
    startrender;
      startitem;
        rotateitem(angle,0,0,1);
        case drawturn of
          0: drawitem(triangle);
          1: drawitem(plane);
          2: drawitem(cube);
          3: drawitem(sphere);
          4: drawitem(cylinder);
        end;
      enditem;
    endrender;
  end;
end;

end.
