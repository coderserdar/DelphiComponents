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
    CheckBox2: TCheckBox;
    procedure API_grbutton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure API_glpanel1ThreadEvent;
  private
  public
    starttime: tdatetime;
    cube: cardinal;
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
  starttime:=now;
  with api_glpanel1 do
  begin

    // set viewpoint
    viewpoint.X0:=1;
    viewpoint.y0:=2;
    viewpoint.Z0:=15;

    // create cube
    startlist(cube);
      makecube(-2,-2,-2,clwhite,2,2,2,clgreen);
    endlist;

    threadactive:=true;
  end;

end;

procedure TForm1.API_glpanel1ThreadEvent;
var
  elapsedtime: real;
begin
  elapsedtime:=millisecondspan(now,starttime);

  with api_glpanel1 do
  begin
    wireframe:=checkbox1.checked;

    startrender;

      // draw axis
      if checkbox2.checked then
        drawaxis;

      // first cube
      startitem;
        moveitem(-5,0,0);
        rotateitem(elapsedtime/20,0,0,1);
        drawitem(cube);
      enditem;

      // second cube
      startitem;
        moveitem(5,0,0);
        rotateitem(elapsedtime/12,1,0,0);
        rotateitem(elapsedtime/16,0,1,0);
        rotateitem(elapsedtime/25,0,0,1);
        drawitem(cube);
      enditem;

    endrender;
  end;
end;

end.
