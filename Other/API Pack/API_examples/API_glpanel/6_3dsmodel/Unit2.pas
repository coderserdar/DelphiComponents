unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_glpanel;

type
  TForm1 = class(TForm)
    API_glpanel1: TAPI_glpanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_glpanel1ThreadEvent;
  private
    { Private declarations }
  public
    texture: cardinal;
    cube: cardinal;
    model3ds: tapi_3dsmodel;
    starttime: tdatetime;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dateutils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  model3ds:=tapi_3dsmodel.Create;
  with api_glpanel1 do
  begin
    if not model3ds.LoadFromFile('testi.3ds') then
      messagedlg('failed to load model-',mterror,[mbok],0);

    if not loadtexture('testi.jpg', texture) then
      messagedlg('failed to load texture.',mterror,[mbok],0);

    startlist(cube);
      makecube(-2,-2,-2, clwhite, 2,2,2, clyellow);
    endlist;

    starttime:=now;
    ThreadActive:=true;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_glpanel1.ThreadActive:=false;
  model3ds.Free;
end;

procedure TForm1.API_glpanel1ThreadEvent;
var
  elapsedtime: real;
begin
  elapsedtime:=millisecondspan(now, starttime);
  with api_glpanel1 do
  begin
    startrender;

    {
      startitem;
        rotateitem(elapsedtime/20, 1,0,0);
        rotateitem(elapsedtime/12, 0,1,0);
        drawtextureditem(cube, texture);
      enditem;
     }

      startitem;
//        scaleitem(0.05,0.05,0.05);
        rotateitem(elapsedtime/50, 1,0,0);
        model3ds.draw;
      enditem;

    endrender;
  end;
end;

end.
