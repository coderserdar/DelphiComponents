unit Unit1;

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
  public
  end;

var
  Form1: TForm1;
  PicPlane: cardinal;
  Pic: cardinal;
  starttime: tdatetime;

implementation

uses
  dateutils;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  api_glpanel1.LoadTexture( 'testi.JPG', pic );
  api_glpanel1.Startlist( Picplane );
  api_glpanel1.MakePlane( -10,-10,0, clwhite, 10,-10,0, clwhite, 10,10,0, clwhite, -10,10,0, clwhite);
  api_glpanel1.Endlist;
  api_glpanel1.ThreadActive:= true;
  starttime:= now;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_glpanel1.ThreadActive:= false;
end;

procedure TForm1.API_glpanel1ThreadEvent;
var
  elapsedtime: double;
begin
  elapsedtime:= secondspan( now, starttime );
  api_glpanel1.StartRender;
    api_glpanel1.StartItem;
      api_glpanel1.RotateItem( elapsedtime*10, 1,1,0);
      api_glpanel1.rotateitem( elapsedtime*17, 0,0,1);
      api_glpanel1.MoveItem( -2, 10- 5*cos(elapsedtime), 5-sin(elapsedtime)*5 );
      api_glpanel1.DrawTexturedItem( PicPlane, pic );
    api_glpanel1.EndItem;
  api_glpanel1.EndRender;
end;

end.
