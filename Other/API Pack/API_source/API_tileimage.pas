unit API_tileimage;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.03, ari pikivirta
// * fixed drawing tiled image ( argh )
// * fixed drawing while in designing state
//
// r1.02, ari pikivirta
// * added center property for centering the image
//
// r1.01, ari pikivirta
// * just updated the email in revision information

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  graphics;

type
  TAPI_tileimage = class(tpaintBox)
  private
    fversion: string;
    ftileimage: boolean;
    fpicture: tpicture;
    fcenter: boolean;
    procedure _picturechanged(sender:tobject);
    procedure _setpicture(picture:tpicture);
    procedure _settileimage(b:boolean);
    procedure dummys(s: string);

  protected
    procedure Paint; override;

  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

  published
    property Version: string read fversion write dummys stored false;
    property Picture: TPicture read fpicture write _setpicture;
    property TileImage:boolean read ftileimage write _settileimage;
    property Center: boolean read fcenter write fcenter;
    property visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

  end;

procedure Register;

implementation

const
  versioninfo = 'r1.03/ari.pikivirta@kolumbus.fi';

{$R *.RES}

procedure TAPI_tileimage.dummys(s: string); begin end;

//--------------------------------------------------------------
procedure tAPI_tileimage._settileimage(b:boolean);
begin
  if ftileimage<>b then
  begin
    ftileimage:=B;
    invalidate;
  end;
end;

//--------------------------------------------------------------
constructor tAPI_tileimage.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  ftileimage:=false;
  fpicture:=tpicture.create;
  fpicture.onchange:=_picturechanged;
  height:=50;
  width:=50;
  fcenter:=false;
end;

//--------------------------------------------------------------
destructor tAPI_tileimage.destroy;
begin
  fpicture.free;
  fpicture:= nil;
  inherited destroy;
end;

//--------------------------------------------------------------
procedure tAPI_tileimage.paint;
var
  x: integer;
  y: integer;
  cw: integer;
  ch: integer;
  w: integer;
  h: integer;
begin
  with inherited canvas do
  begin
    if fpicture<>nil then
    begin
      if ftileimage then
      begin
        w:=picture.width;
        h:=picture.height;
        y:=0;
        cw:=width;
        ch:=height;
        while y<ch do
        begin
          x:=0;
          while x<cw do
          begin
            draw(x,y,fpicture.graphic);
            x:=x+w;
          end;
          y:=y+h;
        end;
      end else
      begin
        if fcenter then
        begin
          // draw picture centered
          draw((width-fpicture.Width) div 2, (height-fpicture.Height) div 2, fpicture.Graphic);
        end else
        begin
          // just throw picture in
          draw(0,0,fpicture.graphic);
        end;
      end;
    end else
    with inherited canvas do
    begin
      pen.style:=psdash;
      brush.style:=bssolid;
      brush.Color:= color;
      rectangle(0,0,width,height);
    end;
  end;
end;

//--------------------------------------------------------------
procedure tAPI_tileimage._setpicture(picture:tpicture);
begin
  fpicture.assign(picture);
  invalidate;
end;

//--------------------------------------------------------------
procedure tAPI_tileimage._picturechanged(sender:tobject);
begin
  try
    invalidate;
  except
  end;
end;

//--------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_tileimage]);
end;

end.
