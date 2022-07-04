unit API_ledgrid;

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
// 20052009, r1.08, ari pikivirta
//  * does not erase background anymore -> removes some of the flicker
//
// 06062007, r1.07, ari pikivirta
//  * changed class from tpanel to tpaintbox
//
// 09012007, r1.06, ari pikivirta
//  * added booleangredient property to disable gradient filling
//
// 10062006, r1.05, ari pikivirta
//  * all properties take effect on editor also
//
// 19.9.2005, r1.04, ari pikivirta
//  * changed fixed array size to variable (no maximum limit anymore)
//
// 1.9.2004, r1.03, ari pikivirta
// * added boolean color properties
// * added setasboolean and getasboolean functions
//
// r1.02, ari pikivirta
// * drawing now via temp bitmap to avoid flickering
// * also draws now the background
//
// r1.01, ari pikivirta
// * added transparency and spacing property
// * added iscolor function

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, graphics, API_graphics,
  API_base, Messages;

type
  TAPI_ledgrid = class(TAPI_Custom_Paintbox)
  private
    ftransparentcolor: tcolor;
    ftransparent: boolean;
    fxcount: integer;
    fycount: integer;
    fspacing: integer;
    tempcanvas: tbitmap;
    fbooltrue1, fbooltrue2: tcolor;
    fboolfalse1, fboolfalse2: tcolor;
    fcolor1: array of array of tcolor;
    fcolor2: array of array of tcolor;
    fboolgradient: boolean;

    procedure setxcount(i: integer);
    procedure setycount(i: integer);
    procedure setbooltrue1(c: tcolor);
    procedure setbooltrue2(c: tcolor);
    procedure setboolfalse1(c: tcolor);
    procedure setboolfalse2(c: tcolor);
    procedure setspacing(i: integer);
    procedure settransparent(b: boolean);
    procedure settransparentcolor(c: tcolor);

  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  public
    constructor Create(aowner: tcomponent); override;
    destructor  Destroy; override;
    procedure   SetColor(x,y: integer; color1, color2: tcolor); overload;
    procedure   SetColor(x,y: integer; color: tcolor); overload;
    function    GetColor(x,y: integer): tcolor;
    function    IsColor(x,y: integer; color: tcolor): boolean;
    function    Boolean(x,y: integer; b: boolean): boolean; overload;
    function    Boolean(x,y: integer): boolean; overload;
    function    GetMousePosX(x: integer): integer;
    function    GetMousePosY(y: integer): integer;
    procedure   ResizeGrid(newxcount, newycount: integer);
  published
    property XCount: integer read fxcount write setxcount;
    property YCount: integer read fycount write setycount;
    property Transparent: boolean read ftransparent write settransparent;
    property TransparentColor: tcolor read ftransparentcolor write settransparentcolor;
    property Spacing: integer read fspacing write setspacing;
    property BoolGradient: boolean read fboolgradient write fboolgradient;
    property BoolTrue: tcolor read fbooltrue1 write setbooltrue1;
    property BoolTrue2: tcolor read fbooltrue2 write setbooltrue2;
    property BoolFalse: tcolor read fboolfalse1 write setboolfalse1;
    property BoolFalse2: tcolor read fboolfalse2 write setboolfalse2;

  end;

procedure Register;

implementation

{$r *.res}

const
  versioninfostring: string = 'r1.08/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
constructor TAPI_ledgrid.create(aowner: tcomponent);
begin
  inherited create(aowner);
  version:=versioninfostring;
  fbooltrue1:= clgreen;
  fbooltrue2:= clgray;
  fboolfalse1:= clred;
  fboolfalse2:= clgray;
  setxcount(10);
  setycount(10);
  ftransparent:=false;
  ftransparentcolor:=clblack;
  fspacing:=1;
  tempcanvas:=tbitmap.create;
  fboolgradient:= false;

  // i hate flickering things..
  ControlStyle:= ControlStyle + [csOpaque];
end;

//------------------------------------------------------------------------------
destructor TAPI_ledgrid.destroy;
begin
  tempcanvas.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_Ledgrid.ResizeGrid(newxcount, newycount: integer);
var
  bmp: tbitmap;
  bmpout: tbitmap;
  i,j: integer;
begin
  if (XCount<>newxcount) or (YCount<>newycount) then
  begin
    bmp:= tbitmap.create;
    try

      // get old bitmap
      bmp.Height:= ycount;
      bmp.width:= xcount;
      for i:=0 to bmp.width-1 do
        for j:=0 to bmp.height-1 do
          bmp.Canvas.Pixels[i,j]:= GetColor(i,j);

      // resize grid
      XCount:= newxcount;
      YCount:= newycount;

      // output new bitmap
      bmpout:= tbitmap.create;
      try
        bmpout.Height:= ycount;
        bmpout.width:= xcount;
        bmpout.Canvas.StretchDraw(bmpout.Canvas.ClipRect,bmp);

        // redraw grid
        for i:=0 to bmpout.Width-1 do
          for j:=0 to bmpout.Height-1 do
            SetColor(i,j,bmpout.Canvas.Pixels[i,j]);

      finally
        bmpout.free;
      end;
    finally
      bmp.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setxcount(i: integer);
begin
  if (i>0) then
  begin
    fxcount:=i;
    setlength(fcolor1, fxcount);
    setlength(fcolor2, fxcount);
    setycount(fycount);
    setycount(fycount);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setycount(i: integer);
var
  j: integer;
begin
  if (i>0) then
  begin
    fycount:=i;
    for j:=0 to fxcount-1 do
    begin
      setlength(fcolor1[j], fycount);
      setlength(fcolor2[j], fycount);
    end;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setcolor(x,y: integer; color: tcolor);
begin
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
    begin
      fcolor1[x,y]:=color;
      fcolor2[x,y]:=color;
      invalidate;
    end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setcolor(x,y: integer; color1, color2: tcolor);
begin
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
    begin
      fcolor1[x,y]:=color1;
      fcolor2[x,y]:=color2;
      invalidate;
    end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setbooltrue1(c: tcolor);
begin
  if c<>fbooltrue1 then
  begin
    fbooltrue1:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setbooltrue2(c: tcolor);
begin
  if c<>fbooltrue2 then
  begin
    fbooltrue2:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setboolfalse1(c: tcolor);
begin
  if c<>fboolfalse1 then
  begin
    fboolfalse1:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setboolfalse2(c: tcolor);
begin
  if c<>fboolfalse2 then
  begin
    fboolfalse2:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.setspacing(i: integer);
begin
  if (i<>fspacing) and (i>-1) then
  begin
    fspacing:= i;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.settransparent(b: boolean);
begin
  if ftransparent<>b then
  begin
    ftransparent:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.settransparentcolor(c: tcolor);
begin
  if c<>ftransparentcolor then
  begin
    ftransparentcolor:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.getmouseposx(x: integer): integer;
var
  i: real;
begin
  if fxcount>0 then
  begin
    i:= width / fxcount;
    result:= trunc( x / i );
  end else
    result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.getmouseposy(y: integer): integer;
var
  i: real;
begin
  if fycount>0 then
  begin
    i:= height / fycount;
    result:= trunc( y / i );
  end else
    result:= 0;
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.getcolor(x,y: integer): tcolor;
begin
  result:=0;
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
      result:=fcolor1[x,y];
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.boolean(x,y: integer; b: boolean): boolean;
begin
  result:=false;
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
    begin
      if b then
      begin
        fcolor1[x,y]:= fbooltrue1;
        fcolor2[x,y]:= fbooltrue2;
      end else
      begin
        fcolor1[x,y]:= fboolfalse1;
        fcolor2[x,y]:= fboolfalse2;
      end;
      invalidate;
    end;
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.boolean(x,y: integer): boolean;
begin
  result:=false;
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
      result:= (fcolor1[x,y]= fbooltrue1);
end;

//------------------------------------------------------------------------------
function TAPI_ledgrid.iscolor(x,y: integer; color: tcolor): boolean;
begin
  result:=false;
  if (x>-1) and (x<fxcount) then
    if (y>-1) and (y<fycount) then
      if fcolor1[x,y]=color then result:=True;
end;

//------------------------------------------------------------------------------
procedure TAPI_ledgrid.paint;
var
  mulx: real;
  muly: real;
  x,y: integer;
  rec: trect;
begin
  if (width<1) or (height<1) then exit;

  if fxcount>0 then mulx:= width/fxcount else mulx:= 1;
  if fycount>0 then muly:= height/fycount else muly:= 1;

  tempcanvas.Height:=height;
  tempcanvas.Width:=width;

  for x:=0 to fxcount-1 do
    for y:=0 to fycount-1 do
    begin
      rec.left:=round(x*mulx);
      rec.Right:=round((x+1)*mulx);
      rec.Top:=round(y*muly);
      rec.Bottom:=round((y+1)*muly);

      // draw background
      if (color=ftransparentcolor) and (ftransparent) then
        tempcanvas.canvas.brush.Style:=bsclear else
        tempcanvas.canvas.Brush.Style:=bssolid;
      tempcanvas.canvas.Brush.Color:= color;
      tempcanvas.canvas.FillRect(rec);

      // draw color rect
      tempcanvas.canvas.Brush.Style:=bssolid;
      rec.Right:=rec.right-fspacing;
      rec.Bottom:=rec.bottom-fspacing;

      // gradient led
      if fboolgradient then
      begin
        gradientfill(tempcanvas.Canvas, rec, fcolor1[x,y], fcolor2[x,y], gsvertical);
      end else
      begin
        tempcanvas.Canvas.Brush.Color:= fcolor1[x,y];
        tempcanvas.Canvas.FillRect(rec);
      end;
    end;

  // draw to actual canvas
  canvas.Draw(0,0,tempcanvas);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_ledgrid]);
end;

end.
