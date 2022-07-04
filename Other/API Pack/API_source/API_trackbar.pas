unit API_trackbar;

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
// 20052009, r1.03, ari pikivirta
//  * does not erase background anymore -> reduces some flicker
//
// 21112005, r1.02, ari pikivirta
//  * changed background buffer bitmap to be created at startup to avoid
//    flickering when moving the thumb
//
// 03102005, r1.01, ari pikivirta
//  * fixed track back colors if min is set higher than zero
//  * added gradient coloring for the background
//  * drawing now first to temporary bitmap to avoid flickering
//  * added properties to limit thumb and value range

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, API_graphics;

type
  TAPI_trackbar_orientation = (TTO_horizontal, TTO_vertical);
  TAPI_trackbar_onchage = procedure(sender: tobject; value: extended) of object;

  TAPI_trackbar = class(Tpaintbox)
  private
    { Private declarations }
    fversion: string;
    fhitrect: trect;
    fthumbrect: trect;
    ftrackrect: trect;
    fvalue: extended;
    fminimum: extended;
    fmaximum: extended;
    ftrackmin: extended;
    ftrackmax: extended;
    fbordercolor: tcolor;
    fbackgroundcolor: tcolor;
    fthumbposition: integer;
    foldthumbposition: integer;
    fthumbheight: integer;
    fthumbwidth: integer;
    fthumbmin: integer;
    fthumbmax: integer;
    fthumbcolor: tcolor;
    fthumbborder: tcolor;
    ftrackheight: integer;
    fsteps: integer;
    ftrack1color: tcolor;
    ftrack1border: tcolor;
    ftrack2color: tcolor;
    ftrack2border: tcolor;
    forientation: TAPI_trackbar_orientation;
    fonchange: TAPI_trackbar_onchage;
    fbmp: tbitmap;
    procedure dummys(s: string);
    procedure setmax(e: extended);
    procedure setmin(e: extended);
    procedure setvalue(e: extended);
    procedure setbordercolor(c: tcolor);
    procedure setbackgroundcolor(c: tcolor);
    procedure setthumbcolor(c: tcolor);
    procedure setthumbborder(c: tcolor);
    procedure setorientation(o: TAPI_trackbar_orientation);
    procedure setthumbwidth(i: integer);
    procedure setthumbheight(i: integer);
    procedure settrack1color(c: tcolor);
    procedure settrack1border(c: tcolor);
    procedure settrack2color(c: tcolor);
    procedure settrack2border(c: tcolor);
    procedure settrackheight(i: integer);
    procedure updatevalue;
    procedure updatethumbminmax;
    procedure settrackrect;
    procedure setthumbrect;
    procedure updateposition;
  protected
    { Protected declarations }
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);override;
    procedure Resize; override;
  public
    { Public declarations }
    constructor Create (AOwner:TComponent);override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    function Step: integer;
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property Maximum: extended read fmaximum write setmax;
    property Minimum: extended read fminimum write setmin;
    property Value: extended read fvalue write setvalue;
    property ColorBorder: tcolor read fbordercolor write setbordercolor;
    property ColorBackground: tcolor read fbackgroundcolor write setbackgroundcolor;
    property ColorThumb: tcolor read fthumbcolor write setthumbcolor;
    property ColorThumbBorder: tcolor read fthumbborder write setthumbborder;
    property ThumbWidth: integer read fthumbwidth write setthumbwidth;
    property ThumbHeight: integer read fthumbheight write setthumbheight;
    property ColorTrack1: tcolor read ftrack1color write settrack1color;
    property ColorTrack1Border: tcolor read ftrack1border write settrack1border;
    property ColorTrack2: tcolor read ftrack2color write settrack2color;
    property ColorTrack2Border: tcolor read ftrack2border write settrack2border;
    property TrackHeight: integer read ftrackheight write settrackheight;
    property Orientation: TAPI_trackbar_orientation read forientation write setorientation;
    property OnChange: TAPI_trackbar_onchage read fonchange write fonchange;
    property TrackMin: extended read ftrackmin write ftrackmin;
    property TrackMax: extended read ftrackmax write ftrackmax;
    property Steps: integer read fsteps write fsteps;
  end;

procedure Register;

implementation

const
  versioninfostring: string = 'r1.03/ari.pikivirta@kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
procedure TAPI_trackbar.dummys(s: string);
begin
  // nothing here.
end;

//------------------------------------------------------------------------------
destructor TAPI_trackbar.Destroy;
begin
  inherited;
  fbmp.free;
end;

//------------------------------------------------------------------------------
constructor TAPI_trackbar.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  fbmp:= tbitmap.create;
  foldthumbposition:= 0;
  width:= 150;
  height:= 24;
  fversion:= versioninfostring;
  forientation:= TTO_horizontal;
  fminimum:= 0;
  ftrackmin:= 0;
  fmaximum:= 100;
  ftrackmax:= 0;
  fvalue:= 20;
  fsteps:= 0;
  fbackgroundcolor:= clblack;
  fthumbheight:= 20;
  fthumbwidth:= 5;
  fthumbcolor:= clwhite;
  fthumbborder:= clsilver;
  ftrackheight:= 6;
  ftrack1color:= clgreen;
  ftrack2color:= clred;
  ftrack1border:= clsilver;
  ftrack2border:= clsilver;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.Resize;
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.updatevalue;
begin
  fvalue:= fminimum + (fthumbposition - fthumbmin) / (fthumbmax - fthumbmin) * (fmaximum - fminimum);
  if (ftrackmin<ftrackmax) then                   // if track range defined
  begin
    if fvalue<ftrackmin then fvalue:= ftrackmin;   // limit to minimum
    if fvalue>ftrackmax then fvalue:= ftrackmax;   // limit to maximum
  end;
  if (fsteps>0) then
  begin
    fvalue:= fminimum + (round(fsteps*(fvalue-fminimum)/(fmaximum-fminimum))*(fmaximum-fminimum)/fsteps);
  end;
end;

//------------------------------------------------------------------------------
function TAPI_trackbar.Step: integer;
begin
  if fsteps>0 then
  begin
    result:= round(fsteps*(fvalue-fminimum)/(fmaximum-fminimum));
  end else
    result:= 0;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrackheight(i: integer);
begin
  if (i<>ftrackheight) then
  begin
    ftrackheight:= i;
    repaint;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setthumbwidth(i: integer);
begin
  if (i<>fthumbwidth) then
  begin
    fthumbwidth:= i;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setthumbheight(i: integer);
begin
  if (i<>fthumbheight) then
  begin
    fthumbheight:= i;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.updatethumbminmax;
begin
  if (forientation = TTO_horizontal) then
  begin
    fthumbmin:= fthumbwidth;
    fthumbmax:= width - fthumbwidth;
  end else
  begin
    fthumbmin:= fthumbheight;
    fthumbmax:= height - fthumbheight;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.updateposition;
var
  fac: extended;
begin
  if (fmaximum-fminimum)<>0 then
  begin
    fac:= (FValue-FMinimum)/(FMaximum-FMinimum);
    FThumbPosition:= FThumbMin+round((FThumbMax-FThumbMin)*fac);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrackrect;
var
  dy,dx: integer;
begin
  if (forientation = TTO_horizontal) then
  begin
    dy:=(height-FTrackHeight) div 2;
    fTrackRect:=Rect(FThumbMin,dy,FThumbMax,height-dy);
    FHitRect:=FTrackrect;
    inflateRect(FHitRect,0,(FThumbHeight-FTrackHeight) div 2);
  end else
  begin
    dx:=(Width-FTrackHeight) div 2;
    FTrackRect:=Rect(dx,FThumbMin,Width-dx,FThumbMax);
    FHitRect:=FTrackrect;
    inflateRect(FHitRect,(FThumbWidth-FTrackHeight) div 2,0);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setthumbrect;
var
  dx,dy: integer;
begin
  if (forientation = TTO_horizontal) then
  begin
    dx:=FThumbWidth div 2;
    dy:=(height-FThumbHeight) div 2;
    FThumbrect:=Rect(FThumbPosition-dx,dy,FThumbPosition+dx,height-dy);
  end else
  begin
    dy:=FThumbHeight div 2;
    dx:=(Width-FThumbWidth) div 2;
    FThumbrect:=Rect(dx,FThumbPosition-dy,Width-dx,FThumbPosition+dy);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.Paint;

  procedure drawbackground( bmp: tbitmap );
  begin
    bmp.canvas.brush.color:= fbackgroundcolor;
    bmp.canvas.FillRect(rect(0,0,width,height));
    bmp.canvas.brush.Color:= fbordercolor;
    bmp.canvas.FrameRect(rect(0,0,width,height));
  end;

  procedure drawtrack( bmp: tbitmap );
  var
    tw: integer;
  begin
    if forientation = TTO_horizontal then
    begin
      if (fvalue - fminimum) <> 0 then
        tw:= round ((ftrackrect.Right - ftrackrect.Left) * (fvalue - fminimum) / (fmaximum - fminimum)) + fthumbwidth
        else tw:= round ((ftrackrect.right - ftrackrect.left) * 1/100) + fthumbwidth;
      bmp.canvas.brush.color:= FTrack1Color;
      gradientfill(bmp.canvas,rect(FTrackRect.Left, ftrackrect.Top, tw, ftrackrect.Bottom),ftrack1border,ftrack1color, gshorizontal);
      //canvas.FillRect(rect(FTrackRect.Left, ftrackrect.Top, tw, ftrackrect.Bottom));
      bmp.canvas.Brush.Color:= ftrack1border;
      bmp.canvas.FrameRect(rect(FTrackRect.Left, ftrackrect.Top, tw, ftrackrect.Bottom));
      bmp.canvas.brush.color:= FTrack2Color;
      gradientfill(bmp.canvas,rect(tw, ftrackrect.Top, ftrackrect.right, ftrackrect.Bottom),ftrack2color,ftrack2border, gshorizontal);
      //canvas.FillRect(rect(tw, ftrackrect.Top, ftrackrect.right, ftrackrect.Bottom));
      bmp.canvas.Brush.Color:= ftrack2border;
      bmp.canvas.FrameRect(rect(tw, ftrackrect.Top, ftrackrect.right, ftrackrect.Bottom));
    end else
    begin
      if (fvalue - fminimum) <> 0 then
        tw:= round ((ftrackrect.bottom - ftrackrect.top) * (fvalue - fminimum) / (fmaximum - fminimum)) + fthumbheight
        else tw:= round ((ftrackrect.bottom - ftrackrect.top) * 1/100) + fthumbwidth;
      bmp.canvas.brush.color:= FTrack2Color;
      gradientfill(bmp.canvas,rect(FTrackRect.Left, ftrackrect.Top, ftrackrect.right, tw),ftrack1border,ftrack1color, gsvertical);
      //canvas.FillRect(rect(FTrackRect.Left, ftrackrect.Top, ftrackrect.right, tw));
      bmp.canvas.Brush.Color:= ftrack2border;
      bmp.canvas.FrameRect(rect(FTrackRect.Left, ftrackrect.Top, ftrackrect.right, tw));
      bmp.canvas.brush.color:= FTrack1Color;
      gradientfill(bmp.canvas,rect(ftrackrect.Left, tw, ftrackrect.right, ftrackrect.Bottom),ftrack2color,ftrack2border, gsvertical);
      //canvas.FillRect(rect(ftrackrect.Left, tw, ftrackrect.right, ftrackrect.Bottom));
      bmp.canvas.Brush.Color:= ftrack1border;
      bmp.canvas.FrameRect(rect(ftrackrect.left, tw, ftrackrect.right, ftrackrect.Bottom));
    end;
  end;

  procedure drawthumb( bmp: tbitmap );
  begin
    bmp.canvas.brush.color:= FThumbColor;
    bmp.canvas.FillRect( FThumbRect );
    bmp.canvas.brush.Color:= fthumbborder;
    bmp.canvas.FrameRect( fthumbrect );
  end;

begin
  updatethumbminmax;
  updateposition;
  SetThumbrect;
  SetTrackRect;

  fbmp.Height:= height;
  fbmp.Width:= width;
  drawbackground( fbmp );
  drawtrack( fbmp );
  drawthumb( fbmp );
  canvas.Draw(0,0,fbmp);       // draw bitmap to canvas
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssleft in shift) then
    if ptinRect(fhitrect, point(x,y)) then
    begin
      case forientation of
        TTO_horizontal: fthumbposition:=x;
        TTO_vertical: fthumbposition:=y;
      end;
      updatevalue;
      if assigned(fonchange) then
        fonchange(self, fvalue);
      invalidate;
    end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setmax(e: extended);
begin
  if (e>fminimum) then
  begin
    fmaximum:= e;
    if (ftrackmax>ftrackmin) then
    begin
      if (fmaximum>ftrackmax) then ftrackmax:= fmaximum;
      if (fvalue>ftrackmax) then fvalue:= ftrackmax;
    end else
      if fvalue>fmaximum then fvalue:= fmaximum;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setmin(e: extended);
begin
  if (e<fmaximum) then
  begin
    fminimum:= e;
    if (ftrackmax>ftrackmin) then
    begin
      if (ftrackmin<fminimum) then ftrackmin:= fminimum;
      if (fvalue<ftrackmin) then fvalue:= ftrackmin;
    end else
      if fvalue<fminimum then fvalue:= fminimum;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrack1color(c: tcolor);
begin
  if (c<>ftrack1color) then
  begin
    ftrack1color:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrack2color(c: tcolor);
begin
  if (c<>ftrack2color) then
  begin
    ftrack2color:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrack1border(c: tcolor);
begin
  if (c<>ftrack1border) then
  begin
    ftrack1border:= c;
    invalidatE;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.settrack2border(c: tcolor);
begin
  if (c<>ftrack2border) then
  begin
    ftrack2border:= c;
    invalidatE;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setvalue(e: extended);
begin
  if (e>=fminimum) and (e<=fmaximum) then
  begin
    fvalue:= e;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setbordercolor(c: tcolor);
begin
  if (c<>fbordercolor) then
  begin
    fbordercolor:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setbackgroundcolor(c: tcolor);
begin
  if (c<>fbackgroundcolor) then
  begin
    fbackgroundcolor:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setthumbcolor(c: tcolor);
begin
  if (c<>fthumbcolor) then
  begin
    fthumbcolor:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setthumbborder(c: tcolor);
begin
  if (c<>fthumbborder) then
  begin
    fthumbborder:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_trackbar.setorientation(o: TAPI_trackbar_orientation);
var
  tmp: integer;
begin
  if (o<>forientation) then
  begin
    forientation:= o;
    if (csDesigning in ComponentState) then
    begin
      tmp:=width;
      width:=height;
      height:=tmp;
      tmp:=FThumbWidth;
      FThumbWidth:=FThumbheight;
      FThumbHeight:=tmp;
    end;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_trackbar]);
end;

end.
