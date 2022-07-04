unit API_progressbar;

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
// 1.10, 03092009, ari pikivirta
//  * fixed bug on drawing step lines
//
// 1.09, 20052009, ari pikivirta
//  * does not erase background anymore -> reduces some flicker
//
// 1.08, 02072008, ari pikivirta
//  * property names changed so that all colors are now next to each other in
//    the property editor
//
// 1.07, 01022008, ari pikivirta
//  * bitmap creation removed from the paint event
//
// 1.05, 13082007, ari pikivirta
//  * added steps property to allow using this also as step-bar
//  * changed showtext to ValueAsText for better understanding
//
// 1.04, 09052007, ari pikivirta
//  * changed class type to paintbox (less predefined properties)
//  * default position set to zero, same time allows having this set to zero
//    from object properties < wasn't possible otherwise for some reason
//
// 1.03, 12082004, ari pikivirta
// * added border color property
// * added boder width property
// * removed the fixed position in design mode
// * removed center text property (now using original one)
// * also using caption text, if not showing value
//
// 1.02, ari pikivirta
// * changed progress property to position
// * fixed memory leak in temp bmp in draw function

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  graphics, API_base, API_graphics;

type
  TAPI_progressbar = class(TAPI_Custom_Paintbox)
  private
    fcaption: string;
    fprogress: extended;
    falignment: talignment;
    fmin: extended;
    fmax: extended;
    faddtext: string;
    fbackground: tcolor;
    fbordercolor: tcolor;
    fborderwidth: integer;
    fstartcolor: tcolor;
    fvertical: boolean;
    fendcolor: tcolor;
    fvalueastext: boolean;
    fsteps: integer;
    fbmp: tbitmap;
    procedure setmax(d:extended);
    procedure setmin(d:extended);
    procedure setprogress(d:extended);
    procedure setbackground(c:tcolor);
    procedure setstartcolor(c:tcolor);
    procedure setendcolor(c:tcolor);
    procedure setaddtext(s:string);
    procedure setvalueastext(b:boolean);
    procedure setvertical(b:boolean);
    procedure setbordercolor(c: tcolor);
    procedure setborderwidth(w: integer);
    procedure setcaption(s: string);
    procedure setalignment(a: talignment);
    procedure setsteps(i: integer);

  protected
    procedure Paint; override;
    procedure Resize; override;

  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  published
    property Min: extended read fmin write setmin;
    property Max: extended read fmax write setmax;
    property Position: extended read fprogress write setprogress;
    property ColorBack: tcolor read fbackground write setbackground;
    property ColorStart: tcolor read fstartcolor write setstartcolor;
    property ColorEnd: tcolor read fendcolor write setendcolor;
    property ValueAsText: boolean read fvalueastext write setvalueastext;
    property AddToText: string read faddtext write setaddtext;
    property Vertical: boolean read fvertical write setvertical;
    property BorderWidth: integer read fborderwidth write setborderwidth;
    property ColorBorder: tcolor read fbordercolor write setbordercolor;
    property Alignment: talignment read falignment write setalignment;
    property Caption: string read fcaption write setcaption;
    property Steps: integer read fsteps write setsteps;

  end;

procedure Register;

implementation

const
  versioninfo = 'r1.10/ari.pikivirta}at{kolumbus.fi';

{$r *.res}

//------------------------------------------------------------------------------
constructor tAPI_progressbar.create(aowner:tcomponent);
begin
  inherited create(aowner);
  height:=        22;
  width:=         150;
  font.Color:=    clwhite;
  version:=       versioninfo;
  fstartcolor:=   cllime;
  fendcolor:=     clred;
  fbackground:=   clblack;
  fvertical:=     false;
  fbordercolor:=  clgray;
  fborderwidth:=  1;
  fvalueastext:=  false; // show caption..
  faddtext:=      '%';
  fmin:=          0;
  fmax:=          100.0;
  fsteps:=        0;
  fprogress:=     0;
  alignment:=     taCenter;
  fbmp:=          tbitmap.create;
  ControlStyle:=  ControlStyle + [csOpaque];
end;

//------------------------------------------------------------------------------
destructor tAPI_progressbar.destroy;
begin
  fbmp.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.setsteps(i: integer);
begin
  if fsteps<>i then
  begin
    fsteps:= i;
    if fsteps<0 then fsteps:= 0;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.setbordercolor(c: tcolor);
begin
  if c<>fbordercolor then
  begin
    fbordercolor:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.setborderwidth(w: integer);
begin
  if w<>fborderwidth then
  begin
    fborderwidth:= w;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.setalignment(a: talignment);
begin
  falignment:= a;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.setcaption(s: string);
begin
  fcaption:= s;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_progressbar.Resize;
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.paint;
var
  rec: trect;
  f: extended;
  text: string;
  i, t: integer;
begin

  // do some checking
  if (fmax-fmin<>0) and (fprogress>fmin) then
  begin
    // calculate position as percentage
    f:=fprogress/abs(fmax-fmin);
    // if there is some steps defined,
    // we need to round the f (position as percentage)
    // to the range and as a whole step
    if fsteps>0 then f:= round(f*fsteps)/fsteps;
  end else
    // position is zero in case not possible
    // to calculate
    f:=0;

  if fborderwidth<0 then
    fborderwidth:=0;

  // create bitmap
  fbmp.Height:= height; //clientheight;
  fbmp.Width:= width; //clientwidth;

  // set drawing styles
  fbmp.canvas.pen.style:= pssolid;
  fbmp.canvas.pen.mode:= pmcopy;
  rec:= fbmp.Canvas.ClipRect;

  // horizontal filling
  if not FVertical then
  begin
    gradientfill(fbmp.canvas, rec, fstartcolor, fendcolor, gshorizontal);
  end else
  // vertical filling
  begin
    gradientfill(fbmp.canvas, rec, fendcolor, fstartcolor, gsvertical);
  end;

  // fill background
  fbmp.canvas.Brush.Color:=fbackground;
  rec:= fbmp.canvas.ClipRect;
  if not fvertical then rec.Left:=round(f*fbmp.Width)
    else rec.bottom:=round((1-f)*fbmp.height);
  fbmp.canvas.FillRect(rec);

  // fill steps
  if (fborderwidth>0) and (fsteps>0) then
  begin
    fbmp.canvas.pen.Style:=psSolid;
    fbmp.canvas.pen.Width:=fborderwidth;
    fbmp.canvas.Pen.Color:=fbordercolor;
    for i:=1 to fsteps-1 do
    begin
      if not fvertical then
      begin
        fbmp.canvas.MoveTo(round(width/fsteps*i),fborderwidth);
        fbmp.canvas.lineto(round(width/fsteps*i),height-fborderwidth);
      end else
      begin
        fbmp.canvas.MoveTo(fborderwidth, round(height/fsteps*i));
        fbmp.canvas.lineto(width-fborderwidth, round(height/fsteps*i));
      end;
    end;
  end;

  // draw borders
  if fborderwidth>0 then
  begin
    fbmp.Canvas.Brush.Style:=bsClear;
    fbmp.canvas.Pen.Color:=fbordercolor;
    fbmp.canvas.pen.Style:=psSolid;
    fbmp.canvas.pen.Width:=fborderwidth;
    fbmp.Canvas.Rectangle(
      fbmp.canvas.ClipRect.Left+(fborderwidth div 2),
      fbmp.canvas.cliprect.Top+(fborderwidth div 2),
      fbmp.canvas.cliprect.Right-(fborderwidth div 2)+1,
      fbmp.canvas.cliprect.Bottom-(fborderwidth div 2)+1
    );
  end;

  // in case show as text is enabled, we'll draw the
  // current position as a text on the component,
  // otherwise it will be the caption to draw (more customizable also)
  rec:= fbmp.Canvas.ClipRect; //.cliprect;
  fbmp.canvas.brush.Style:= bsclear;
  fbmp.canvas.Font.Assign( self.Font );
  if fvalueastext then
  begin
    // draw text
    text:= formatfloat('0.00',fprogress)+fAddText;
    i:= (fbmp.height div 2) - (fbmp.canvas.TextHeight(text) div 2);
  end else
  begin
    // draw caption
    text:= caption;
    i:= (fbmp.height div 2) - (fbmp.canvas.TextHeight(text) div 2);
  end;
  case alignment of
    taCenter:       t:=(fbmp.Width div 2) - (fbmp.canvas.TextWidth(text) div 2);
    taRightJustify: t:=fbmp.Width-borderwidth-2-fbmp.Canvas.textwidth(text);
    else //taLeftJustify:
      t:=fborderwidth+2;
  end;
  fbmp.canvas.TextOut(rec.Left+t,rec.Top+i,text);

  // copy to canvas
  canvas.Draw(0,0,fbmp);
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setprogress(d:extended);
begin
  fprogress:=d;
  if fprogress>fmax then fprogress:=fmax;
  if fprogress<fmin then fprogress:=fmin;
  repaint;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setmax(d:extended);
begin
  if d<>fmax then
  begin
    fmax:=d;
    if fmin>fmax then fmin:= fmax;
    if fprogress>fmax then fprogress:=fmax;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setstartcolor(c:tcolor);
begin
  if c<>fstartcolor then
  begin
    fstartcolor:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setvertical(b:boolean);
begin
  if b<>fvertical then
  begin
    fvertical:=b;
    repaint;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setendcolor(c:tcolor);
begin
  if c<>fendcolor then
  begin
    fendcolor:=c;
    repaint;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setmin(d:extended);
begin
  if d<>fmin then
  begin
    fmin:=d;
    if fmax<fmin then fmax:= fmin;
    if fprogress<fmin then fprogress:=fmin;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setvalueastext(b:boolean);
begin
  if b<>fvalueastext then
  begin
    fvalueastext:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setbackground(c:tcolor);
begin
  if c<>fbackground then
  begin
    fbackground:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_progressbar.setaddtext(s:String);
begin
  if s<>faddtext then
  begin
    faddtext:=s;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_progressbar]);
end;

end.
