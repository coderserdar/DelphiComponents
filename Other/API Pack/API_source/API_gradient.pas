unit API_gradient;

//------------------------------------------------------------------------------
// gradient panel component
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
// 20052009, r1.07, ari pikivirta
//  * does not remove background anymore
//
// 02072008, r1.06, ari pikivirta
//  * start- and endcolor property names changed to colorstart and colorend
//
// 19062008, r1.05, ari pikivirta
//  * moved gradient filling function to api_graphics unit
//
// 15052008, r1.04, ari pikivirta
//  * color count passed to as parameter changed to -1
//
// 01022008, r1.03, ari pikivirta
//  * removed bmp creating fronm the paint event to make it faster
//  * added opaque to drawing style to avoid flickering
//
// 18082006, r1.02, ari pikivirta
//  * added autohide property
//  * added onmouseenter and leave events
//
// 25032006, r1.01, ari pikivirta
//  * moved gradientfill function to common to be able to point
//    to this function from other component's as well. trough this modification
//    also removed all other component's internall filling functions and
//    applied this one (no version change in those).
//  * added overloaded gradientfill function to automatically adjust amount
//    of color steps for the area size

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  graphics, forms, API_base, API_graphics;

type
  TAPI_gradient = class(TAPI_Custom_Panel)
  private
    fgradientstyle: tgradientstyle;
    fstartcolor: tcolor;
    fendcolor: tcolor;
    fnumofcolors: integer;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fautohide: boolean;
    ftimer: ttimer;
    fbmp: tbitmap;
    fallowhresize: boolean;
    foldwidth: integer;
    fallowvresize: boolean;
    foldheight: integer;
    procedure setstartcolor(c:tcolor);
    procedure setendcolor(c:tcolor);
    procedure setnumofcolors(i:integer);
    procedure setgradientstyle(i:tgradientstyle);
    procedure ontimer(sender: tobject);

  protected
    procedure Paint; override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Loaded; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  public
    constructor Create(AOwner:Tcomponent); override;
    destructor Destroy; override;

  published
    property ColorStart: tcolor read fstartcolor write setstartcolor;
    property ColorEnd: tcolor read fendcolor write setendcolor;
    property NumberofColors: integer read fnumofcolors write setnumofcolors;
    property GradientStyle: tgradientstyle read fgradientstyle write setgradientstyle;
    //property color: tcolor write setstartcolor;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property AutoHide: boolean read fautohide write fautohide;
    property ResizeHorizontal: boolean read fallowHresize write fallowHresize default FALSE;
    property ResizeVertical: boolean read fallowVresize write fallowVresize default FALSE;

  end;

procedure Register;

implementation

{$R *.RES}

const
  versioninfo = 'r1.07/ari.pikivirta@kolumbus.fi';

// -------------------------------------------------------
constructor TAPI_gradient.create(aowner:tcomponent);
begin
  inherited create(aowner);
  Version:= versioninfo;
  fstartcolor:=clblack;
  fendcolor:=clgray;
  fnumofcolors:=255;
  fgradientstyle:= gsHorizontal;
  fautohide:= false;
  ftimer:= ttimer.create(self);
  ftimer.Interval:= 100;
  ftimer.OnTimer:= OnTimer;
  ftimer.Enabled:= true;
  fbmp:= tbitmap.create;
  ControlStyle:= ControlStyle + [csOpaque];
end;

// -------------------------------------------------------
destructor TAPI_gradient.destroy;
begin
  ftimer.Enabled:= false;
  ftimer.Free;
  fbmp.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_gradient.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_gradient.Loaded;
begin
  // store old heigth and width
  foldheight:= height;
  foldwidth:= width;
  // hide automatically ?
  if (not (csdesigning in componentstate)) and
    (fautohide) and (visible) then visible:= false;
  inherited Loaded;
end;

// -------------------------------------------------------
procedure TAPI_gradient.setstartcolor(c: tcolor);
begin
  if c<>fstartcolor then
  begin
    color:= c; // set color same with start color
    fstartcolor:= c;
    invalidate;
  end;
end;

// -------------------------------------------------------
procedure TAPI_gradient.setendcolor(c:tcolor);
begin
  if c<>fendcolor then
  begin
    fendcolor:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_gradient.ontimer(sender: tobject);
var
  tl, sp, pt: tpoint;
begin
  if (fautohide) then
  begin
    tl.X:= 0;
    tl.y:= 0;
    sp:= ClientToScreen(tl);
    getcursorpos(Pt);

    if (pt.x>sp.x) and (pt.x<sp.x+width) and
      (pt.y>sp.y) and (pt.y<sp.y+height) then
    begin
      if not visible then visible:= true;
    end else
      if visible then visible:= false;

  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_gradient.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_gradient.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

// -------------------------------------------------------
(*
procedure TAPI_gradient.WMSize( var message: TWMSize );
var
  xRatio: real;
  i: integer;
  ctl: TWinControl;
begin
  if not (csDesigning in ComponentState) then //and (align = alNone)bResize then
  begin
    if fallowHresize then
    begin
      xRatio:= Width/fOldWidth;
      for i:= 0 to ControlCount - 1 do
      begin
        ctl:= TWinControl( Controls[i] );
        ctl.Left:= Round( ctl.Left * xRatio );
        ctl.Width:= Round( ctl.Width * xRatio );
      end;
    end;
    if fallowVresize then
    begin
      xRatio:= Height/fOldHeight;
      for i:= 0 to ControlCount - 1 do
      begin
        ctl:= TWinControl( Controls[i] );
        ctl.Top:= Round( ctl.Top * xRatio );
        ctl.Height:= Round( ctl.Height * xRatio );
      end;
    end;
  end else
  begin
    fOldWidth := Width;
    fOldHeight := Height;
  end;
  fOldWidth:= Width;
  fOldHeight:= Height;
end;
*)

// -------------------------------------------------------
procedure TAPI_gradient.setnumofcolors(i:integer);
begin
  if i<>fnumofcolors then
  begin
    fnumofcolors:=i;
    invalidate;
  end;
end;

// -------------------------------------------------------
procedure TAPI_gradient.setgradientstyle(i:tgradientstyle);
begin
  if fgradientstyle<>i then
  begin
    fgradientstyle:=i;
    invalidate;
  end;
end;

// -------------------------------------------------------
procedure TAPI_gradient.paint;
begin
  fbmp.Height:= height;
  fbmp.Width:= width;
  gradientfill(fbmp.canvas, fbmp.Canvas.ClipRect, fstartcolor, fendcolor, fgradientstyle);
  canvas.Draw(0,0,fbmp);
end;

// -------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_gradient]);
end;

end.
