unit API_statusbar;

//------------------------------------------------------------------------------
// statusbar that allows to place components on it
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
// r1.01/19082006/ari pikivirta
//  * added onmouseleave and enter events
//  * added autohide property
//

interface

uses
  Windows, SysUtils, Classes, Controls, ComCtrls, Types, Messages, Forms,
  ExtCtrls;

type
  TAPI_statusbar = class(TStatusBar)
  private
    fversion: string;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fautohide: boolean;
    ftimer: ttimer;
    procedure dummys(s: string);
    procedure ontimer(sender: tobject);

  protected
    procedure DrawPanel(panel: tstatuspanel; const rect: trect); override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Loaded; override;

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

  published
    property Version: string read fversion write dummys stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property AutoHide: boolean read fautohide write fautohide;

  end;

procedure Register;

implementation

{$r *.res}

const
  versioninfostring: string ='r1.02/ari.pikivirta@kolumbus.fi';

procedure TAPI_statusbar.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_statusbar.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfostring;
  parentfont:=false;
  ControlStyle := ControlStyle + [csAcceptsControls];
  fautohide:= false;
  ftimer:= ttimer.create(self);
  ftimer.Interval:= 100;
  ftimer.OnTimer:= OnTimer;
  ftimer.Enabled:= true;
end;

//------------------------------------------------------------------------------
destructor TAPI_statusbar.destroy;
begin
  ftimer.Enabled:= false;
  ftimer.Free;
  ftimer:= nil;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_statusbar.DrawPanel(panel: tstatuspanel; const rect: trect);
begin
  (*
  if self.Panels.Count>0 then
  begin
    if panel = self.Panels[0] then
    begin
      // what do we do with 1st panel =) ?
    end;
  end;

  R:= Rect;
  with canvas do
  begin
    Pen.Color   := clGreen;   // border color
    Brush.Color := clGreen;   // panel color
    Rectangle(Rect.Left+1, Rect.Top+1, Rect.Right-1, Rect.Bottom-1);
    drawtext(Handle, PChar(Panel.Text), -1, R, Alignments[Panel.Alignment]);
 end;
 *)
 inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_statusbar.Loaded;
begin
  if (not (csdesigning in componentstate)) and
    (fautohide) and (visible) then visible:= false;
  inherited Loaded;
end;

//------------------------------------------------------------------------------
procedure TAPI_statusbar.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_statusbar.ontimer(sender: tobject);
var
  tl, sp, pt: tpoint;
  QuarterHeight: integer;
begin
  if (fautohide) then
  begin
    QuarterHeight:= (height div 4);
    if QuarterHeight<1 then QuarterHeight:= 1;

    tl.X:= 0;
    tl.y:= 0;
    sp:= ClientToScreen(tl);
    getcursorpos(Pt);

    if visible then
    begin
      // immediately when off the panel, the statusbar is
      // set invisible
      if not ((pt.x>sp.x) and (pt.x<sp.x+width) and (pt.y>sp.y) and (pt.y<sp.y+height)) then
        visible:= false;
    end else
      // mouse must be at quarter way bottom of height to
      // get statusbar back visible (some hysteresis)
      if ((pt.x>sp.x) and (pt.x<sp.x+width) and (pt.y>sp.y+Height-QuarterHeight) and (pt.y<sp.y+Height)) then
        visible:= true;

  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_statusbar.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_statusbar]);
end;

end.
