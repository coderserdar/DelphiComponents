unit API_statebmp;

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
// r1.03, 05092008, ari pikivirta
//  * added property to enable internal coloring on only one neutral picture (none)
//
// r1.01, ari pikivirta
// * added onmouseneter and onmouselave properties
// * added change on click property
// * added none on mouse over (to show "none" picture when mouse is over)
//
// r1.02, ari pikivirta
// * added functions to work with booleans
// * assigned automatic invalidate to statebmp resize

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls,
  graphics;

type
  tsbstatetype = (sbUp, sbDown, sbNone);

  TAPI_statebmp = class(tpaintBox)
  private
    fversion: string;
    fstate: tsbstatetype;

    fpicup: tpicture;
    fpicdown: tpicture;
    fpicnone: tpicture; // neutral..

    fmouseover: boolean;
    fonmouseleave: tnotifyevent;
    fonmouseenter: tnotifyevent;
    fchangeonclick: boolean;
    fnoneonmouseover: boolean;
    fintcoloring: boolean;

    procedure setstate(n:tsbstatetype);
    procedure setpicup(p:tpicture);
    procedure setpicdown(p:tpicture);
    procedure setpicnone(p:tpicture);
    procedure dummys(s: string);
    procedure dummyb(b: boolean);
    procedure setintcoloring(b: boolean);

  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure Resize; override;

  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    procedure SetAsBoolean(b: boolean);
    function GetAsBoolean: boolean;

  published
    property Version: string read fversion write dummys stored false;
    property State: tsbstatetype read fstate write setstate;
    property PictureUp: tpicture read fpicup write setpicup;
    property PictureDown: tpicture read fpicdown write setpicdown;
    property PictureNone: tpicture read fpicnone write setpicnone;
    property ChangeOnClick: boolean read fchangeonclick write fchangeonclick;
    property NoneOnMouseOver: boolean read fnoneonmouseover write fnoneonmouseover;
    property MouseOver: boolean read fmouseover write dummyb stored false;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property IntColoring: boolean read fintcoloring write setintcoloring;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  api_graphics;

const
  versioninfo = 'r1.03/ari.pikivirta@kolumbus.fi';

procedure TAPI_statebmp.dummys(s: string); begin end;
procedure TAPI_statebmp.dummyb(b: boolean); begin end;

//------------------------------------------------------------------------------
constructor tAPI_statebmp.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:= versioninfo;
  fpicup:= tpicture.Create;
  fpicdown:= tpicture.Create;
  fpicnone:= tpicture.create;
  fchangeonclick:= false;
  fnoneonmouseover:= false;
  fstate:= sbNone;
  fintcoloring:= false;
end;

//------------------------------------------------------------------------------
destructor tAPI_statebmp.destroy;
begin
  fpicup.free;
  fpicdown.free;
  fpicnone.free;
  fpicup:= nil;
  fpicdown:= nil;
  fpicnone:= nil;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.SetAsBoolean(b: boolean);
begin
  if (b) and (fstate<>sbup) then
  begin
    fstate:=sbup;
    invalidate;
  end else
  if (not b) and (fstate<>sbdown) then
  begin
    fstate:=sbdown;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_statebmp.GetAsBoolean: boolean;
begin
  result:=(fstate=sbup);
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.Resize;
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_statebmp.setstate(n:tsbstatetype);
begin
  if n<>fstate then
  begin
    fstate:=n;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.setintcoloring(b: boolean);
begin
  (*
  if (b) and (
    (fpicdown <> NIL) or (fpicup <> NIL)
    ) then
  begin
    fintcoloring:= false;
  end else
  *)
  fintcoloring:= b; // apply value
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_statebmp.setpicup(p:tpicture);
begin
  fpicup.Bitmap.Assign(p.Graphic);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_statebmp.setpicdown(p:tpicture);
begin
  fpicdown.Bitmap.Assign(p.Graphic);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_statebmp.setpicnone(p:tpicture);
begin
  fpicnone.Bitmap.Assign(p.Graphic);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_statebmp.paint;
var
  oldstate: tsbstatetype;
  bmp: tbitmap;
begin
  // check if none on mouseover
  oldstate:=fstate;
  if (fnoneonmouseover) and (fmouseover) then
    fstate:=sbnone;

  // create temporary bitmap
  bmp:= tbitmap.create;
  try
    bmp.Height:= height;
    bmp.Width:= width;

    // draw as it would be empty
    bmp.canvas.Brush.Color:= clsilver;
    bmp.canvas.FillRect(bmp.canvas.ClipRect);
    bmp.canvas.brush.Color:= clyellow;
    bmp.canvas.FrameRect(bmp.canvas.ClipRect);

    // draw picture to canvas
    bmp.canvas.CopyMode := cmSrcCopy;
    case fstate of
    sbUp:
      if (fintcoloring) and (assigned(fpicnone)) then
      begin
        // lighten neutral picture
        bmp.Canvas.StretchDraw(bmp.Canvas.ClipRect, fpicnone.Bitmap);
        api_graphics.Highlight(bmp, bmp.Canvas.ClipRect, 128-64);
      end else
      if assigned(fpicup) then
      begin
        //bmp.canvas.CopyRect(bmp.Canvas.ClipRect, fpicup.Bitmap.Canvas, fpicup.Bitmap.Canvas.ClipRect);
        bmp.Canvas.StretchDraw(bmp.canvas.cliprect, fpicup.bitmap);
      end;
    sbDown:
      if (fintcoloring) and (assigned(fpicnone)) then
      begin
        // darken neutral picture
        bmp.Canvas.StretchDraw(bmp.Canvas.ClipRect, fpicnone.Bitmap);
        api_graphics.Highlight(bmp, bmp.Canvas.ClipRect, 128+64);
      end else
      if assigned(fpicdown) then
      begin
        //bmp.canvas.CopyRect(bmp.Canvas.ClipRect, fpicdown.Bitmap.Canvas, fpicdown.Bitmap.Canvas.ClipRect)
        bmp.Canvas.StretchDraw(bmp.canvas.cliprect, fpicdown.bitmap);
      end;
    sbNone:
      if assigned(fpicnone) then
      begin
        //bmp.canvas.CopyRect(bmp.Canvas.ClipRect, fpicnone.Bitmap.Canvas, fpicnone.Bitmap.Canvas.ClipRect);
        bmp.Canvas.StretchDraw(bmp.canvas.cliprect, fpicnone.bitmap);
      end;
    end;

    // copy temporary bitmap onto canvas
    canvas.Draw(0,0,bmp);

  finally
    bmp.free;
  end;

  // return old state
  if (fnoneonmouseover) and (fmouseover) then
    fstate:=oldstate;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (button=mbleft) and (fchangeonclick) then
  begin
    if fstate=sbdown then fstate:=sbup
      else fstate:=sbdown;
    invalidate;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.MouseEnter(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseover:=true;
    invalidate;
    if assigned(fonmouseenter) then fonmouseenter(self);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.MouseLeave(var Message: TMessage);
begin
  if not (csdesigning in componentstate) then
  begin
    fmouseover:=false;
    invalidate;
    if assigned(fonmouseleave) then fonmouseleave(self);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_statebmp.Click;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_statebmp]);
end;

end.
