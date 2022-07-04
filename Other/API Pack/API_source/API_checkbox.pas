unit API_checkbox;

//------------------------------------------------------------------------------
// api_checkbox
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta[at]kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.04, 23022009, ari pikivirta
//  * added check height property
//  * fixed bug on LSCheckBox drawing
//  * changed LsCheck and added LSCross led styles
//
// r1.03, 06052008, ari pikivirta
//  * removed autosize stuff, wasn't working nicely in use
//
// r1.02, 15022008, ari pikivirta
//  * added autosize property, true by default
//
// r1.01, 04012008, ari pikivirta
//  * changed tgraphicscontrol to tapi_custom_paintbox to have anchors etc.
//  * fixed bug on defining initial width (was zero)
//
// r1.00, 07022007, ari pikivirta
//  * created to have nicer looking checkbox with transparent background
//

interface

uses
  SysUtils, Classes, StdCtrls, Controls, Graphics, Messages,
  API_base;

type
  TLedStyle = (LSSquare, LSEllipse, LSBox, LSCheck, LSCheckBox, LSCross);
  TAPIOnMouseDown = procedure (sender: tcomponent; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TAPIOnMouseUp = procedure (sender: tcomponent; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TAPIOnClick = procedure (sender: tcomponent; Checked: Boolean) of object;

  TAPI_checkbox = class(TAPI_Custom_Paintbox)
  private
    { Private declarations }
    //fversion: string;
    fstyle: tledstyle;
    fwordwrap: boolean;
    falignment: TLeftRight;
    fcaption: string;
    ffont: tfont;
    ffont_mouseover: tfont;
    fchecked: boolean;
    fcheckcoloron: tcolor;
    fcheckcoloroff: tcolor;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fonmouseup: tAPIonmouseup;
    fonmousedown: tAPIonmousedown;
    fonclick: TAPIOnClick;
    fmouseover: boolean;
    FCheckheight: integer;
    procedure setstyle(l: tledstyle);
    procedure setwordwrap(b: boolean);
    procedure setalignment(l: tleftright);
    procedure setcaption(s: string);
    procedure setchecked(b: boolean);
    procedure setfont(f: tfont);
    procedure setmouseoverfont(f: tfont);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner: tcomponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    //property Version: string read fversion write dummys stored false;
    property Style: tledstyle read fstyle write setstyle;
    property Wordwrap: boolean read fwordwrap write setwordwrap;
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property Caption: string read fcaption write setcaption;
    property Checked: boolean read fchecked write setchecked;
    property Font: tfont read ffont write setfont;
    property FontMouseOver: tfont read ffont_mouseover write setmouseoverfont;
    property CheckColorOn: tcolor read fcheckcoloron write fcheckcoloron;
    property CheckColorOff: tcolor read fcheckcoloroff write fcheckcoloroff;
    property OnClick: tAPIOnClick read fonclick write fonclick;
    property OnMouseUp: tAPIonmouseup read fonmouseup write fonmouseup;
    property OnMouseDown: tAPIonmousedown read fonmousedown write fonmousedown;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property CheckHeight: integer read fcheckheight write fcheckheight;
  end;

procedure Register;

implementation

{$R *.RES}

uses
  Windows, forms;

//------------------------------------------------------------------------------
constructor TAPI_checkbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Version:= 'r1.04, ari.pikivirta[at]kolumbus.fi';
  ffont:= tfont.Create;
  ffont.Assign(tform(aowner).font);
  ffont_mouseover:= Tfont.create;
  ffont_mouseover.assign(ffont);
  fstyle:= lscheck;
  fchecked:= false;
  fcaption:= 'TAPI_checkbox';
  fwordwrap:= true;
  fcheckcoloron:= clgreen;
  fcheckcoloroff:= clred;
  fmouseover:= false;
  height:= 18;
  width:= 95;
  fcheckheight:= 10;
end;

//------------------------------------------------------------------------------
destructor TAPI_checkbox.Destroy;
begin
  ffont.Free;
  ffont_mouseover.free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setstyle(l: tledstyle);
begin
  if fstyle<>l then
  begin
    fstyle:= l;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setalignment(l: tleftright);
begin
  if l<>falignment then
  begin
    falignment:= l;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setwordwrap(b: Boolean);
begin
  if b<>fwordwrap then
  begin
    fwordwrap:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setcaption(s: string);
begin
  if s<>fcaption then
  begin
    fcaption:= s;
    repaint;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setfont(f: tfont);
begin
  if f<>ffont then
  begin
    ffont.Assign(f);
    repaint;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setmouseoverfont(f: tfont);
begin
  if f<>ffont_mouseover then
  begin
    ffont_mouseover.assign(f);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.setchecked(b: boolean);
begin
  if b<>fchecked then
  begin
    fchecked:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.Click;
begin
  fchecked:= not fchecked;
  if assigned(fonclick) then fonclick(self, fchecked);
  //inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(fonmousedown) then
    fonmousedown(self, button, shift, x, y);
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(fonmouseup) then
    fonmouseup(self, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
  if not fmouseover then
  begin
    fmouseover:= true;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
  if fmouseover then
  begin
    fmouseover:= false;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_checkbox.Paint;
var
  flags: cardinal;
  drawrect, temprect: trect;
  len, x1, y1, x2, y2, h, pw, pw3: integer;
begin
  len:= length(fcaption);

  // assign font
  if fmouseover then canvas.font.assign(ffont_mouseover)
    else canvas.Font.Assign(ffont);

  // get drawing area
  drawrect.Left:= 0;
  drawrect.Top:= 0;
  drawrect.Right:= width;
  drawrect.Bottom:= self.height;

  // draw led
  with canvas do
  begin

    // calculate rectangle for the led
    // to be drawn (led size is fixed
    // temporarily until have some time
    // to add that property)
    pw:= FCheckheight div 2; // check image size; todo: add as one of settable properties
    if alignment = taRightJustify then
    begin
      // right justify
      x1:= width - (pw*3); // reserve some space
      y1:= (self.height div 2) - pw;
      x2:= width - pw;
      y2:= (self.height div 2) + pw;
    end else
    begin
      // left justify
      x1:= pw;
      y1:= (self.height div 2) - pw;
      x2:= pw*3; // reserve some space
      y2:= (self.height div 2) + pw;
    end;
    // init temporary var for specials
    pw3:= FCheckheight div 3;

    // brush color according to led state
    if fchecked then canvas.Brush.Color:= fcheckcoloron
      else canvas.brush.color:= fcheckcoloroff;

    // actual drawing
    case FStyle of
      LSSquare:
        canvas.FillRect(rect(x1,y1,x2,y2));
      LSEllipse:
        canvas.Ellipse(x1,y1,x2,y2);
      LSBox:
        begin
          canvas.Pen.Color:= canvas.Brush.Color;
          canvas.pen.Width:= 2;
          canvas.MoveTo(x1,y1);
          canvas.LineTo(x1,y2);
          canvas.lineto(x2,y2);
          canvas.lineto(x2,y1);
          canvas.lineto(x1,y1);
        end;
      LSCross:
        begin
          canvas.Pen.Color:= canvas.Brush.Color;
          canvas.pen.Width:= 2;
          canvas.moveto(x1,y1);
          canvas.lineto(x2,y2);
          canvas.moveto(x1,y2);
          canvas.LineTo(x2,y1);
        end;
      LSCheck:
        begin
          if fchecked then
          begin
            // red background
            canvas.Pen.Color:= fcheckcoloroff;
            canvas.pen.Width:= 1;
            canvas.MoveTo(x1+pw3, y1+pw3);
            canvas.LineTo(x1+pw3, y2);
            canvas.lineto(x2, y2);
            canvas.lineto(x2, y1+pw3);
            canvas.lineto(x1+pw3, y1+pw3);
            // green check
            canvas.pen.color:= fcheckcoloron;
            canvas.pen.width:= 2;
            canvas.moveto(x1,y1);
            canvas.lineto(x2-pw3,y2);
            canvas.lineto(x2-pw3,y1);
          end else
          begin
            canvas.Pen.Color:= canvas.Brush.Color;
            canvas.pen.Width:= 1;
            canvas.MoveTo(x1,y1);
            canvas.LineTo(x1,y2);
            canvas.lineto(x2,y2);
            canvas.lineto(x2,y1);
            canvas.lineto(x1,y1);
          end;
        end;
      LSCheckBox:
        begin
          if fchecked then
          begin
            // red background
            canvas.Pen.Color:= fcheckcoloroff;
            canvas.pen.Width:= 1;
            canvas.MoveTo(x1, y1);
            canvas.LineTo(x1, y2);
            canvas.lineto(x2, y2);
            canvas.lineto(x2, y1);
            canvas.lineto(x1, y1);
            // green check
            canvas.pen.color:= fcheckcoloron;
            canvas.pen.width:= 2;
            canvas.moveto(x1,y1);
            canvas.lineto(x2,y2);
            canvas.moveto(x1,y2);
            canvas.lineto(x2,y1);
          end else
          begin
            canvas.Pen.Color:= canvas.Brush.Color;
            canvas.pen.Width:= 1;
            canvas.MoveTo(x1,y1);
            canvas.LineTo(x1,y2);
            canvas.lineto(x2,y2);
            canvas.lineto(x2,y1);
            canvas.lineto(x1,y1);
          end;
        end;
    end; // case
  end; // with canvas

  // define text drawing flags..
  flags:= DT_NOPREFIX;
  if fwordwrap then flags:= flags or DT_WORDBREAK;
  if alignment = taLeftJustify then flags:= flags or DT_LEFT;
  if alignment = taRightJustify then flags:= flags or DT_RIGHT;

  // horizontal alignment
  if alignment = taRightJustify then
  begin
    drawrect.left:= 0;
    drawrect.right:= drawrect.right - (pw*2 + FCheckHeight);
  end else
  begin
    drawrect.left:= pw*2 + FCheckHeight;
    drawrect.right:= width;
  end;

  // first calc actual needed space for the text in vertical
  temprect:= drawrect;
  flags:= flags + DT_CALCRECT;
  windows.DrawText(canvas.Handle, pchar(fcaption), Len, temprect, flags);
  flags:= flags - DT_CALCRECT;

  // vertical center
  h:= temprect.bottom-temprect.top;
  drawrect.Top:= (drawrect.Bottom-drawrect.Top) div 2 - h div 2;
  drawrect.Bottom:= drawrect.top + h;

  // write the actual text into area defined at the beginning of this drawing routine
  canvas.Brush.Style:= bsclear;
  windows.DrawText( canvas.Handle, pchar(fcaption), Len, drawrect, flags);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_checkbox]);
end;

end.
