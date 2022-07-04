unit API_grbutton;

//------------------------------------------------------------------------------
// button component with many features: different coloring when mouse is over
// the button, led possibility (then works like checkbox), etc.
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
// 03072009, r1.12, ari pikivirta
//  * fixed bug on setting regions on designing mode
//  * borderstyle marked readonly into the property editor
//  * region formatting also works on resising component on designtime
//
// 02072009, r1.11, ari pikivirta
//  * added disabled color (when enabled is set to false)
//  * implemented check options from the api_checkbox
//  * fixed bug on mouse down event
//  * added elliptic and corner radius properties
//
// 20062009, r1.10, ari pikivirta
//  * does not remove background anymore
//
// 19062008, r1.09, ari pikivirta
//  * added gradient style property
//  * added bitmap buffer to make drawing smoother
//
// 07052008, r1.08, ari pikivirta
//  * fixed bug mousedown and mouseup events
//
// 01042006, r1.07, ari pikivirta
//  * added linkerto property to allow button to follow some other
//    control on the from if it's moved
//  * fixed the text drawing function to not to draw onto the led and
//    also to have the vertical alignment correctly
//
// 20032006, r1.06, ari pikivirta
//  * added lsbox and lscheck style for the led
//
// 19092005, r1.05, ari pikivirta
//  * added gradient coloring
//
// 12102004, r1.04, ari pikivirta
// * added main font synchronization possibility through property
//
// 25082004, r1.03, ari pikivirta
// * fixed border width property use
// * fixed client area measurement (now text doesn't get garbage)
//
// r1.02, ari pikivirta (04082004)
// * changed mouse over caption color to mouse over font
// * simplified the whole structure a lot
// * removed unnecessary redraws if some property was changed
// * added word wrap function for caption
// * added vertical alignment for caption
//
// r1.01, ari pikivirta
// * added led to the button, visible if set so
// * added alignment for the text
// * removed lots of unnecessary lines
//
// r1.00, ari pikivirta

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, graphics,
  extctrls, API_graphics, Dialogs, API_base, Forms;

type
  tledposition = (lpLeft, lpRight);
  TLedStyle = (lsSquare, lsEllipse, lsBox, lsCheck, lsCheckBox, lsCross);
  TVerticalAlignment = (taVerticalCenter, taAlignTop, taAlignBottom);

  TAPI_grbutton = class(TAPI_Custom_Panel)
  private
    Bmp: TBitmap;
    fcaption: string;
    fwordwrap: boolean;
    fvalign: TVerticalAlignment;
    fcolor: TColor;
    fborder: TColor;
    fmouseoverfont: tfont;
    fovercolor: TColor;
    fdowncolor: TColor;
    fgradientend: tcolor;
    fflipgradient: boolean;
    //fmousedownevent: TMouseEvent;
    //fmouseupevent: TMouseEvent;
    fmouseenterevent: TNotifyEvent;
    fmouseleaveevent: TNotifyEvent;
    fenabled: Boolean;
    fshowcaption: boolean;
    fdesigning: boolean;
    fmouseover: boolean;
    fmousedown: boolean;
    fLedExists: boolean;
    fLedColorOn: tcolor;
    fLedColorOff: tcolor;
    fLedState: boolean;
    fLedPos: tledposition;
    fLedStyle: tledstyle;
    fLedChangeOnClick: boolean;
    fcontrol: twincontrol;
    fobjectinstance: pointer;
    foldwindowproc: pointer;
    fposleft, fpostop: integer;
    fgradientstyle: tgradientstyle;
    fbuffer: tbitmap;
    FCheckheight: integer;
    fdisabledcolor: tcolor;
    felliptic: boolean;
    fradius: integer;
    fRgn: HRGN;
    fdummyborderstyle: TBorderStyle;

    procedure setcolor (Const value: TColor);
    procedure SetBorder(Const Value: TColor);
    procedure setcaption (Const value: string);
    procedure SetBmp(Value: TBitmap);
    procedure setshowcaption(Const value: boolean);
    procedure drawpicture(ACanvas: TCanvas; Const Arect: Trect );
    procedure drawtext(ACanvas: TCanvas; Const Arect: Trect );
    procedure drawled(ACanvas: TCanvas; Const Arect: Trect );
    procedure setledexists(Const b: boolean);
    procedure setledstate(Const b: boolean);
    procedure setledcoloron(Const c: tcolor);
    procedure setledcoloroff(Const c: tcolor);
    procedure setledstyle(Const s: tledstyle);
    procedure setledposition(Const p: tledposition);
    procedure setledchangeonclick(Const b: boolean);
    procedure setmouseoverfont(Const f: tfont);
    procedure setwordwrap(Const b: boolean);
    procedure setgradientend(Const c: tcolor);
    procedure ControlWindowProc(var msg: TMessage);
    procedure SetControl(AValue : TWinControl);
    procedure SetValign(value: TVerticalAlignment);
    procedure SetGradientStyle(gs: tgradientstyle);
    procedure SetRegion;
    procedure DeleteRegion;
    procedure SetRadius(Const NewValue: Integer);
    procedure SetElliptic(Const MakeItElliptic: Boolean);

  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure SetParent(Value: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure MoveToPos; virtual;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Settings(s: string); overload;
    function Settings: string; overload;

  published
    property LinkedTo: twincontrol read fcontrol write setcontrol;
    property Caption : string read fcaption write setcaption;
    property Enabled : Boolean read fenabled write Fenabled;
    property OnMouseEnter: TNotifyEvent read fmouseenterevent write fmouseenterevent;
    property OnMouseLeave: TNotifyEvent read fmouseleaveevent write fmouseleaveevent;
    property Glyph: TBitmap read Bmp write SetBmp;
    property FontMouseOver: tfont read fmouseoverfont write setmouseoverfont;

    property LedExists: boolean read fledexists write setledexists;
    property LedState: boolean read fledstate write setledstate;
    property LedChangeOnClick: boolean read fledchangeonclick write setledchangeonclick;
    property LedStyle: tledstyle read fledstyle write setledstyle;
    property LedPosition: tledposition read fledpos write setledposition;
    property LedColorOn: tcolor read fledcoloron write setledcoloron;
    property LedColorOff: tcolor read fledcoloroff write setledcoloroff;
    property LedHeight: integer read fcheckheight write fcheckheight;

    property Color: TColor read fcolor write setcolor;
    property ColorOver: TColor read fovercolor write fovercolor;
    property ColorDown: TColor read fdowncolor write fdowncolor;
    property ColorDisabled: TColor read fdisabledcolor write fdisabledcolor;
    property GradientEnd: tcolor read fgradientend write setgradientend;
    property GradientStyle: tgradientstyle read fgradientstyle write SetGradientStyle;
    property GradinetFlip: boolean read fflipgradient write fflipgradient;

    property BorderWidth;
    property ColorBorder: TColor read fborder write setborder;
    property Elliptic: boolean read felliptic write SetElliptic;
    property CornerRadius: integer read fradius write SetRadius;

    property ShowCaption: Boolean read fshowcaption write setshowcaption;
    property WordWrap: boolean read fwordwrap write setwordwrap;
    property ShowHint;
    property ParentShowHint;
    property OnMouseMove;
    property Font;
    property Alignment;
    property VerticalAlignment: TVerticalAlignment read fvalign write setvalign;

    property BorderStyle: TBorderStyle read fdummyborderstyle; // hide borderstyle property
  end;

procedure Register;

implementation

const
  versioninfo = 'r1.12/ari.pikivirta-at-kolumbus.fi';

{$R *.RES}

//------------------------------------------------------------------------------
constructor tAPI_grbutton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  version:= versioninfo;

  Alignment:= taCenter;

  Width:= 80;
  Height:= 22;
  borderwidth:= 1;
  Bmp:= TBitmap.Create;
  fmouseoverfont:= tfont.create;

  fdesigning := (csdesigning in componentstate);
  color:= clBtnFace;

  fmouseoverfont.Assign( font );
  colorover:= clsilver;
  wordwrap:= FALSE;
  fflipgradient:= FALSE;
  colordown:= clgray;
  fborder:= clwhite;  //
  gradientend:= clwhite;
  gradientstyle:= gsVertical;
  fenabled:= TRUE;
  fshowcaption:= TRUE;
  fmousedown:= FALSE;
  fmouseover:= FALSE;
  fledstate:= FALSE;
  fledchangeonclick:= FALSE;
  fledcoloron:= clgreen;
  fledcoloroff:= clred;
  fledexists:= FALSE;
  fledstyle:= lsellipse;
  fledpos:= lpright;
  fcheckheight:= 8;
  fdisabledcolor:= clgray;
  FObjectInstance := classes.MakeObjectInstance(ControlWindowProc);
  fpostop:= 0;
  fposleft:= 0;
  felliptic:= FALSE;
  fradius:= 5;

  // temp bufer for drawing
  fbuffer:= tbitmap.create;
  fbuffer.PixelFormat:= pf24bit;
end;

//------------------------------------------------------------------------------
destructor TAPI_grbutton.destroy;
begin
  if Assigned(FControl) then SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
  deleteregion; // make sure region is also freed
  classes.FreeObjectInstance(FObjectInstance);
  FObjectInstance := nil;
  bmp.free;
  fbuffer.free;
  fmouseoverfont.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // do not erase background..
  //inherited;
  message.result:= 1;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.Settings(s: string);
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.text:= s;
    ledexists:= strtobool(sl.Values['ledexists']);
    ledchangeonclick:= strtobool(sl.values['ledchangeonclick']);
    ledcoloron:= strtoint(sl.values['ledcoloron']);
    ledstyle:= tledstyle(strtoint(sl.values['ledstyle']));
    ledposition:= tledposition(strtoint(sl.values['ledposition']));
    ledcoloroff:= strtoint(sl.values['ledcoloroff']);
    color:= strtoint(sl.values['color']);
    colorborder:= strtoint(sl.values['colorborder']);
    gradientend:= strtoint(sl.values['gradientend']);
    enabled:= strtobool(sl.values['enabled']);
    fontmouseover.Name:= sl.values['fontmouseover_name'];
    fontmouseover.Size:= strtoint(sl.values['fontmouseover_size']);
    fontmouseover.Color:= strtoint(sl.values['fontmouseover_color']);
    fontmouseover.style:= tfontstyles(tfontstyle(strtoint(sl.values['fontmouseover_style'])));
    colorover:= strtoint(sl.values['colorover']);
    colordown:= strtoint(sl.values['colordown']);
    showcaption:= strtobool(sl.values['showcaption']);
    wordwrap:= strtobool(sl.values['wordwrap']);
    borderwidth:= strtoint(sl.values['borderwidth']);
    showhint:= strtobool(sl.values['showhint']);
    font.Name:= sl.values['font_name'];
    font.size:= strtoint(sl.values['font_size']);
    font.Color:= strtoint(sl.values['font_color']);
    font.style:= tfontstyles(tfontstyle(strtoint(sl.values['font_style'])));
    alignment:= talignment(strtoint(sl.values['alignment']));
    verticalalignment:= tverticalalignment(strtoint(sl.values['verticalalignment']));
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_grbutton.Settings: string;
var
  sl: tstringlist;
begin
  sl:= tstringlist.create;
  try
    sl.clear;
    sl.add('ledexists='+booltostr(ledexists));
    sl.add('ledchangeonclick='+booltostr(ledchangeonclick));
    sl.add('ledcoloron='+inttostr(ledcoloron));
    sl.add('ledstyle='+inttostr(ord(ledstyle)));
    sl.add('ledposition='+inttostr(ord(ledposition)));
    sl.add('ledcoloroff='+inttostr(ledcoloroff));
    sl.add('color='+inttostr(color));
    sl.add('colorborder='+inttostr(colorborder));
    sl.add('gradientend='+inttostr(gradientend));
    sl.add('enabled='+booltostr(enabled));
    sl.add('fontmouseover_name='+fontmouseover.Name);
    sl.add('fontmouseover_size='+inttostr(fontmouseover.Size));
    sl.add('fontmouseover_color='+inttostr(fontmouseover.color));
    sl.add('fontmouseover_style='+inttostr(ord(tfontstyle(fontmouseover.style))));
    sl.add('colorover='+inttostr(colorover));
    sl.add('colordown='+inttostr(colordown));
    sl.add('showcaption='+booltostr(showcaption));
    sl.add('wordwrap='+booltostr(wordwrap));
    sl.add('borderwidth='+inttostr(borderwidth));
    sl.add('showhint='+booltostr(showhint));
    sl.add('font_name='+font.name);
    sl.add('font_size='+inttostr(font.size));
    sl.add('font_color='+inttostr(font.color));
    sl.add('font_style='+inttostr(ord(tfontstyle(font.style))));
    sl.add('alignment='+inttostr(ord(alignment)));
    sl.add('verticalalignment='+inttostr(ord(verticalalignment)));
    result:= sl.text;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetGradientStyle(gs: tgradientstyle);
begin
  if gs<>fgradientstyle then
  begin
    fgradientstyle:= gs;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetRegion;
begin
  // create region
  If (frgn=0) then
  begin
    if felliptic then
      frgn:= CreateRoundRectRgn(0, 0, width+1, height+1, fradius, fradius)
      else frgn:= CreateRectRgn(0, 0, width, height);
  end;
  // Now, set the RgnAll as what we see for the Window
  if (frgn<>0) then
  begin
    (*
      From SetWindowRgn in the help file:
       "After a successful call to SetWindowRgn,
       the operating system owns the region specified
       by the region handle hRgn. The operating system
       does not make a copy of the region. Thus, you
       should not make any further function calls with
       this region handle. In particular, do not close
       this region handle."

       So don't call DeleteObject on RgnAll after using
       it for SetWindowRgn (thanks to Richard Albury for
       pointing this out!) A previous version of this article
       made this mistake.
    *)
    SetWindowRgn(Handle, frgn, TRUE); // << repaint
  end;
end;

procedure TAPI_grbutton.DeleteRegion;
begin
  If frgn=0 Then Exit;
  DeleteObject(frgn);
  frgn:= 0;
End;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledexists(Const b: boolean);
begin
  if b<>fledexists then
  begin
    fledexists:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetValign(value: TVerticalAlignment);
begin
  if value<>fvalign then
  begin
    fvalign:= value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure Tapi_grbutton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if ((Operation = opRemove)  and (AComponent = FControl)) then FControl := nil;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.ControlWindowProc(var msg: TMessage);
begin
  if msg.Msg = WM_ENABLE then Enabled := FControl.Enabled;
  if msg.Msg = WM_MOVE then   MoveToPos;
  if msg.Msg = WM_SIZE then   MoveToPos;
  if msg.Msg = WM_DESTROY then SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
  msg.result := CallWindowProc (fOldWindowProc, TWinControl(FControl).Handle, msg.msg, msg.wParam, msg.lParam)
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetControl(AValue: TWinControl);
begin
  if AValue <> FControl then
  begin
    if Assigned(FControl) then
      SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, integer(FOldWindowProc));
    FControl := AValue;
    if Assigned(FControl) then
    begin
      FControl.FreeNotification(Self);
      FOldWindowProc := TfnWndProc (SetWindowLong (TWinControl(FControl).Handle, GWL_WNDPROC, Integer (FObjectInstance)));
      // mark position difference
      fposleft:= left - fcontrol.left;
      fpostop:= top - fcontrol.top;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.Resize;
begin
  DeleteRegion;

  inherited;

  SetRegion;

  if Assigned(FControl) then
  begin
    fpostop:= top - fcontrol.top;
    fposleft:= left - fcontrol.left;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.MoveToPos;
begin
  if Assigned(FControl) then
  begin
    setbounds( fcontrol.left + fposleft, fcontrol.top + fpostop, width, height );
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.SetParent(Value: TWinControl);
begin
  inherited;
  if Value <> nil then fcaption:= Name;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setwordwrap(Const b: boolean);
begin
  if b<>fwordwrap then
  begin
    fwordwrap:= b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setgradientend(Const c: tcolor);
begin
  if c<>fgradientend then
  begin
    fgradientend:= c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setmouseoverfont(Const f: tfont);
begin
  if f<>fmouseoverfont then
  begin
    fmouseoverfont.Assign( f );
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledstate(Const b: boolean);
begin
  if b<>fledstate then
  begin
    fledstate:=b;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledcoloron(Const c: tcolor);
begin
  if c<>fledcoloron then
  begin
    fledcoloron:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledcoloroff(Const c: tcolor);
begin
  if c<>fledcoloroff then
  begin
    fledcoloroff:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledchangeonclick(Const b: boolean);
begin
  fledchangeonclick:=b;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledposition(Const p: tledposition);
begin
  if fledpos<>p then
  begin
    fledpos:=p;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.setledstyle(Const s: tledstyle);
begin
  if fledstyle<>s then
  begin
    fledstyle:=s;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.drawtext(ACanvas: TCanvas; Const ARect: Trect);
const
  DEFEMPTYSPACE = 2;
var
  len: integer;
  temprect, drawrect: trect;
  flags: cardinal;
  w,h{,p}: integer;
  {ts, s: string;}
begin
  // create rect for the text, check if led exists
  // and remove that area from possible text area
  // to allow led to be drawn as whole.
  if fledexists then
  begin
    if fledpos = lpleft then
    begin
      drawrect:= rect(
        (borderwidth+DEFEMPTYSPACE) + fcheckheight,
        (borderwidth+DEFEMPTYSPACE),
        width - (borderwidth+DEFEMPTYSPACE),
        height - (borderwidth+DEFEMPTYSPACE)
        );
    end else
    begin
      drawrect:= rect(
        borderwidth+DEFEMPTYSPACE,
        borderwidth+DEFEMPTYSPACE,
        width - (borderwidth+DEFEMPTYSPACE) - fcheckheight,
        height - (borderwidth+DEFEMPTYSPACE)
        );
    end;
  end else
  begin
    drawrect:= rect(
      borderwidth,
      borderwidth,
      width - borderwidth,
      height - borderwidth
      );
  end;

  // check how many characters there is inthe
  // caption text
  len:= length(fcaption);

  // use bsclear to draw the text without painting
  // the background
  Acanvas.Brush.Style:= bsclear;

  {
  DT_BOTTOM	Justifies the text to the bottom of the rectangle. This value must be combined with DT_SINGLELINE.
  DT_CALCRECT	Determines the width and height of the rectangle. If there are multiple lines of text, DrawText uses the width of the rectangle pointed to by the lpRect parameter and extends the base of the rectangle to bound the last line of text. If there is only one line of text, DrawText modifies the right side of the rectangle so that it bounds the last character in the line. In either case, DrawText returns the height of the formatted text but does not draw the text.
  DT_CENTER	Centers text horizontally in the rectangle.
  DT_EDITCONTROL	Duplicates the text-displaying characteristics of a multiline edit control. Specifically, the average character width is calculated in the same manner as for an edit control, and the function does not display a partially visible last line.
  DT_END_ELLIPSIS or DT_PATH_ELLIPSIS	Replaces part of the given string with ellipses, if necessary, so that the result fits in the specified  rectangle. The given string is not modified unless the DT_MODIFYSTRING flag is specified.You can specify DT_END_ELLIPSIS to replace characters at the end of the string, or DT_PATH_ELLIPSIS to replace characters in the middle of the string. If the string contains backslash (\) characters, DT_PATH_ELLIPSIS preserves as much as possible of the text after the last backslash.
  DT_EXPANDTABS	Expands tab characters. The default number of characters per tab is eight.
  DT_EXTERNALLEADING	Includes the font external leading in line height. Normally, external leading is not included in the height of a line of text.
  DT_LEFT	Aligns text to the left.
  DT_MODIFYSTRING	Modifies the given string to match the displayed text. This flag has no effect unless the DT_END_ELLIPSIS or DT_PATH_ELLIPSIS flag is specified.
  DT_NOCLIP	Draws without clipping. DrawText is somewhat faster when DT_NOCLIP is used.
  DT_NOPREFIX	Turns off processing of prefix characters. Normally, DrawText interprets the mnemonic-prefix character & as a directive to underscore the character that follows, and the mnemonic-prefix characters && as a directive to print a single &. By specifying DT_NOPREFIX, this processing is turned off.
  DT_RIGHT	Aligns text to the right.
  DT_RTLREADING	Layout in right to left reading order for bi-directional text when the font selected into the hdc is a Hebrew or Arabic font. The default reading order for all text is left to right.
  DT_SINGLELINE	Displays text on a single line only. Carriage returns and linefeeds do not break the line.
  DT_TABSTOP	Sets tab stops. Bits 15-8 (high-order byte of the low-order word) of the uFormat parameter specify the number of characters for each tab. The default number of characters per tab is eight.
  DT_TOP	Top-justifies text (single line only).
  DT_VCENTER	Centers text vertically (single line only).
  DT_WORDBREAK	Breaks words. Lines are automatically broken between words if a word would extend past the edge of the rectangle specified by the lpRect parameter. A carriage return-linefeed sequence also breaks the line.
  }

  // define text drawing flags..
  flags:= DT_NOPREFIX;
  if fwordwrap then flags:= flags or DT_WORDBREAK;
  if alignment = taLeftJustify then flags:= flags or DT_LEFT;
  if alignment = taRightJustify then flags:= flags or DT_RIGHT;
  if alignment = taCenter then flags:= flags or DT_CENTER;
  (*
  // unneeded on calculating space
  // requirements (but available
  // on d2k6!!!)
  if VerticalAlignment = taVerticalCenter then flags:= flags or DT_VCENTER;
  if verticalalignment = taAlignTop then flags:= flags or DT_TOP;
  if verticalalignment = taAlignBottom then flags:= flags or DT_BOTTOM;
  *)

  // first calc actual needed space for
  // the text in vertical
  temprect:= drawrect;
  flags:= flags + DT_CALCRECT;
  windows.DrawText(
    Acanvas.Handle,
    pchar(fcaption),
    Len,
    temprect,
    flags);
  flags:= flags - DT_CALCRECT;

  // manually calculate needed space, because
  // api function for that seems to suck for some
  // reason.
  (*
  temprect:= rect(0,0,0,0);
  if ((flags and DT_WORDBREAK)>0) and (pos(' ',fcaption)>0) then
  begin
    ts:= fcaption;
    s:= '';
    while (ts<>'') do                   // while whole caption processed
    begin
      p:= pos(' ',ts);                  // find next space
      if p=0 then p:= length(ts);       // take rest if space not found
      if p>0 then                       // if found
      begin
        // combonation exceeeds the width
        if (canvas.TextWidth(s+copy(ts,1,p))>(drawrect.right-drawrect.left)) then
        begin
          if (s<>'') and (canvas.textwidth(copy(ts,1,p))>(drawrect.right-drawrect.left)) then
          begin
            temprect.Bottom:= temprect.Bottom + canvas.textheight(s);
            if canvas.textwidth(s) > temprect.right then
              temprect.Right:= canvas.TextWidth(s);
            s:= s + copy(ts,1,p-1);
          end else
          begin
            s:= s + copy(ts,1,p-1);
            temprect.Bottom:= temprect.Bottom + canvas.textheight(s);
            if canvas.textwidth(s) > temprect.right then
              temprect.Right:= canvas.TextWidth(s);
            s:= '';
          end;
        end else
        // just increase words
          s:= s + copy(ts,1,p);
        delete(ts,1,p);
      end;
    end;
  end else
  begin
    // one line..
    temprect.Bottom:= canvas.TextHeight(fcaption);
    temprect.right:= canvas.textwidth(fcaption);
  end;
  showmessage(inttostr(temprect.Left)+', '+
    inttostr(temprect.Top)+', '+
    inttostr(temprect.Right)+', '+
    inttostr(temprect.Bottom));
  *)
  // re-check ranges..
//  if temprect.Right > drawrect.Right then temprect.Right:= drawrect.right;
//  if temprect.Left < drawrect.left then temprect.left:= drawrect.left;

  // horizontal alignment
  case alignment of
    taLeftJustify:
      begin
        w:= drawrect.left;
        if (fledexists) and (fledpos=lpleft) then w:= w + borderwidth;
      end;
    taCenter:
      begin
        w:= ((drawrect.right-drawrect.Left) - (temprect.right-temprect.left)) div 2;
      end;
    taRightJustify:
      begin
        w:= drawrect.Right - temprect.right; //-temprect.Left);
        if (fledexists) and (fledpos=lpright) then w:= w - borderwidth;
      end;
    else raise exception.create('Invalid Alignment property');
  end;
  temprect.Left:= temprect.Left + w;
  temprect.Right:= temprect.Right + w;

  // vertical alignment
  case VerticalAlignment of
    taAlignTop:         h:= drawrect.top;
    taVerticalCenter:   h:= ((drawrect.Bottom-drawrect.top) - (temprect.bottom-temprect.Top)) div 2;
    taAlignBottom:      h:= drawrect.bottom - (temprect.bottom-temprect.top);
    else raise exception.create('Invalid VerticalAlignment property');
  end;
  temprect.top:= temprect.top + h;
  temprect.bottom:= temprect.bottom + h;

  // write the actual text into area defined
  // at the beginning of this drawing routine
  windows.DrawText(Acanvas.Handle, pchar(fcaption), Len, temprect, flags);
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.drawpicture(ACanvas: TCanvas; Const Arect: Trect);
begin
  Acanvas.StretchDraw(Arect, bmp);
  (*
  Canvas.BrushCopy(Rect(Width div 2-Bmp.Width div 2+1,
    Height div 2-Bmp.Height div 2+1,
    bmp.width+Width div 2-Bmp.Width div 2+1,
    bmp.height+Height div 2-Bmp.Height div 2+1),
    bmp,Rect(0,0,bmp.width,
    bmp.height),bmp.Canvas.pixels[0,0]);
  *)
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.drawled(ACanvas: TCanvas; Const Arect: Trect);
var
  x1, y1, x2, y2, pw, pw3: integer;
begin
  (*
      what we need to check here is the leds
      width to calculate the text area correctly -
      to avoid text to be drawn on the led.
  *)

  // draw led
  with Acanvas do
  begin

    // calculate rectangle for the led
    // to be drawn (led size is fixed
    // temporarily until have some time
    // to add that property)
    pw:= FCheckheight div 2; // div 2 + borderwidth; // check image size; todo: add as one of settable properties
    if fledpos=lpright then
    //if alignment = taRightJustify then
    begin
      // right justify
      x1:= width - (pw*3) - borderwidth; // reserve some space
      y1:= (self.height div 2) - pw;
      x2:= width - pw - borderwidth;
      y2:= (self.height div 2) + pw;
    end else
    begin
      // left justify
      x1:= pw + borderwidth;
      y1:= (self.height div 2) - pw;
      x2:= pw*3 + borderwidth; // reserve some space
      y2:= (self.height div 2) + pw;
    end;
    // init temporary var for specials
    pw3:= FCheckheight div 3;

    // brush color according to led state
    if fledstate then Brush.Color:= fledcoloron
      else brush.color:= fledcoloroff;

    // actual drawing
    case FledStyle of
      LSSquare:
        FillRect(rect(x1,y1,x2,y2));
      LSEllipse:
        Ellipse(x1,y1,x2,y2);
      LSBox:
        begin
          Pen.Color:= Brush.Color;
          pen.Width:= 2;
          MoveTo(x1,y1);
          LineTo(x1,y2);
          lineto(x2,y2);
          lineto(x2,y1);
          lineto(x1,y1);
        end;
      LSCross:
        begin
          Pen.Color:= Brush.Color;
          pen.Width:= 2;
          moveto(x1,y1);
          lineto(x2,y2);
          moveto(x1,y2);
          LineTo(x2,y1);
        end;
      LSCheck:
        begin
          if fledstate then
          begin
            // red background
            Pen.Color:= fledcoloroff;
            pen.Width:= 1;
            MoveTo(x1+pw3, y1+pw3);
            LineTo(x1+pw3, y2);
            lineto(x2, y2);
            lineto(x2, y1+pw3);
            lineto(x1+pw3, y1+pw3);
            // green check
            pen.color:= fledcoloron;
            pen.width:= 2;
            moveto(x1,y1);
            lineto(x2-pw3,y2);
            lineto(x2-pw3,y1);
          end else
          begin
            Pen.Color:= canvas.Brush.Color;
            pen.Width:= 1;
            MoveTo(x1,y1);
            LineTo(x1,y2);
            lineto(x2,y2);
            lineto(x2,y1);
            lineto(x1,y1);
          end;
        end;
      LSCheckBox:
        begin
          if fledstate then
          begin
            // red background
            Pen.Color:= fledcoloroff;
            pen.Width:= 1;
            MoveTo(x1, y1);
            LineTo(x1, y2);
            lineto(x2, y2);
            lineto(x2, y1);
            lineto(x1, y1);
            // green check
            pen.color:= fledcoloron;
            pen.width:= 2;
            moveto(x1,y1);
            lineto(x2,y2);
            moveto(x1,y2);
            lineto(x2,y1);
          end else
          begin
            Pen.Color:= Brush.Color;
            canvas.pen.Width:= 1;
            MoveTo(x1,y1);
            LineTo(x1,y2);
            lineto(x2,y2);
            lineto(x2,y1);
            lineto(x1,y1);
          end;
        end;
    end; // case
  end; // with canvas
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.Paint;
var
  Arect: trect;
  tmpI: integer;
begin
  //inherited Paint;
  SetRegion;

  // adjust background buffer size
  fbuffer.Width:= width;
  fbuffer.Height:= height;
  tmpI:= borderwidth div 2;
  Arect:= rect(tmpI, tmpI, width - tmpI, height - tmpI);

  // background
  with fbuffer.canvas do
  begin
    if not enabled then // 02072009
    begin
      Font:= self.Font;
      brush.color:= fdisabledcolor;
    end else
    if fmouseover then
    begin
      Font:= fmouseoverfont;
      if fmousedown then brush.color:= fdowncolor
        else brush.color:= fovercolor;
    end else
    begin
      Font:= self.Font; // 19062006
      Brush.color:= fcolor;
    end;
    // do gradient fill
    if not fflipgradient then
      gradientfill(fbuffer.canvas, rect(borderwidth, borderwidth, width-borderwidth, height-borderwidth), Brush.Color, fgradientend, fgradientstyle)
      else gradientfill(fbuffer.canvas, rect(borderwidth, borderwidth, width-borderwidth, height-borderwidth), fgradientend, brush.color, fgradientstyle);
  end;

  // draw picture
  if not bmp.Empty then drawpicture(FBuffer.Canvas, Arect);

  // draw led, ledexists is handled inside the function
  if fledexists then drawled(FBuffer.Canvas, Arect);

  // draw text
  if fshowcaption then drawtext(FBuffer.Canvas, Arect);

  // draw border
  with fbuffer.canvas do
  begin
    if borderwidth>0 then
    begin
      Pen.Color:= fborder;
      Pen.Width:= borderwidth;
      Brush.Style:= bsclear;
      if felliptic then
      begin
        RoundRect(tmpI, tmpI, (width-tmpI), (height-tmpI), FRadius, FRadius)
      end else
      begin
        Rectangle(rect(tmpI, tmpI, (width-tmpI), (height-tmpI)));
      end;
    end;
  end;

  // copy fbuffer bitmap to canvas
  canvas.Draw(0,0,fbuffer);
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.Click;
begin
  inherited;
  if enabled then
    if fledchangeonclick then
      fledstate:=not fledstate;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.setcolor (Const value: TColor);
begin
  if fcolor<>value then
  begin
    fcolor := Value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.SetBorder(Const Value: TColor);
begin
  if fborder<>value then
  begin
    fborder:= Value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetRadius(Const NewValue: Integer);
begin
  if newvalue<>fradius then
  begin
    deleteregion;
    fradius:= newvalue;
    setregion;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_grbutton.SetElliptic(Const MakeItElliptic: Boolean);
begin
  if MakeItElliptic<>FElliptic then
  begin
    deleteregion;
    felliptic:= MakeItElliptic;
    setregion;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.setcaption(Const value: string);
begin
  if fcaption <> value then
  begin
    fcaption:= value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.SetBmp(Value: TBitmap);
begin
  Bmp.Assign(value);
  bmp.Transparent:= false;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.setshowcaption(Const value: boolean);
begin
  if value<>fshowcaption then
  begin
    fshowcaption:= value;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if button = mbleft then
  begin
    fmousedown:= TRUE;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if fmousedown then
  begin
    fmousedown:= FALSE;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.MouseEnter(var Message: TMessage);
begin
  if fdesigning then exit;
  fmouseover:=true;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure tAPI_grbutton.MouseLeave(var Message: TMessage);
begin
  if fdesigning then exit;
  fmouseover:=false;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_grbutton]);
end;

end.
