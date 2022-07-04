unit API_memo;

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
// 03112009, r1.08, ari pikivirta
//  * should follow parent font and color changes (in case same)
//  * added overloaded version of findtext function
//
// 30062009, r1.07, ari pikivirta
//  * added SetTabWidth procedure
//
// 25052007, r1.06, ari pikivirta
//  * added ScrollToBottom and ScrollToTop procedures
//
// 12102004, r1.05, ari pikivirta
//  * renamed mouseoverfont property to fontmouseover
//  * added font synchronization possibility with main font (name = same)
//  * added maxlines property (removes lines depending on caret pos)
//
// r1.04, 12082004, ari pikivirta
// * added font when mouse is over
//
// r1.03, ari pikivirta
// * fixed mouse over coloring
//
// r1.02, ari pikivirta
// * on mouse over color added
//
// r1.01, ari pikivirta
// * added cursor positioning
// * added onmouseleave and onmouseenter events

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics;

type
  TAPI_memo = class(TMemo)
  private
    fversion: string;
    fmouseoverinitialized: boolean;
    fonhscroll: tnotifyevent;
    fonvscroll: tnotifyevent;
    fonmouseleave: tnotifyevent;
    fonmouseenter: tnotifyevent;
    foldcolor: tcolor;
    fmouseovercolor: tcolor;
    foldfont: tfont;
    fmouseoverfont: tfont;
    fmaxlines: integer;
    ftabwidth: integer;

    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;

    procedure setmouseoverfont ( f : tfont );
    procedure dummys(s: string);
    procedure setmaxlines(i: integer);

    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Change; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  Line: integer;
    function  Col: integer;
    procedure SetPos (Col, Line: integer);
    function  FindText(Const ALookForThis: string; Const AStartFrom: integer = 1; Const ASetSelStart: Boolean = TRUE): integer; overload;
    function  FindText(Const ALookForThis: string): boolean; overload;

    procedure Insert(const Index: integer; const LineText: string);

    procedure ScrollToBottom;
    procedure ScrollToTop;

    procedure SetTabWidth(Const I: Integer);

  published
    property Version: string read fversion write dummys stored false;
    property MouseOverColor: tcolor read fmouseovercolor write fmouseovercolor;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
    property OnHScroll: tnotifyevent read fonhscroll write fonhscroll;
    property OnVScroll: tnotifyevent read fonvscroll write fonvscroll;
    property FontMouseOver: tfont read fmouseoverfont write setmouseoverfont;
    property Font;
    property MaxLines: integer read fmaxlines write setmaxlines;

  end;

procedure Register;

implementation

{$r *.res}
{$WARN UNSAFE_CODE OFF}

const
  VERSIONINFOSTRING: string = 'r1.08/ari.pikivirta}at{kolumbus.fi';

procedure TAPI_memo.dummys(s: string); begin end;

//------------------------------------------------------------------------------
procedure TAPI_memo.setmouseoverfont(f: tfont);
begin
  if f<>fmouseoverfont then
  begin
    fmouseoverfont.Assign( f );
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
constructor TAPI_memo.create(aowner: tcomponent);
begin
  inherited create(aowner);
  doublebuffered:= True;
  fversion:= VERSIONINFOSTRING;
  foldcolor:= color;
  fmouseovercolor:= color;
  foldfont:= tfont.create;
  foldfont.assign ( font );
  fmouseoverfont:= tfont.create;
  fmouseoverfont.assign ( font );
  fmouseoverinitialized:=false;
  // capture memo's background
  // fback:=tbitmap.create;
  // fback:=captureclientimage(self);
  fmaxlines:= 0;
  ftabwidth:= 8;
end;

//------------------------------------------------------------------------------
destructor TAPI_memo.destroy;
begin
  // if assigned (fback) then fback.free;
  fmouseoverfont.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.CMParentColorChanged(var Message: TMessage);
var
  Same1: Boolean;
begin
  Same1:= fmouseovercolor = color;
  inherited;
  if (Same1) then fmouseovercolor:= color;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.CMParentFontChanged(var Message: TMessage);
  //
  function IsFontEqual (Font1, Font2: TFont): Boolean;
  begin
    Result := ( Font1.Name = Font2.Name ) and
      ( Font1.Size = Font2.Size ) and
      ( Font1.Style = Font2.Style ) and
      ( Font1.Color  = Font2.Color );
  end;
  //
var
  Same1: Boolean;
begin
  Same1:= IsFontEqual(font, fmouseoverfont);
  inherited;
  if (Same1) then fmouseoverfont.assign(font);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.SetTabWidth(Const I: Integer);
begin
  self.WantTabs:= TRUE;
  if (i<>ftabwidth) then
  begin
    ftabwidth:= i;
    SendMessage(self.Handle, EM_SETTABSTOPS, 1, Longint(@ftabwidth));
  end;
end;

//------------------------------------------------------------------------------
function TAPI_memo.Line: integer;
begin
  result:=perform (EM_LINEFROMCHAR, SelStart, 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.setmaxlines(i: integer);
begin
  if (i<>fmaxlines) and ((i>2) or (i=0)) then
  begin
    fmaxlines:= i;
    change;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_Memo.Insert(const Index: integer; const LineText: string);
begin
  lines.Insert(index, LineText);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.Change;
var
  pt: tpoint;
begin
  if (fmaxlines>0) and (lines.count>fmaxlines) then
  begin
    pt:= getcaretpos;                         // get cursor position
    if (pt.Y<3) then                          // if below first line
    begin
      while lines.count>fmaxlines do          // while more than needded
        lines.delete(lines.count-1);          // delete last line
      selstart:= 0;                           // set cursor to beginning
    end else
    begin
      while lines.Count>fmaxlines do          // while more than needed
        lines.Delete(0);                      // remove from beginning
      selstart:= length(lines.text);          // goto last pos
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.ScrollToBottom;
begin
  self.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.ScrollToTop;
begin
  self.Perform(WM_VSCROLL, SB_TOP, 0);
end;

//------------------------------------------------------------------------------
function TAPI_memo.Col: integer;
begin
  result:= selstart - perform(EM_LINEINDEX, line, 0) + 1;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.SetPos (Col, Line: integer);
begin
  selstart:= perform (EM_LINEINDEX, line, 0) + col -1;
end;

//------------------------------------------------------------------------------
function TAPI_memo.FindText(
  Const ALookForThis: string;
  Const AStartFrom: integer = 1;
  Const ASetSelStart: Boolean = TRUE): integer;
begin
  if (AStartFrom>0) and (AStartFrom<length(self.text)) then
    result:= pos(ALookForThis, copy(self.text, AStartFrom, length(self.text)))
    else result:= pos(ALookForThis, self.text);
  //
  if (result>0) then
  begin
    if (ASetSelStart) then
    begin
      self.SelStart:= result;
      self.SelLength:= length(self.text);
    end;
  end;
end;

function TAPI_memo.FindText(Const ALookForThis: string): boolean;
begin
  result:= not (FindText(ALookForThis, 1, TRUE)<0);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.WMHScroll(var Message: TWMHScroll);
begin
  if assigned(fonhscroll) then fonhscroll(self);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.MouseEnter(var Message: TMessage);
begin
  if (not (csdesigning in componentstate)) and (fmouseovercolor<>color) then
  begin
    fmouseoverinitialized:=true;
    foldcolor:=color;
    color:=fmouseovercolor;
    foldfont.Assign( font );
    font.Assign (fmouseoverfont);
    invalidate;
  end;
  if assigned(fonmouseenter) then fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.MouseLeave(var Message: TMessage);
begin
  if (not (csdesigning in componentstate)) and (fmouseovercolor<>foldcolor)
    and (fmouseoverinitialized) then
  begin
    font.Assign (foldfont);
    color:=foldcolor;
    invalidate;
  end;
  if assigned(fonmouseleave) then fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.WMVScroll(var Message: TWMVScroll);
begin
  if assigned(fonvscroll) then fonvscroll(self);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_memo.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_memo]);
end;

end.
