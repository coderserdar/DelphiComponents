unit API_edit;

//------------------------------------------------------------------------------
// edit box with several addon features like, different colors when mouse is
// is on component or component is in edit state and can be set to only accept
// numbers.
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
// 03112009, r1.14, ari pikivirta
//  * added checking of parent font and color changes
//
// 03072009, r1.13, ari pikivirta
//  * improved checking keys on filling the edit already
//
// 03072009, r1.12, ari pikivirta
//  * added properties to enabled elliptic look (but to work properly needs
//    user to put the borderstule to bsNone manually)
//  * minor code cleaning since there's been some time since last looked at this
//  * removed settings saving and loading as unnecessary feature
//
// 25052007, r1.11, ari pikivirta
//  * added functions Multiply and Divide for numerical contents
//
// 23082006, r1.10, ari pikivirta
//  * default font fixed
//
// 12.7.2006, r1.09, ari pikivirta
//  * added settings property to easily exchange properties
//  * fixed bug in float formatting (decimal was allowed as single character)
//
// 11.4.2006, r1.08, ari pikivirta
//  * removed font synchronization
//  * added hex editing mode
//  * fixed conversions more flexible (asfloat, asinteger..)
//
// 15.9.2004, r1.07, ari pikivirta
// * added decimal count property (again), this time working
// * changed filtering of values and keys a bit more simple way
//
// 1.9.2004, r1.06, ari pikivirta
// * ndded support for hexadecimal values
// * fixed bug using float editor
// * removed decimal count property
//
// r1.05, 12082004, ari pikivirta
// * added mouse over font
// * added font when editing
//
// r1.04, 10082004, ari pikivirta
// * added min & max property for number editing
// * added decimal count property when editing floating point values
// * added get value as datetime function
//
// r1.03, ari pikivirta
// * combined the functionality of API_numedit to this one (mode change)
// * automatically converts current text to selected mode
//
// r1.02, ari pikivirta
// * now checking component design state so that the component
//   doesn't change color in any sitution while designing
//

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, graphics, forms, API_base;

type
  tapieditmode = (mNumEdit, mFloatEdit, mTextEdit, mHexEdit);

  TAPI_edit = class(TAPI_Custom_Edit)
  private
    falignment: talignment;
    fwordwrap: boolean;
    fmultiline: boolean;
    fcolorin: tcolor;
    fcolorout: tcolor;
    fcolormouse: tcolor;
    fmode: tapieditmode;
    fmouseover: boolean;
    fmouseenter: tnotifyEvent;
    fmouseleave: tnotifyEvent;
    fdecimals: integer;
    fvaluemin: double;
    fvaluemax: double;
    fuserange: boolean;
    fdeffont: tfont;
    feditfont: tfont;
    fmouseoverfont: tfont;
    frgn: HRGN;
    felliptic: boolean;
    fradius: integer;

    procedure setwordwrap(Const w: boolean);
    procedure setalignment(Const a: talignment);
    procedure setcolorin(Const c: tcolor);
    procedure setcolorout(Const c: tcolor);
    procedure setmultiline(Const b: boolean);
    procedure setcolormouse(Const c: tcolor);
    procedure setmode(Const m: tapieditmode);
    procedure setdecimals(Const i: integer);
    procedure setminvalue(Const d: double);
    procedure setmaxvalue(Const d: double);
    procedure setmouseoverfont(Const f: tfont);
    procedure seteditfont(Const f: tfont);
    procedure SetRegion;
    procedure DeleteRegion;
    procedure SetElliptic(Const b: Boolean);
    procedure SetRadius(Const r: integer);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;

  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure Resize; override;
    //procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CreateParams(var params: TCreateParams); override;
    procedure   KeyPress(var Key: Char); override;

    procedure   CheckKey(var Key: Char; Const CharPosition: Integer = 0);
    procedure   Filter;
    function    asInteger: Int64; overload;
    procedure   asInteger(Const I: Int64); overload;
    function    asFloat: Double; overload;
    procedure   asFloat(Const F: Double); overload;
    function    asDateTime: TDatetime; overload;
    procedure   asDateTime(Const DT: TDatetime); overload;

    function    Multiply(Const d: Double): double;
    function    Divide(Const d: Double): double;

  published
    property EditMode: tapieditmode read fmode write setmode;
    property Decimals: integer read fdecimals write setdecimals;
    property Wordwrap: boolean read fwordwrap write setwordwrap;
    property Alignment: talignment read falignment write setalignment;
    property ColorIn: tcolor read fcolorin write setcolorin;
    property ColorOut: tcolor read fcolorout write setcolorout;
    property Multiline: boolean read fmultiline write setmultiline;
    property ColorMouse: tcolor read fcolormouse write setcolormouse;
    property OnMouseEnter: tnotifyevent read fmouseenter write fmouseenter;
    property OnMouseLeave: tnotifyevent read fmouseleave write fmouseleave;
    property ValueRange: boolean read fuserange write fuserange;
    property ValueMin: double read fvaluemin write setminvalue;
    property ValueMax: double read fvaluemax write setmaxvalue;
    property Color;
    property FontMouseOver: tfont read fmouseoverfont write setmouseoverfont;
    property FontEditing: tfont read feditfont write seteditfont;

    property Elliptic: boolean read felliptic write setelliptic; //felliptic;
    property EllipticRadius: integer read fradius write setradius; //fradius;
  end;

procedure Register;

implementation

const
  versioninfo = 'r1.14/ari.pikivirta)at(kolumbus.fi';

{$include '..\API_source\inc\CompilerVersions.INC'}
{$r *.res}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

//=========================
// function: create
// testing this item to have some detailed comments also documented
// via the component2html thing :)
//=========================
constructor tAPI_edit.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:= versioninfo;

  color:= clwhite;
  fdeffont:= tfont.create;
  fdeffont.assign(tform(aowner as tform).font);
  fmouseoverfont:= tfont.create;
  fmouseoverfont.assign(fdeffont);
  feditfont:= tfont.create;
  feditfont.assign(fdeffont);
  fmode:= mTextEdit;
  fwordwrap:= FALSE;
  falignment:= taleftjustify;
  fcolorin:= $00CAFFCA;
  fcolorout:= clwhite;
  fcolormouse:= $00B7FFFF;
  fdecimals:= 2;
  fvaluemin:= 0;
  fvaluemax:= 0;
  fmultiline:= FALSE;
  fmouseover:= FALSE;
  width:= 80;
  height:= 22;
  fuserange:= FALSE;
  frgn:= 0;
  felliptic:= FALSE;
  fradius:= 5;
end;

// ----------------------------------------------------------
procedure TAPI_edit.CMParentColorChanged(var Message: TMessage);
var
  ASame1, ASame2, ASame3: Boolean;
begin
  ASame1:= fcolorin = color;
  ASame2:= fcolorout = color;
  ASame3:= fcolormouse = color;
  inherited;
  if (Asame1) then fcolorin:= color;
  if (ASame2) then fcolorout:= color;
  if (ASame3) then fcolormouse:= color;
end;

// ----------------------------------------------------------
procedure TAPI_edit.CMParentFontChanged(var Message: TMessage);
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
  ASame1, ASame2, ASame3: Boolean;
begin
  ASame1:= IsFontEqual(fdeffont, font);
  ASame2:= IsFontEqual(fmouseoverfont, font);
  ASame3:= IsFontEqual(feditfont, font);
  inherited;
  if (ASame1) then fdeffont.assign(font);
  if (ASame2) then fmouseoverfont.assign(font);
  if (ASame3) then feditfont.assign(font);
end;

// ----------------------------------------------------------
procedure TAPI_edit.Loaded;
begin
  inherited;
end;

// ----------------------------------------------------------
destructor tAPI_edit.destroy;
begin
  DeleteRegion;
  fmouseoverfont.Free;
  feditfont.free;
  fdeffont.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_edit.SetRegion;
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

procedure TAPI_edit.DeleteRegion;
begin
  If frgn=0 Then Exit;
  DeleteObject(frgn);
  frgn:= 0;
End;

// ---------------------------------------------------------
procedure TAPI_edit.Resize;
begin
  DeleteRegion;
  inherited;
  SetRegion;
end;

// ---------------------------------------------------------
procedure TAPI_edit.SetElliptic(Const b: Boolean);
begin
  if b<>felliptic then
  begin
    deleteregion;
    felliptic:= b;
    setregion;
    invalidate;
  end;
end;

// ---------------------------------------------------------
procedure TAPI_edit.SetRadius(Const r: integer);
begin
  if r<>fradius then
  begin
    deleteregion;
    fradius:= r;
    setregion;
    invalidate;
  end;
end;

// ---------------------------------------------------------
(*
procedure TAPI_edit.Paint;
begin
  SetRegion;
  inherited;
end;
*)

// ---------------------------------------------------------
procedure TAPI_edit.setmouseoverfont(Const f: tfont);
begin
  if fmouseoverfont<>f then
  begin
    fmouseoverfont.assign(f);
    if fmouseover then invalidate;
  end;
end;

// ---------------------------------------------------------
procedure TAPI_edit.seteditfont(Const f: tfont);
begin
  if feditfont<>f then
  begin
    feditfont.assign(f);
    if self.Focused then invalidate;
  end;
end;

// ---------------------------------------------------------
procedure TAPI_edit.CheckKey(var Key: Char; Const CharPosition: Integer = 0);
begin
  case fmode of
    mNumEdit:
      begin
        {$ifdef DELPHI2009UP}
        if (charinset(key, ['0'..'9', '+', '-', #8, #13])) then
        {$else}
        if (key in ['0'..'9', '+', '-', #8, #13]) then
        {$endif}
        begin
          if CharPosition>1 then
          begin
            {$ifdef DELPHI2009UP}
            if (charinset(key, ['+', '-'])) then
            {$else}
            if (key in ['+', '-']) then
            {$endif}
              key:= #0; // + or - can only be put in beginning
          end;
        end else
          key:= #0;
      end;
    mFloatEdit:
      begin
        {$ifdef DELPHI2009UP}
        if (charinset(key, ['0'..'9', '+', '-', decimalseparator, #8, #13])) then
        {$else}
        if (key in ['0'..'9', '+', '-', decimalseparator, #8, #13]) then
        {$endif}
        begin
          if (CharPosition>1) then
          begin
            {$ifdef DELPHI2009UP}
            if (charinset(key, ['+', '-'])) then
            {$else}
            if (key in ['+', '-']) then
            {$endif}
              key:= #0; // + or - can only be put in beginning
          end else
          if (CharPosition=1) and (key=decimalseparator) then key:= #0;
        end else
          key:= #0;
      end;
    mHexEdit:
      begin
        {$ifdef DELPHI2009UP}
        if not (charinset(key, ['0'..'9', 'A'..'F', 'a'..'f', #8, #13])) then
        {$else}
        if not (key in ['0'..'9', 'A'..'F', 'a'..'f', #8, #13]) then
        {$endif}
        key:=#0;
      end;
    mTextEdit:  ; // no need to change anything
  end;
end;

// ---------------------------------------------------------
procedure TAPI_edit.Filter;
var
  i: int64;
  j: integer;
  d: extended;
  s, temps: string;
  ch: char;
  //
  function GetCheckedInput: string;
  var
    k: integer;
  begin
    result:= '';
    for k:=1 to length(text) do
    begin
      ch:= text[k];
      checkkey(ch, k);
      if ch<>#0 then result:= result + ch;
    end;
  end;
  //
begin
  case fmode of
    mnumedit:
      begin
        s:= GetCheckedInput;
        if (fuserange) and (fvaluemax>fvaluemin) then
        begin
          trystrtoint64(s, i);
          if i>fvaluemax then s:= inttostr( trunc( fvaluemax ) );
          if i<fvaluemin then s:= inttostr( trunc( fvaluemin ) );
        end;
        text:= s;
      end;
    mfloatedit:
      begin
        s:= GetCheckedInput;
        trystrtofloat(s, d);
        if (fuserange) and (fvaluemax>fvaluemin) then
        begin
          if d>fvaluemax then s:= floattostr( fvaluemax );                      // limit to maximum of range
          if d<fvaluemin then s:= floattostr( fvaluemin );                      // limit to minimum of range
        end;
        if decimals>0 then                                                      // if amount of decimals is set
        begin
          temps:= '';                                                           // clear temp string
          for j:=0 to decimals-1 do temps:=temps+'0';
          s:= formatfloat('0.'+temps, d);                                     // store formatted into s
        end;
        text:= s;                                                               // set s as text
      end;
    mHexEdit:
      begin
        s:= GetCheckedInput;
        if s='' then s:= '0';
        if (fuserange) and (fvaluemax>fvaluemin) then
        begin
          trystrtoint64('$'+s, i);
          if i>fvaluemax then s:= inttohex(trunc(fvaluemax), trunc(fvaluemax/16));
          if i<fvaluemin then s:= inttohex(trunc(fvaluemin), trunc(fvaluemin/16))
        end;
        text:= s;
      end;
    mtextedit:;
  end;
end;

// ----------------------------------------------------------
procedure TAPI_edit.setmode(Const m: tapieditmode);
begin
  if m<>fmode then
  begin
    fmode:=m;
    filter;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.KeyPress(var Key: Char);
var
  x: integer;
begin
  X:= LongRec(Perform(EM_GETSEL, 0, 0)).Hi;
  //Y:= Perform(EM_LINEFROMCHAR, Result.X, 0); = line
  X:= X - Perform(EM_LINEINDEX, -1, 0);
  CheckKey(Key, (X+1));
  if (key=#13) then filter;
  inherited keypress(key);
end;

// ----------------------------------------------------------
procedure TAPI_edit.setminvalue(Const d: double);
begin
  if (d<>fvaluemin) and (d<fvaluemax) then
  begin
    fvaluemin:= d;
    filter;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure TAPI_edit.setmaxvalue(Const d: double);
begin
  if (d<>fvaluemax) and (d>fvaluemin) then
  begin
    fvaluemax:= d;
    filter;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure TAPI_edit.setdecimals(Const i: integer);
begin
  if i<>fdecimals then
  begin
    fdecimals:= i;
    filter;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.createparams(var params: TCreateParams);
const
  Alignments: array[TAlignment] of word=(ES_LEFT,ES_RIGHT,ES_CENTER);
  WordWraps: array[Boolean] of longint = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(params);
  Params.Style:=Params.Style and (not WordWraps[FWordWrap]) or Alignments[FAlignment];
  if fmultiline then params.style:=params.style or ES_MULTILINE;
end;

// ----------------------------------------------------------
procedure tAPI_edit.setwordwrap(Const w: boolean);
begin
  if fwordwrap<>w then
  begin
    fwordwrap:=w;
    recreatewnd;
  end;
end;

// ----------------------------------------------------------
function TAPI_edit.asInteger: int64;
var
  d: extended;
begin
  case fmode of
    mNumEdit:
      trystrtoint64(text, result);
    mFloatEdit:
      begin
        trystrtofloat(text, d);
        result:= trunc(d);
      end;
    mTextEdit:
      trystrtoint64(text, result);
    mHexEdit:
      trystrtoint64('$'+text, result);
  end;
end;

// ----------------------------------------------------------
procedure TAPI_edit.asInteger(Const i: int64);
begin
  text:= inttostr(i);
end;

// ----------------------------------------------------------
function TAPI_edit.asFloat: double;
begin
  case fmode of
    mNumEdit:
      trystrtofloat(text, result);
    mFloatEdit:
      trystrtofloat(text, result);
    mTextEdit:
      trystrtofloat(text, result);
    mHexEdit:
      trystrtofloat('$'+text, result);
  end;
end;

// ----------------------------------------------------------
procedure TAPI_edit.asFloat(Const f: double);
begin
  text:= floattostr(f);
end;

// ----------------------------------------------------------
function TAPI_edit.asDatetime: tdatetime;
begin
  result:= asFloat;
end;

// ----------------------------------------------------------
procedure TAPI_edit.asDatetime(Const dt: tdatetime);
begin
  text:= floattostr(dt);
end;

// ----------------------------------------------------------
function TAPI_edit.Multiply(Const d: double): double;
begin
  result:= asFloat * d;
  asFloat(result);
end;

// ----------------------------------------------------------
function TAPI_edit.Divide(Const d: double): double;
begin
  result:= asFloat / d;
  asFloat(result);
end;

// ----------------------------------------------------------
procedure tAPI_edit.setalignment(Const a: talignment);
begin
  if falignment<>a then
  begin
    falignment:=a;
    recreatewnd;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.setcolorin(Const c: tcolor);
begin
  if fcolorin<>c then
  begin
    fcolorin:=c;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.setcolorout(Const c: tcolor);
begin
  if fcolorout<>c then
  begin
    fcolorout:=c;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.doenter;
begin
  if not (csdesigning in componentstate) then
  begin
    color:=fcolorin;
    font.Assign(feditfont);
    invalidate;
  end;
  inherited;
end;

// ----------------------------------------------------------
procedure tAPI_edit.doexit;
begin
  if not (csdesigning in componentstate) then
  begin
    color:=fcolorout;
    filter;
    font.Assign(fdeffont);
    invalidate;
  end;
  inherited;
end;

// ----------------------------------------------------------
procedure tAPI_edit.setmultiline(Const b: boolean);
begin
  if b<>fmultiline then
  begin
    fmultiline:=b;
    recreatewnd;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.setcolormouse(Const c: tcolor);
begin
  if c<>fcolormouse then
  begin
    fcolormouse:=c;
    invalidate;
  end;
end;

// ----------------------------------------------------------
procedure tAPI_edit.MouseEnter(var Message: TMessage);
begin
  fmouseover:=true;
  if not (csdesigning in componentstate) then
  if not focused then
  begin
    font.Assign(fmouseoverfont);
    color:=fcolormouse;
  end else
  begin
    font.assign(fdeffont);
    color:=fcolorin;
  end;
  invalidate;
end;

// ----------------------------------------------------------
procedure tAPI_edit.MouseLeave(var Message: TMessage);
begin
  fmouseover:=false;
  if not (csdesigning in componentstate) then
  if not focused then
  begin
    font.Assign(fdeffont);
    color:=fcolorout;
  end else
  begin
    font.assign(feditfont);
    color:=fcolorin;
  end;
  invalidate;
end;

// ----------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_edit]);
end;

end.
