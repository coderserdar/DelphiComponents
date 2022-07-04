unit API_inedit;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Messages, Classes, Controls, StdCtrls, Types, Windows;

type
  TAPI_inedit = class(TEdit)
  private
    fversion: string;

    fonlynum: boolean;
    falignment: talignment;
    fwordwrap: boolean;

    fexitonenter: boolean;
    fexitonesc: boolean;
    fexitontab: boolean;
    fexitonarrows: boolean;
    fexitonpg: boolean;

    fgoingoff: tnotifyevent;

    MEnter : TNotifyEvent;
    MLeave : TNotifyEvent;

    procedure CNkeydown(var Message: TWMkeydown); message CN_KEYDOWN;
    Procedure CMEnter( Var Message : TCMEnter );   message CM_ENTER;
    Procedure CMExit( Var Message : TCMExit );   message CM_EXIT;

    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure setwordwrap(b: boolean);
    procedure setalignment(a:talignment);

    procedure dummys(s: string);

  protected
    procedure CreateParams(var params: TCreateParams); override;

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    procedure KeyPress(var Key: Char); override;

    procedure Open(rec: trect);
    function asFloat: double;
    function asInteger: integer;
    procedure ExitNow;

  published
    property Version: string read fversion write dummys stored false;

    property OnlyNumbers: boolean read fonlynum write fonlynum;

    property Wordwrap: boolean read fwordwrap write setwordwrap;
    property Alignment: talignment read falignment write setalignment;

    property OnMouseEnter: TNotifyEvent read MEnter write MEnter;
    property OnMouseLeave: TNotifyEvent read MLeave write MLeave;

    property ExitonEnter: boolean read fexitonenter write fexitonenter;
    property ExitonEsc: boolean read fexitonesc write fexitonesc;
    property ExitonArrows: boolean read fexitonarrows write fexitonarrows;
    property ExitonTab: boolean read fexitontab write fexitontab;
    property ExitonPg: boolean read fexitonpg write fexitonpg;

    property OnHide: tnotifyevent read fgoingoff write fgoingoff;

  end;

procedure Register;

implementation

{$r *.res}

const
 versioninfostring: string = 'r1.00/ari.pikivirta@kolumbus.fi';

procedure TAPI_inedit.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor TAPI_inedit.create(aowner: tcomponent);
begin
  inherited create(aowner);

  if not (csdesigning in componentstate) then
    visible:=false;

  autosize:=false;
  height:=16;
  width:=16;

  falignment:=taleftjustify;
  fwordwrap:=true;
  fonlynum:=false;

  fversion:=versioninfostring;
  fexitonenter:=true;
  fexitonesc:=false;
  fexitontab:=true;
  fexitonarrows:=false;
  fexitonpg:=true;

  text:='';
end;

//------------------------------------------------------------------------------
destructor TAPI_inedit.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.Open(rec: trect);
begin
  left:=rec.Left;
  Top:=rec.Top;
  height:=rec.Bottom-rec.Top;
  width:=rec.Right-rec.Left;
  Visible:=true;
  setfocus;
end;

//------------------------------------------------------------------------------
function TAPI_inedit.asFloat: double;
begin
  try
    result:=strtofloat(text);
  except
    result:=0;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_inedit.asInteger: Integer;
begin
  try
    result:=strtoint(text);
  except
    result:=0;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_inedit.createparams(var params: TCreateParams);
const Alignments: array[TAlignment] of word=(ES_LEFT,ES_RIGHT,ES_CENTER);
      WordWraps: array[Boolean] of longint = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(params);
  Params.Style:=Params.Style and (not WordWraps[FWordWrap])
    or Alignments[FAlignment] or ES_MULTILINE;
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.KeyPress(var Key: Char);
begin
  if fonlynum then
  begin
    if not (Key in ['0'..'9',decimalseparator,'+','-',#8,#13]) then
    key:=#0;
  end;
  inherited keypress(key);
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.CNkeydown(var Message: TWMkeydown);
begin
  case message.CharCode of
    VK_TAB: if fexitontab then exitnow;
    VK_RETURN: if fexitonenter then exitnow;
    VK_ESCAPE: if fexitonesc then exitnow;
    VK_UP: if fexitonarrows then exitnow;
    VK_DOWN: if fexitonarrows then exitnow;
    VK_LEFT: if fexitonarrows then exitnow;
    VK_RIGHT: if fexitonarrows then exitnow;
    VK_PRIOR: if fexitonpg then exitnow;
    VK_NEXT: if fexitonpg then exitnow;
  end;
end;

//------------------------------------------------------------------------------
Procedure TAPI_inedit.CMEnter( Var Message: TCMEnter );
begin
  inherited;
end;

//------------------------------------------------------------------------------
Procedure TAPI_inedit.CMExit( Var Message: TCMExit );
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.ExitNow;
begin
  visible:=false;
  if assigned(fgoingoff) then fgoingoff(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.MouseEnter(var Message: TMessage);
begin
  if assigned(menter) then
    menter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.MouseLeave(var Message: TMessage);
begin
  if assigned(mleave) then
    mleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.setwordwrap(b: boolean);
begin
  if b<>fwordwrap then
  begin
    fwordwrap:=b;
    recreatewnd;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_inedit.setalignment(a:talignment);
begin
  if a<>falignment then
  begin
    falignment:=a;
    recreatewnd;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Old', [TAPI_inedit]);
end;

end.
