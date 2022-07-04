unit API_numedit;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// r1.02, ari pikivirta
// * added desinging state regognition

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, graphics;

type

  TAPI_numedit = class(TEdit)
  private
    fversion: string;
    falignment: talignment;
    fwordwrap: boolean;
    fmultiline: boolean;
    fcolorin: tcolor;
    fcolorout: tcolor;
    fcolormouse: tcolor;
    MEnter : TNotifyEvent;
    MLeave : TNotifyEvent;
    procedure setwordwrap(w: boolean);
    procedure setalignment(a: talignment);
    procedure setcolorin(c: tcolor);
    procedure setcolorout(c: tcolor);
    procedure setmultiline(b: boolean);
    procedure setcolormouse(c: tcolor);
    procedure dummys(s: string);

  protected
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CreateParams(var params: TCreateParams); override;
    procedure KeyPress(var Key: Char); override;

  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function asInteger: integer;
    function asFloat: double;

  published
    property Version: string read fversion write dummys stored false;
    property Wordwrap: boolean read fwordwrap write setwordwrap;
    property Alignment: talignment read falignment write setalignment;
    property ColorIn: tcolor read fcolorin write setcolorin;
    property ColorOut: tcolor read fcolorout write setcolorout;
    property Multiline: boolean read fmultiline write setmultiline;
    property ColorMouse: tcolor read fcolormouse write setcolormouse;
    property OnMouseEnter: TNotifyEvent read MEnter write MEnter;
    property OnMouseLeave: TNotifyEvent read MLeave write MLeave;
    property Color;

  end;

procedure Register;

implementation

const
  versioninfo = 'r1.02/ari.pikivirta@kolumbus.fi';

{$r *.res}

procedure TAPI_numedit.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor tAPI_numedit.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fwordwrap:=false;
  falignment:=tarightjustify;
  fcolorin:=cllime;
  fcolorout:=clwhite;
  fcolormouse:=clyellow;
  fmultiline:=false;
  width:=80;
  height:=22;
  text:='0';
end;

//------------------------------------------------------------------------------
destructor tAPI_numedit.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.createparams(var params: TCreateParams);
const Alignments: array[TAlignment] of word=(ES_LEFT,ES_RIGHT,ES_CENTER);
      WordWraps: array[Boolean] of longint = (0, ES_AUTOHSCROLL);
begin
  inherited CreateParams(params);
  Params.Style:=Params.Style and (not WordWraps[FWordWrap])
    or Alignments[FAlignment];
  if fmultiline then params.style:=params.style or ES_MULTILINE;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.KeyPress(var Key: Char);
begin
  if not (Key in ['0'..'9',decimalseparator,'+','-',#8,#13]) then
    key:=#0;
  inherited keypress(key);
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setwordwrap(w:boolean);
begin
  if fwordwrap<>w then
  begin
    fwordwrap:=w;
    recreatewnd;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setalignment(a:talignment);
begin
  if falignment<>a then
  begin
    falignment:=a;
    recreatewnd;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setcolorin(c: tcolor);
begin
  if fcolorin<>c then
  begin
    fcolorin:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setcolorout(c: tcolor);
begin
  if fcolorout<>c then
  begin
    fcolorout:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.doenter;
begin
  if not (csdesigning in componentstate) then
    color:=fcolorin;
  inherited;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.doexit;
begin
  if not (csdesigning in componentstate) then
    color:=fcolorout;
  inherited;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setmultiline(b:boolean);
begin
  if b<>fmultiline then
  begin
    fmultiline:=b;
    recreatewnd;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.setcolormouse(c: tcolor);
begin
  if c<>fcolormouse then
  begin
    fcolormouse:=c;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.MouseEnter(var Message: TMessage);
begin
  if csdesigning in componentstate then exit;
  if not focused then
    color:=fcolormouse;
end;

//------------------------------------------------------------------------------
procedure tAPI_numedit.MouseLeave(var Message: TMessage);
begin
  if csdesigning in componentstate then exit;
  if not focused then
    color:=fcolorout;
end;

//------------------------------------------------------------------------------
function tAPI_numedit.asInteger: integer;
var
  s: string;
begin
  s:=text;
  try
    result:=trunc(strtofloat(s));
  except
    result:=0;
  end;
end;

//------------------------------------------------------------------------------
function tAPI_numedit.asFloat: double;
var
  s: string;
begin
  s:=text;
  try
    result:=strtofloat(s);
  except
    result:=0;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Old', [TAPI_numedit]);
end;

end.
