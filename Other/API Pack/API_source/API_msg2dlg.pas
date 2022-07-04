unit API_msg2dlg;

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
// 11082006, r1.04, ari pikivirta
//  * changed internal form creation as try..finally to make sure form
//    is unloaded on app close
//
// 12102004, r1.03, ari pikivirta
// * removed font property, because of the form is customizable anyway
//
// 1.02, ari pikivirta
// * added font property

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics;

type
  tmsg2state = (m2hide, m2show);

  TAPI_msg2dlg = class(tcomponent)
  private
    fversion: string;
    fcaption: string;
    ffade: boolean;
    fmsg: tstringlist;
    fstate: tmsg2state;
    ffont: tfont;
    fcolor: tcolor;

    procedure setstate(s: tmsg2state);
    procedure setmsg(sl: tstringlist);
    procedure setcaption(s: string);
    procedure dummys(s: string);
    procedure setfont(f: tfont);
    procedure setcolor(c: tcolor);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure Update;

  published
    property version: string read fversion write dummys stored false;
    property Caption: string read fcaption write setcaption;
    property Fade: boolean read ffade write ffade;
    property Msg: tstringlist read fmsg write setmsg;
    property State: tmsg2state read fstate write setstate;
    property Font: tfont read ffont write setfont;
    property Color: tcolor read fcolor write setcolor;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  u_msg2dlg;

const
  versioninfo = 'r1.03/ari.pikivirta@kolumbus.fi';

procedure TAPI_msg2dlg.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor tAPI_msg2dlg.create(aowner:tcomponent);
begin
  inherited create(aowner);
  ffade:=false;
  fstate:=m2hide;
  fversion:=versioninfo;
  fmsg:=tstringlist.Create;
  fcaption:='Message';
  ffont:= tfont.create;
  ffont.assign( (aowner as tform).Font );
  fcolor:= clbtnface;
end;

//------------------------------------------------------------------------------
procedure tAPI_msg2dlg.setstate(s:tmsg2state);
begin
  if (assigned(f_msg2dlg)) and (s<>fstate) then
  begin
    fstate:=s;
    f_msg2dlg.API_abform1.Active:=ffade;
    if fstate=m2show then show
      else hide;
  end;
end;

//------------------------------------------------------------------------------
destructor tAPI_msg2dlg.destroy;
begin
  if assigned(f_msg2dlg) then
  begin
    f_msg2dlg.Free;
    f_msg2dlg:= nil;
  end;
  fmsg.Free;
  ffont.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_msg2dlg.setmsg(sl: tstringlist);
begin
  if (sl.text<>fmsg.text) then
  begin
    fmsg.text:= sl.text;
    update;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_msg2dlg.setcaption(s: string);
begin
  if (s<>fcaption) then
  begin
    fcaption:= s;
    update;
  end;
end;

//------------------------------------------------------------------------------
procedure tAPI_msg2dlg.show;
begin
  f_msg2dlg:=tf_msg2dlg.Create(application);
  try
    update;
    f_msg2dlg.show;
    while f_msg2dlg.showing do
      application.processmessages;
  finally
    f_msg2dlg.free;
    f_msg2dlg:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure tapi_msg2dlg.hide;
begin
  if assigned(f_msg2dlg) then
    f_msg2dlg.close;
end;

//------------------------------------------------------------------------------
procedure tAPI_msg2dlg.update;
var
  i: integer;
begin
  if assigned(f_msg2dlg) then
  begin
    f_msg2dlg.Memo1.Font.assign( ffont );
    f_msg2dlg.Color:= fcolor;
    f_msg2dlg.Memo1.Color:= fcolor;
    f_msg2dlg.Caption:=fcaption;
    f_msg2dlg.API_abform1.Active:= ffade;
    f_msg2dlg.memo1.lines.BeginUpdate;
    try
      f_msg2dlg.Memo1.clear;
      for i:=0 to fmsg.Count-1 do
        f_msg2dlg.memo1.lines.add(msg[i]);
    finally
      f_msg2dlg.Memo1.Lines.EndUpdate;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_msg2dlg.setfont(f: tfont);
begin
  ffont.assign( f );
  update;
end;

//------------------------------------------------------------------------------
procedure TAPI_msg2dlg.setcolor(c: tcolor);
begin
  fcolor:= c;
  update;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_msg2dlg]);
end;

end.
