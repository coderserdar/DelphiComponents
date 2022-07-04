unit API_logindlg;

//------------------------------------------------------------------------------
// customizable login dialog
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
// 12102004, r1.03, ari pikivirta
// * removed font property (there is no use for that since the form has to
//   be customizable anyway)
//
// 1.02, ari pikivirta
// * added font property

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics;

type
  TAPI_logindlg = class(tcomponent)
  private
    fversion: string;
    ffade: boolean;
    fuser: string;
    fpass: string;
    fclear: boolean;
    fcaption: string;
    ffont: tfont;
    fcolor: tcolor;

    procedure setuser(s:string);
    procedure setpass(s:string);
    procedure setfade(b:boolean);
    procedure setclear(b:boolean);
    procedure setcaption(s:string);
    procedure dummys(s: string);
    procedure setfont(f: tfont);
    procedure setcolor(c: tcolor);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function Execute: boolean;

  published
    property version: string read fversion write dummys stored false;
    property User: string read fuser write setuser;
    property Caption: string read fcaption write setcaption;
    property Fade: boolean read ffade write setfade;
    property Pass: string read fpass write setpass;
    property ClearOnExecute: boolean read fclear write setclear;
    property Color: tcolor read fcolor write setcolor;
    property Font: tfont read ffont write setfont;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  u_logindlg;

const
  versioninfo = 'r1.03/ari.pikivirta@kolumbus.fi';

procedure TAPI_logindlg.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor tAPI_logindlg.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fuser:='';
  fpass:='';
  ffade:=false;
  fcaption:='Login';

  fcolor:= clbtnface;
  ffont:= tfont.Create;
  ffont.Assign( (aowner as tform).Font );
end;

//------------------------------------------------------------------------------
destructor tAPI_logindlg.destroy;
begin
  ffont.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure tAPI_logindlg.setuser(s:string);
begin
  fuser:=s;
end;

//------------------------------------------------------------------------------
procedure tAPI_logindlg.setcaption(s:string);
begin
  fcaption:=s;
end;

//------------------------------------------------------------------------------
procedure tAPI_logindlg.setfade(b:boolean);
begin
  ffade:=b;
end;

//------------------------------------------------------------------------------
procedure tAPI_logindlg.setpass(s:string);
begin
  fpass:=s;
end;

//------------------------------------------------------------------------------
procedure tAPI_logindlg.setclear(b:boolean);
begin
  if b<>fclear then
    fclear:=b;
end;

//------------------------------------------------------------------------------
function tAPI_logindlg.Execute: boolean;
begin
  result:=false;
  u_logindlg.f_logindlg:=tf_logindlg.Create(self);
  try
    u_logindlg.f_logindlg.username:=fuser;
    u_logindlg.f_logindlg.password:=fpass;
    u_logindlg.f_logindlg.clearpass:=fclear;
    u_logindlg.f_logindlg.caption:=fcaption;
    u_logindlg.f_logindlg.API_abform1.Active:=ffade;
    u_logindlg.f_logindlg.Color:= fcolor;
    u_logindlg.f_logindlg.Label1.Font.Assign( ffont );
    u_logindlg.f_logindlg.Label2.Font.Assign( ffont );
    u_logindlg.f_logindlg.API_grbutton1.FontMouseOver.Assign( ffont );
    u_logindlg.f_logindlg.API_grbutton1.Font.Assign( ffont );
    u_logindlg.f_logindlg.API_grbutton2.FontMouseOver.Assign( ffont );
    u_logindlg.f_logindlg.API_grbutton2.Font.Assign( ffont );
    u_logindlg.f_logindlg.Edit1.Font.Assign( ffont );
    u_logindlg.f_logindlg.MaskEdit1.Font.Assign( ffont );
    u_logindlg.f_logindlg.ShowModal;
    if u_logindlg.f_logindlg.login then
    begin
      fuser:=u_logindlg.f_logindlg.username;
      fpass:=u_logindlg.f_logindlg.password;
      result:=true;
    end;
  finally
    u_logindlg.f_logindlg.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_logindlg.setfont(f: tfont);
begin
  ffont.Assign( f );
end;

//------------------------------------------------------------------------------
procedure TAPI_logindlg.setcolor(c: tcolor);
begin
  fcolor:= c;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_logindlg]);
end;

end.
