unit API_confirmdlg;

//------------------------------------------------------------------------------
// customizable confirm dialog - with multiline message and fading.
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
// 26082004, r1.04, ari pikivirta
// * fixed accesviolation error caused by not existing font
// * removed font
//
// r1.03, ari pikivirta
// * added font property
//
// r1.02, ari pikivirta
// * revision change for correct email
// * removed unneeded procedures

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, API_base;

type
  tconfirmationmode = (c_yes, c_apply);

  TAPI_confirmdlg = class(TAPI_Custom_Component)
  private
    ffade: boolean;
    fcaption: string;
    fmode: tconfirmationmode;
    fmsg: tstringlist;
    fresult: boolean;
  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function Execute: boolean; overload;
    function Execute(title, text: string): boolean; overload;
  published
    property Caption: string read fcaption write fcaption;
    property Msg: tstringlist read fmsg write fmsg;
    property Fade: boolean read ffade write ffade;
    property Mode: tconfirmationmode read fmode write fmode;
    property Result: boolean read fresult write fresult;
  end;

procedure Register;

implementation

{$R *.RES}

uses
  u_confirmdlg;

const
  versioninfo = 'r1.04/ari.pikivirta]t[kolumbus.fi';

//------------------------------------------------------------------------------
constructor tAPI_confirmdlg.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:=versioninfo;
  ffade:=false;
  fmsg:=tstringlist.Create;
  fcaption:='Confirmation';
end;

//------------------------------------------------------------------------------
destructor tAPI_confirmdlg.destroy;
begin
  fmsg.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function tAPI_confirmdlg.Execute: boolean;
var
  i: integer;
begin
  u_confirmdlg.f_confirmdlg:=tf_confirmdlg.Create(self);
  try
    u_confirmdlg.f_confirmdlg.Caption:=fcaption;
    u_confirmdlg.f_confirmdlg.mode:=byte(fmode);
    u_confirmdlg.f_confirmdlg.Memo1.clear;
    for i:=0 to fmsg.Count-1 do
      u_confirmdlg.f_confirmdlg.memo1.lines.add(msg[i]);
    u_confirmdlg.f_confirmdlg.API_abform1.Active:= ffade;
    u_confirmdlg.f_confirmdlg.ShowModal;
    fresult:=u_confirmdlg.f_confirmdlg.answer;
    execute:=fresult;
  finally
    u_confirmdlg.f_confirmdlg.Free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_confirmdlg.Execute(title, text: string): boolean;
begin
  u_confirmdlg.f_confirmdlg:= tf_confirmdlg.Create(self);
  try
    u_confirmdlg.f_confirmdlg.Caption:= title;
    u_confirmdlg.f_confirmdlg.mode:= byte(fmode);
    u_confirmdlg.f_confirmdlg.Memo1.Text:= text;
    u_confirmdlg.f_confirmdlg.API_abform1.Active:= ffade;
    u_confirmdlg.f_confirmdlg.ShowModal;
    fresult:= u_confirmdlg.f_confirmdlg.answer;
    execute:= fresult;
  finally
    u_confirmdlg.f_confirmdlg.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_confirmdlg]);
end;

end.
