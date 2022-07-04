unit API_msgdlg;

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
// 14022008, r1.04, ari pikivirta
//  * added some global functions to cover customized "standard" message dialogs
//    with almost basic parameter sets
//
// 12102004, r1.03, ari pikivirta
// * removed font property, because the form is customizable anyway
//
// 1.02, ari pikivirta
// * added font property

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, Dialogs;

type
  TAPI_msgdlg = class(tcomponent)
  private
    fversion: string;
    fcaption: string;
    ffade: boolean;
    fmsg: tstringlist;
    ffont: tfont;
    fcolor: tcolor;
    ftimeout: integer;
    fbuttoncaption: string;

    procedure dummys(s: string);
    procedure setcolor(c: tcolor);
    procedure setfont(f: tfont);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function Execute: boolean;

  published
    property version: string read fversion write dummys stored false;
    property Caption: string read fcaption write fcaption;
    property Fade: boolean read ffade write ffade;
    property Msg: tstringlist read fmsg write fmsg;
    property Font: tfont read ffont write setfont;
    property Color: tcolor read fcolor write setcolor;
    property Timeout: integer read ftimeout write ftimeout;
    property ButtonCaption: string read fbuttoncaption write fbuttoncaption;

  end;

// exported function
function MessageDlg(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  Color: TColor; Font: TFont): integer;

procedure Register;

implementation

{$R *.RES}

uses
  u_msgdlg;

const
  versioninfo = 'r1.04/ari.pikivirta@kolumbus.fi';

procedure TAPI_msgdlg.dummys(s: string); begin end;

//------------------------------------------------------------------------------
// Customized "Standard" Dialog..
//------------------------------------------------------------------------------
function MessageDlg(Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  Color: TColor; Font: TFont): integer;
var
  f: TForm;
begin
  f:= Dialogs.CreateMessageDialog(Msg, DlgType, Buttons);
  f.Color:= Color;
  f.Font.Assign(font);
  result:= f.showmodal;
end;

//------------------------------------------------------------------------------
// COMPONENT
//------------------------------------------------------------------------------
constructor tAPI_msgdlg.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fmsg:=tstringlist.Create;
  ffade:=false;
  fcaption:='Message';
  ffont:= tfont.create;
  ffont.Assign( (aowner as tform).Font );
  fcolor:= clbtnface;
  ftimeout:= 0;
  fbuttoncaption:= 'Ok';
end;

//------------------------------------------------------------------------------
destructor tAPI_msgdlg.destroy;
begin
  fmsg.Free;
  ffont.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function tAPI_msgdlg.Execute: boolean;
var
  i: integer;
begin
  result:=false;
  u_msgdlg.f_msgdlg:=tf_msgdlg.Create(application);
  try
    u_msgdlg.f_msgdlg.Caption:=fcaption;
    u_msgdlg.f_msgdlg.Memo1.clear;
    u_msgdlg.f_msgdlg.Memo1.Color:= fcolor;
    u_msgdlg.f_msgdlg.Color:= fcolor;
    u_msgdlg.f_msgdlg.Font.Assign( ffont );
    u_msgdlg.f_msgdlg.Memo1.Font.Assign( ffont );
    u_msgdlg.f_msgdlg.API_grbutton1.Font.Assign( ffont );
    u_msgdlg.f_msgdlg.API_grbutton1.FontMouseOver.Assign( ffont );
    u_msgdlg.f_msgdlg.timeout:= ftimeout;
    u_msgdlg.f_msgdlg.API_grbutton1.Caption:= fbuttoncaption;
    for i:=0 to fmsg.Count-1 do
      u_msgdlg.f_msgdlg.memo1.lines.add(msg[i]);
    u_msgdlg.f_msgdlg.API_abform1.Active:=ffade;
    u_msgdlg.f_msgdlg.ShowModal;
  finally
    u_msgdlg.f_msgdlg.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_msgdlg.setcolor(c: tcolor);
begin
  fcolor:= c;
end;

//------------------------------------------------------------------------------
procedure TAPI_msgdlg.setfont(f: tfont);
begin
  ffont.Assign( f );
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_msgdlg]);
end;

end.
