unit API_memoquery;

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
// 12102004, r1.02, ari pikivirta
// * removed font property, because the form is customizable anyway
//
// r1.01, ari pikivirta
// * fixed fading of dialog
// * added memo font property

interface

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics;

type
  TAPI_memoquery = class(tcomponent)
  private
    fversion: string;
    ffade: boolean;
    fmemo: string;
    fclear: boolean;
    fcaption: string;

    procedure setmemo(s:string);
    procedure setfade(b:boolean);
    procedure setclear(b:boolean);
    procedure setcaption(s:string);
    procedure dummys(s: string);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;
    function Execute: boolean;

  published
    property Version: string read fversion write dummys stored false;
    property Memo: string read fmemo write setmemo;
    property Caption: string read fcaption write setcaption;
    property Fade: boolean read ffade write setfade;
    property Clear: boolean read fclear write setclear;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  u_memoquery;

const
  versioninfo = 'r1.02/ari.pikivirta@kolumbus.fi';

procedure TAPI_memoquery.dummys(s: string); begin end;

//------------------------------------------------------------------------------
constructor tAPI_memoquery.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fmemo:='';
  ffade:=false;
  fclear:=true;
  fcaption:='Query';
end;

//------------------------------------------------------------------------------
destructor tAPI_memoquery.destroy;
begin
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure tAPI_memoquery.setmemo(s:string);
begin
  fmemo:=s;
end;

//------------------------------------------------------------------------------
procedure tAPI_memoquery.setcaption(s:string);
begin
  fcaption:=s;
end;

//------------------------------------------------------------------------------
procedure tAPI_memoquery.setfade(b:boolean);
begin
  ffade:=b;
end;

//------------------------------------------------------------------------------
procedure tAPi_memoquery.setclear(b:boolean);
begin
  fclear:=b;
end;

//------------------------------------------------------------------------------
function tAPI_memoquery.Execute: boolean;
begin
  result:=false;
  u_memoquery.f_memoquery:=tf_memoquery.Create(application);
  try
    u_memoquery.f_memoquery.memo:=fmemo;
    u_memoquery.f_memoquery.clear:=fclear;
    u_memoquery.f_memoquery.caption:=fcaption;
    u_memoquery.f_memoquery.API_abform1.Active:=ffade;
    u_memoquery.f_memoquery.ShowModal;
    if u_memoquery.f_memoquery.memo<>'' then
    begin
      fmemo:=u_memoquery.f_memoquery.memo;
      result:=true;
    end else
      fmemo:='';
  finally
    u_memoquery.f_memoquery.free;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_memoquery]);
end;

end.
