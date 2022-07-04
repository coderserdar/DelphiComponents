unit API_abform;

//------------------------------------------------------------------------------
// fading effect for forms. when placed on any form this component makes
// the fading in and out very easy through component's parameters.
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
// r1.04, 21082006, ari pikivirta
//  * fixed mouse enter and leave events
//
// r1.03, 26.7.2006, ari pikivirta
//  * added OnMouseEnter and OnMouseLeave events
//  * added OnFirstActivate event to do some nice init things if needed
//
// r1.02, ari pikivirta
// * fixed the csdesigning state check to remove reassigning form events
//
// r1.01, ari pikivirta
// * added active property
//
// r1.00, ari pikivirta

interface

uses
  Windows, SysUtils, Messages, Controls, Classes, Forms, ExtCtrls, Types,
  Graphics, API_base;

type
  TAPI_abform = class(TAPI_Custom_Component)
  private
    fform: tform;
    foncreate: tnotifyevent;
    fonshow: tnotifyevent;
    fonhide: tnotifyevent;
    fonactivate: tnotifyevent;
    fonclose: tcloseevent;
    fmouseenter: tnotifyevent;
    fmouseleave: tnotifyevent;
    fonfirstactivate: tnotifyevent;

    foldform_x: integer;
    falphastart: byte;
    falphastop: byte;
    ftimedelay: byte;
    fstepsize: byte;
    factive: boolean;
    ffirstactivated: boolean;
    ftimer: ttimer;
    fmouseover: boolean;

    procedure Fadein;
    procedure Fadeout;
    procedure setactive(b: boolean);
    procedure OnTimer(sender: tobject);

  protected
    procedure FormCreate(sender: tobject);
    procedure FormShow(sender: tobject);
    procedure FormHide(sender: tobject);
    procedure FormActivate(sender: tobject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function  GetForm: TForm;
    procedure Loaded; override;

  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    function MouseIsOver: boolean;

  published
    property Active: boolean read factive write setactive;
    property AlphaStart: byte read falphastart write falphastart;
    property AlphaStop: byte read falphastop write falphastop;
    property TimeDelay: byte read ftimedelay write ftimedelay;
    property StepSize: byte read fstepsize write fstepsize;
    property OnMouseLeave: tnotifyevent read fmouseleave write fmouseleave;
    property OnMouseEnter: tnotifyevent read fmouseenter write fmouseenter;
    property OnFirstActivate: tnotifyevent read fonfirstactivate write fonfirstactivate;

  end;

procedure Register;

implementation

{$r *.res}

const
  versioninfostring: string = 'r1.04/ari.pikivirta@kolumbus.fi';

//------------------------------------------------------------------------------
procedure TAPI_abform.setactive(b: boolean);
begin
  if fform=nil then exit;
  if (b <> factive) then
  begin
    if b then
    begin
      FForm.OnCreate:= FormCreate;
      FForm.OnHide:= FormHide;
      FForm.OnShow:= FormShow;
      FForm.OnActivate:= FormActivate;
      FForm.OnClose:= FormClose;
      FActive:= true;
      fform.AlphaBlend:=true;
      fform.AlphaBlendValue:=falphastart
    end else
    begin
      FForm.OnCreate:= FOnCreate;
      FForm.OnHide:= FOnHide;
      FForm.OnShow:= FOnShow;
      FForm.OnActivate:= FOnActivate;
      FForm.OnClose:= FOnClose;
      FActive:= false;
      fform.AlphaBlendValue:=falphastop;
      fform.AlphaBlend:=false;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_abform.GetForm: TForm;
begin
  if (Owner is TcustomForm) then
    Result:= TForm(Owner as TcustomForm)
    else Result:= nil;
end;

//------------------------------------------------------------------------------
constructor TAPI_abform.create(aowner: tcomponent);
begin
  inherited create(aowner);
  version:= versioninfostring;
  factive:= false;
  falphastart:= 0;
  falphastop:= 255;
  ftimedelay:= 1;
  fstepsize:= 50;
  ffirstactivated:= false;
  fform:= getform;
  fmouseover:= false;
  ftimer:= ttimer.create(self);
  ftimer.interval:= 500;
  ftimer.OnTimer:= OnTimer;
  foldform_x:= -1;

  if fform<>nil then
  begin
    FOnShow:= fform.OnShow;
    FOnCreate:= FForm.OnCreate;
    FOnActivate:= FForm.OnActivate;
    FOnHide:= FForm.OnHide;
    FOnClose:= FForm.OnClose;
    ftimer.Enabled:= true;
  end;
end;

//------------------------------------------------------------------------------
destructor TAPI_abform.destroy;
begin
  setactive(false);
  ftimer.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.fadein;
var
  temp: integer;
begin
  //temp:=falphastart;
  temp:= fform.AlphaBlendValue;

  while (temp<alphastop) do
  begin
    temp:=temp+fstepsize;
    if temp>falphastop then
      temp:=falphastop;
    fform.AlphaBlendValue:=temp;
    sleep(ftimedelay);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.fadeout;
var
  temp: integer;
begin
  //temp:=falphastop-fstepsize;
  temp:= fform.AlphaBlendValue;

  while (temp>falphastart) do
  begin
    temp:=temp-fstepsize;
    if temp<falphastart then
      temp:=falphastart;
    fform.AlphaBlendValue:=temp;
    sleep(ftimedelay);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.FormCreate(sender: tobject);
begin
  if factive then
  begin
    fform.AlphaBlend:= true;
    fform.AlphaBlendValue:= falphastart;
  end;

  if assigned (foncreate) then
    foncreate(sender);
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.FormShow(sender: tobject);
begin
  if factive then
    fadein;

  if assigned (fonshow) then
    fonshow(sender);
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.OnTimer(sender: tobject);
var
  pt, fp: tpoint;
begin
  if not assigned(fform) then exit;

  if fform.Active then
  begin
    getcursorpos(pt);
    fp:= fform.ScreenToClient(fform.ClientOrigin);
    if (pt.X>fp.X) and (pt.X<fp.X+fform.Width) and (pt.y>fp.Y) and (pt.Y<fp.Y+fform.height) then
    begin
      if not fmouseover then
      begin
        fmouseover:= true;
        if assigned(fmouseenter) then
          fmouseenter(self);
      end;
    end else
    begin
      if fmouseover then
      begin
        fmouseover:= false;
        if assigned(fmouseleave) then
          fmouseleave(self);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_abform.MouseIsOver: boolean;
begin
  result:= fmouseover;
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.FormHide(sender: tobject);
begin
  if active then
    fadeout;

  if assigned (fonhide) then
    fonhide(sender);
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.FormActivate(sender: tobject);
begin
//  if factive then
//    fadein;

  if not ffirstactivated then
  begin
    if assigned(fonfirstactivate) then
      fonfirstactivate(self);
    ffirstactivated:= True;
  end;

  if assigned(fonactivate) then
    fonactivate(sender);
end;

//------------------------------------------------------------------------------
procedure TAPI_abform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if active then
    fadeout;

  if assigned(fonclose) then
    fonclose(sender, action);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_abform]);
end;

end.
