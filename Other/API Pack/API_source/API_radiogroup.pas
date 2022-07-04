unit API_radiogroup;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// r1.01, 07052008, ari pikivirta
//  * added onmouseleave and onmouseenter events
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, API_base, Graphics,
  Messages;

type
  TAPI_radiogroup = class(TCustomRadioGroup)
  private
    { Private declarations }
    fversion: string;
    //fcaption: string;
    fcaptionfont: TFont;
    fonmouseenter: tnotifyevent;
    fonmouseleave: tnotifyevent;
    procedure Dummys(s: string);
    //procedure setcaption(s: string);
    procedure setcaptionfont(f: tfont);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    //property Caption: string read fcaption write setcaption;
    property Caption;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Items;
    property OnMouseEnter: tnotifyevent read fonmouseenter write fonmouseenter;
    property OnMouseLeave: tnotifyevent read fonmouseleave write fonmouseleave;
  end;

procedure Register;

implementation

{$R *.RES}

//------------------------------------------------------------------------------
constructor TAPI_radiogroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fversion:= 'r1.01/ari.pikivirta@kolumbus.fi';
  fcaptionfont:= tfont.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_radiogroup.Destroy;
begin
  fcaptionfont.free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_radiogroup.Dummys(s: string);
begin
  // does nothing
end;

//------------------------------------------------------------------------------
procedure TAPI_radiogroup.setcaptionfont(f: tfont);
begin
  if f<>captionfont then
  begin
    fcaptionfont.Assign(f);
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_radiogroup.MouseEnter(var Message: TMessage);
begin
  if assigned(fonmouseenter) then
    fonmouseenter(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_radiogroup.MouseLeave(var Message: TMessage);
begin
  if assigned(fonmouseleave) then
    fonmouseleave(self);
end;

//------------------------------------------------------------------------------
procedure TAPI_radiogroup.Paint;
var
  h: Integer;
  r: TRect;
begin
  canvas.Font:= captionfont;

  h:= canvas.textheight('0');
  R:= Rect(0, H div 2 - 1, Width, Height);
  if Ctl3D then
  begin
    Inc(R.Left);
    Inc(R.Top);
    canvas.Brush.Color:= clBtnHighlight;
    canvas.FrameRect(R);
    OffsetRect(R, -1, -1);
    canvas.Brush.Color := clBtnShadow;
  end;

  canvas.Brush.Color := clWindowFrame;
  canvas.FrameRect(R);

  R:= Rect(8, 0, 0, H);
  DrawText(canvas.Handle, pchar(caption), length(caption), R, DT_LEFT or DT_SINGLELINE or DT_CALCRECT);
  canvas.Brush.Color:= Color;
  DrawText(canvas.Handle, pchar(caption), length(caption), R, DT_LEFT or DT_SINGLELINE);
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_radiogroup]);
end;

end.
