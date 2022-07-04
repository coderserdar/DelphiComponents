unit API_PageControl;

//------------------------------------------------------------------------------
// API_PageControl
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, Controls, Classes, CommCtrl, ComCtrls, Graphics;

type
  TAPI_PageControl = class(TPageControl)
  private
    fversion: String;
    fhighlightfont: tfont;
    fhighlightcolor: tcolor;
    fhighlightedtabs: integer; // 0..63 tabs
    factivefont: tfont;
    factivecolor: tcolor;
    procedure Dummys(Const S: String);
    procedure SetActiveFont(Const F: TFont);
    procedure SetActiveColor(Const C: TColor);
    procedure SetHighlightFont(Const F: TFont);
    procedure SetHighlightColor(Const C: TColor);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    function  GetSheetRect: TRect;
    procedure SetHighlightedTabs(Const I: integer);
  protected
    //procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: string read fversion write dummys stored FALSE;
    property FontActive: tfont read factivefont write SetActiveFont;
    property ColorActive: tcolor read factivecolor write SetActiveColor;
    property FontHighlight: tfont read fhighlightfont write SetHighlightFont;
    property ColorHighlight: tcolor read fhighlightcolor write SetHighlightColor;
    property Highlighted: integer read fhighlightedtabs write SetHighlightedTabs;
  end;

procedure Register;

implementation

uses
  api_base;

//------------------------------------------------------------------------------
constructor TAPI_PageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];
  self.OwnerDraw:= TRUE;
  factivefont:= tfont.create;
  factivefont.assign(font);
  //factivefont.Style:= factivefont.style + [fsBold];
  factivecolor:= color;
  fhighlightfont:= tfont.create;
  fhighlightfont.assign(font);
  fhighlightcolor:= color;
  fhighlightedtabs:= 0;
end;

//------------------------------------------------------------------------------
destructor TAPI_PageControl.Destroy;
begin
  factivefont.free;
  fhighlightfont.free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.Dummys(Const S: String);
begin
  // does nothing
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.SetActiveFont(Const F: TFont);
begin
  factivefont.assign(f);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.SetHighlightColor(Const C: TColor);
begin
  if (c<>fHighlightcolor) then
  begin
    fHighlightcolor:= C;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.SetHighlightFont(Const F: TFont);
begin
  fHighlightfont.assign(f);
  invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.SetActiveColor(Const C: TColor);
begin
  if (c<>factivecolor) then
  begin
    factivecolor:= C;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.CMParentColorChanged (var Message: TMessage);
var
  Is1, Is2: Boolean;
begin
  Is1 := ( factivecolor = Color );
  Is2 := ( fhighlightcolor = Color );
  inherited;
  if Is1 and ( factivecolor <> Color ) then SetActiveColor(Color);
  if Is2 and ( fhighlightcolor <> Color ) then SetHighlightColor(Color);
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.CMParentFontChanged (var Message: TMessage);
var
  Is1, Is2: Boolean;
  //
  function IsFontEqual (Font1, Font2: TFont): Boolean;
  begin
    Result := ( Font1.Name = Font2.Name ) and
      ( Font1.Size = Font2.Size ) and
      ( Font1.Style = Font2.Style ) and
      ( Font1.Color  = Font2.Color );
  end;
  //
begin
  Is1 := IsFontEqual (factivefont, Font);
  Is2 := IsFontEqual (fhighlightfont, Font);
  inherited;
  if Is1 and not IsFontEqual (factivefont, Font) then setactivefont(font);
  if Is2 and not IsFontEqual (fhighlightfont, Font) then sethighlightfont(font);
  Invalidate;
end;

//------------------------------------------------------------------------------
function TAPI_PageControl.GetSheetRect: TRect;
var
  TabRect: TRect;

begin
  // Get tab rectangle
  SendMessage (Handle, TCM_GETITEMRECT, 0, Longint(@TabRect));
  // Calculate sheet rectangle
  Result := Rect (2, TabRect.Bottom - TabRect.Top + 4, Width-3, Height-2);
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.SetHighlightedTabs(Const I: integer);
begin
  if fhighlightedtabs<>i then
  begin
    fhighlightedtabs:= I;
    invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  arect: trect;
begin
  if (TabIndex < 0) then Exit;

  // set font and colors
  if (Active) then
  begin
    Canvas.Font := factivefont;
    Canvas.Brush.Color := factivecolor;
  end else
  if (bitisset(fhighlightedtabs, TabIndex)) then
  begin
    Canvas.Font := fhighlightfont;
    Canvas.Brush.Color := fhighlightcolor;
  end else
  begin
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
  end;

  // draw background
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect (Rect);

  // draw text
  ARect:= Rect;
  DrawText(Canvas.Handle, PAnsiChar(Tabs[TabIndex]), length(tabs[TabIndex]), ARect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
end;

//------------------------------------------------------------------------------
procedure TAPI_PageControl.Change;
begin
  inherited Change;
  invalidate;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Vcl', [TAPI_PageControl]);
end;

end.
