////////////////////////////////////////////////////////////////////////////////
// Component.......: TImageButton v.1.1
// Author..........: Grzegorz Marek Molenda aka NevTon
// Creation.Date...: 30.04.2007
// Modified.Date...: 11.07.2007
// Copyright.......: ViTE Software Solutions, (C) 1998 - 2007
// E-mail..........: gmnevton@o2.pl
// News.group......: pl.comp.lang.delphi
// Web.page........: vitesoft.net or vitesoft.prv.pl
// Status..........: Public Domain
////////////////////////////////////////////////////////////////////////////////
// Any changes in unit code allowed, however, please inform the author best
// through e-mail about performed changes and enclose code corrections,
// it will allow to uniformity unit and controls over versions.
////////////////////////////////////////////////////////////////////////////////
// Prosze NIE USUWAC danych autora, mozna za to samemu sie tu dopisac!
// Please DO NOT delete data of author, it is possible to add yourself too here!
// -----------------------------------------------------------------------------
// Wspolautorzy....:
// Co-authors......:
////////////////////////////////////////////////////////////////////////////////
// 11.07.2007......: Fix - Popup menu shows when right clicked
//                   Bug - Image blinks when Transparent is set to true, turn it off and You don't have this problem!
//                   Add - Property ParentBkgndColor to fill background to parent color
//                   Add - GetParentControlColor function shows how to obtain protected property Color value from TControl!
////////////////////////////////////////////////////////////////////////////////

unit ImageButton;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms;

type
  TPictureInitialState = (isNormal, isDarken);
  TButtonOnEnterState = (esDoNothing, esLighten, esNormal, esMakeShadow);
//  TPictureCenterState = (csNone, csVertical, csHorizontal, csMiddle);
  TButtonOnPopupState = (psAfterLMouseUp, psManual);
  TButtonShadowState = (ssNone, ssSkypeStyle, ssGlyphShadow);
  TPictureLightState = (ls10Percent, ls20Percent, ls30Percent, ls40Percent, ls50Percent);

  TImageButton = class(TGraphicControl)
  private
    FGlyph: TBitmap;
    FPicture: TBitmap;
    FDrawing: Boolean;
    FTransparent: Boolean;
    FParentBkgndColor: Boolean;
    FTransparentColor: TColor;
    FToolButton: Boolean;
    FInitialState: TPictureInitialState;
    FOnEnterState: TButtonOnEnterState;
//    FCenterState: TPictureCenterState;
    FPopupState: TButtonOnPopupState;
    FShadowState: TButtonShadowState;
    FLightState: TPictureLightState;
    FMoveWhenPressed: Boolean;
    FMovePoint: TPoint;
    FShadowColor: TColor;

    FEntered: Boolean;
    FDown: Boolean;

    FOnMouseEnter,
    FOnMouseLeave,
    FOnMouseDown,
    FOnMouseUp: TNotifyEvent;

    function  GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(Value: TBitmap);
    procedure SetTransparent(Value: Boolean);
    procedure SetParentBkgndColor(Value: Boolean);
    procedure SetInitialState(Value: TPictureInitialState);
    procedure SetOnEnterState(Value: TButtonOnEnterState);
//    procedure SetCenterState(Value: TPictureCenterState);
    procedure SetPopupState(Value: TButtonOnPopupState);
    procedure SetShadowState(Value: TButtonShadowState);
    procedure SetLightState(Value: TPictureLightState);
    procedure SetToolButton(Value: Boolean);
    procedure SetShadowColor(Value: TColor);
    procedure SetMoveWhenPressed(Value: Boolean);
  protected
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function  DestRect: TRect;
    function  DoPaletteChange: Boolean;
    function  GetPalette: HPALETTE; override;
    procedure Paint; override;

    procedure WMMouseEnter(var Msg: TWMMouse); message CM_MOUSEENTER;
    procedure WMMouseLeave(var Msg: TWMMouse); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Msg: TWMLButtonUp); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  published
//    property Align;
//    property AutoSize;
    property Anchors;
//    property Constraints;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;

    property Picture: TBitmap read FPicture write SetPicture stored True;

    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property ParentBkgndColor: Boolean read FParentBkgndColor write SetParentBkgndColor default True;
    property InitialState: TPictureInitialState read FInitialState write SetInitialState default isNormal;
    property OnEnterState: TButtonOnEnterState read FOnEnterState write SetOnEnterState default esDoNothing;
//    property CenterState: TPictureCenterState read FCenterState write SetCenterState default csNone;
    property PopupState: TButtonOnPopupState read FPopupState write SetPopupState default psAfterLMouseUp;
    property ShadowState: TButtonShadowState read FShadowState write SetShadowState default ssNone;
    property LightState: TPictureLightState read FLightState write SetLightState default ls30Percent;
    property ToolButton: Boolean read FToolButton write SetToolButton default False;
    property MoveWhenPressed: Boolean read FMoveWhenPressed write SetMoveWhenPressed default True;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default $00B1AFAC;

    //** Events **//
    property OnMouseLBtnDown: TNotifyEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseLBtnUp: TNotifyEvent read FOnMouseUp write FOnMouseUp;
  end;

procedure Register;

implementation

{$R *.RES}

uses Consts, Types, GraphUtil;

procedure Register;
begin
  RegisterComponents('Standard', [TImageButton]);
end;

////////////////////////////////////////////////////////////////////////////////

function IntToByte(i:Integer):Byte;
begin
  if i > 255 then
    Result := 255
  else if i < 0 then
    Result := 0
  else
    Result := i;
end;

procedure Darkness(Bitmap: TBitmap; Amount: Integer);
var
  ByteArray: PByteArray;
  r,g,b,x,y: Integer;
  PixelColor: TColor;
begin
  Bitmap.PixelFormat:=pf24bit;
  PixelColor:=Bitmap.Canvas.Pixels[0, Bitmap.Height-1];
  for y:=0 to Bitmap.Height-1 do
    begin
      ByteArray:=Bitmap.ScanLine[y];
      for x:=0 to Bitmap.Width-1 do
        begin
          if Bitmap.Canvas.Pixels[x, y] <> PixelColor then
            begin
              r:=ByteArray[x*3];
              g:=ByteArray[x*3+1];
              b:=ByteArray[x*3+2];
              ByteArray[x*3]:=IntToByte(r-((r)*Amount)div 255);
              ByteArray[x*3+1]:=IntToByte(g-((g)*Amount)div 255);
              ByteArray[x*3+2]:=IntToByte(b-((b)*Amount)div 255);
            end;
        end;
    end;
end;

procedure Lightness(Bitmap: TBitmap; Amount: Integer);
var
  ByteArray: PByteArray;
  r,g,b,x,y: Integer;
  PixelColor: TColor;
begin
  Bitmap.PixelFormat:=pf24bit;
  PixelColor:=Bitmap.Canvas.Pixels[0, Bitmap.Height-1];
  for y:=0 to Bitmap.Height-1 do
    begin
      ByteArray:=Bitmap.ScanLine[y];
      for x:=0 to Bitmap.Width-1 do
        begin
          if Bitmap.Canvas.Pixels[x, y] <> PixelColor then
            begin
              r:=ByteArray[x*3];
              g:=ByteArray[x*3+1];
              b:=ByteArray[x*3+2];
              ByteArray[x*3]:=IntToByte(r+((255-r)*Amount)div 255);
              ByteArray[x*3+1]:=IntToByte(g+((255-g)*Amount)div 255);
              ByteArray[x*3+2]:=IntToByte(b+((255-b)*Amount)div 255);
            end;
        end;
    end;
end;

procedure GrayScale(Bitmap: TBitmap);
var
  Row: ^TRGBTriple;
  x,y,Index: Integer;
  PixelColor: TColor;
begin
  Bitmap.PixelFormat:=pf24bit;
  PixelColor:=Bitmap.Canvas.Pixels[0, Bitmap.Height-1];
  for y:=0 to Bitmap.Height-1 do
    begin
      Row:=Bitmap.ScanLine[y];
      for x:=0 to Bitmap.Width-1 do
        begin
          if Bitmap.Canvas.Pixels[x, y] <> PixelColor then
            begin
              Index:=((Row.rgbtRed*77 + Row.rgbtGreen*150 + Row.rgbtBlue*29) shr 8);
              Row.rgbtBlue:=Index;
              Row.rgbtGreen:=Index;
              Row.rgbtRed:=Index;
            end;
          Inc(Row);
        end;
    end;
end;

procedure ConvertBitmapToTransparent(Bitmap: TBitmap; BgColor: TColor);
var
  x, y: Integer;
  PixelColor: TColor;
begin
  with Bitmap do
    begin
      PixelColor:=Canvas.Pixels[0, Height-1];
      for y:=0 to Height-1 do
        for x:=0 to Width-1 do
          if Canvas.Pixels[x, y] = PixelColor then Canvas.Pixels[x, y]:=BgColor;
    end;
end;

procedure TransparentDraw(Bitmap: TBitmap; Source: TBitmap); overload;
var
  x, y: Integer;
  PixelColor: TColor;
begin
  if (Bitmap.Width <> Source.Width) or (Bitmap.Height <> Source.Height) then Exit;
  with Bitmap do
    begin
      PixelColor:=Source.Canvas.Pixels[0, Source.Height-1];
      for y:=0 to Source.Height-1 do
        for x:=0 to Source.Width-1 do
          if Source.Canvas.Pixels[x, y] <> PixelColor then Canvas.Pixels[x, y]:=Source.Canvas.Pixels[x, y];
    end;
end;

procedure TransparentDraw(Canvas: TCanvas; Source: TBitmap; DestPt: TPoint); overload;
var
  x, y: Integer;
  PixelColor: TColor;
begin
  if (Source.Width = 0) or (Source.Height = 0) then Exit;
  with Canvas do
    begin
      PixelColor:=Source.Canvas.Pixels[0, Source.Height-1];
      for y:=0 to Source.Height-1 do
        for x:=0 to Source.Width-1 do
          if Source.Canvas.Pixels[x, y] <> PixelColor then Pixels[DestPt.X+x, DestPt.Y+y]:=Source.Canvas.Pixels[x, y];
    end;
end;

type
  CrackControl = class(TControl);

function GetParentControlColor(AParent: TComponent): TColor;
begin
  Result:=clNone;
  if (AParent <> Nil) and (AParent is TControl) then
    Result:=CrackControl(AParent).Color;
end;

{ TImageButton }

constructor TImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle + [csReplicatable] - [csNoStdEvents] + [csClickEvents];
  Width:=57;
  Height:=25;
  FGlyph:=TBitmap.Create;
  FGlyph.Width:=Width;
  FGlyph.Height:=Height;
  FPicture:=TBitmap.Create;
  FPicture.OnChange:=PictureChanged;

  FTransparent:=False;
  FParentBkgndColor:=True;
  FTransparentColor:=FPicture.Canvas.Pixels[0, FPicture.Height-1];
  FDrawing:=False;
  FToolButton:=False;
  FInitialState:=isNormal;
  FOnEnterState:=esDoNothing;
//  FCenterState:=csNone;
  FPopupState:=psAfterLMouseUp;
  FShadowState:=ssNone;
  FShadowColor:=$00B1AFAC;
  FLightState:=ls30Percent;
  FMoveWhenPressed:=True;
  FMovePoint:=Point(0, 0);

  FEntered:=False;
  FDown:=False;
end;

destructor TImageButton.Destroy;
begin
  FPicture.FreeImage;
  FPicture.Free;
  FGlyph.FreeImage;
  FGlyph.Free;
  inherited;
end;

procedure TImageButton.WMMouseEnter(var Msg: TWMMouse);
begin
  if Enabled then
    begin
      FEntered:=True;
      if FMoveWhenPressed and FDown then FMovePoint:=Point(1, 1)
      else FMovePoint:=Point(0, 0);
      if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
      PictureChanged(Self);
    end;
end;

procedure TImageButton.WMMouseLeave(var Msg: TWMMouse);
begin
  if Enabled then
    begin
      FEntered:=False;
      if FMoveWhenPressed then FMovePoint:=Point(0, 0);
      if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
      PictureChanged(Self);
    end;
end;

procedure TImageButton.WMLButtonDown(var Msg: TWMMouse);
begin
  if Enabled then
    begin
      inherited;
      FDown:=True;
      if not FEntered then FEntered:=True;
      if FMoveWhenPressed then FMovePoint:=Point(1, 1)
      else FMovePoint:=Point(0, 0);
      if Assigned(FOnMouseDown) then FOnMouseDown(Self);
      PictureChanged(Self);
    end;
end;

procedure TImageButton.WMLButtonUp(var Msg: TWMMouse);
var
  pt: TPoint;
begin
  if Enabled then
    begin
      inherited;
      FDown:=False;
      if FMoveWhenPressed then FMovePoint:=Point(0, 0);
      if Assigned(FOnMouseUp) then FOnMouseUp(Self);
      PictureChanged(Self);
      if (PopupMenu <> Nil) and FToolButton and (PopupState = psAfterLMouseUp) then
        begin
          FEntered:=False;
          PictureChanged(Self);
          pt:=Self.ClientToScreen(CenterPoint(ClientRect));
          PopupMenu.Popup(pt.X, pt.Y);
        end;
    end;
end;

procedure TImageButton.WMRButtonDown(var Msg: TWMRButtonDown);
begin
  Msg.Result:=1;
end;

procedure TImageButton.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  Msg.Result:=1;
end;

function TImageButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result:=True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and (Picture.Height > 0) then
    begin
      if Align in [alNone, alLeft, alRight] then
        if FToolButton then NewWidth:=Picture.Width+10
        else NewWidth:=Picture.Width;
      if Align in [alNone, alTop, alBottom] then NewHeight:=Picture.Height;
    end;
end;

function TImageButton.DestRect: TRect;
var
  w, h{, cw, ch}: Integer;
begin
  w:=FGlyph.Width;
  h:=FGlyph.Height;
//  cw:=ClientWidth;
//  ch:=ClientHeight;
//  if ((w > cw) or (h > ch)) then
//    begin
//      if w > cw then cw:=w;
//      if h > ch then ch:=h;
//    end;

  with Result do
    begin
      Left  :=0;
      Top   :=0;
      Right :=w;
      Bottom:=h;
    end;

// csNone, csVertical, csHorizontal, csMiddle
//  case CenterState of
//    csMiddle    : OffsetRect(Result, (cw-w) div 2, (ch-h) div 2);
//    csHorizontal: OffsetRect(Result, (cw-w) div 2, 0);
//    csVertical  : OffsetRect(Result, 0, (ch-h) div 2);
//  end;
end;

function TImageButton.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result:=False;
  Tmp:=Picture;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and (Tmp.PaletteModified) then
    begin
      if (Tmp.Palette = 0) then
        Tmp.PaletteModified:=False
      else
        begin
          ParentForm:=GetParentForm(Self);
          if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
            begin
              if FDrawing then
                ParentForm.Perform(wm_QueryNewPalette, 0, 0)
              else
                PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
              Result:=True;
              Tmp.PaletteModified:=False;
            end;
        end;
    end;
end;

function TImageButton.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture = Nil then
    begin
      Bitmap:=TBitmap.Create;
      try
        Bitmap.Width:=Width;
        Bitmap.Height:=Height;
        Picture:=Bitmap;
      finally
        Bitmap.Free;
      end;
    end;
  if Picture is TBitmap then
    Result:=TBitmap(Picture).Canvas
  else
    raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

function TImageButton.GetPalette: HPALETTE;
begin
  Result:=0;
  if FPicture <> Nil then Result:=FPicture.Palette;
end;

procedure TImageButton.Paint;
const
  Amount: array[ls10Percent..ls50Percent] of Byte = (25, 51, 76, 102, 127);
var
  Save: Boolean;
  TBmp: TBitmap;
  x, y: Integer;
  DestPt: TPoint;
  ParentColor: TColor;
begin
  if csDesigning in ComponentState then
    with inherited Canvas do
      begin
        Lock;
        Pen.Style:=psDot;
        Brush.Style:=bsClear;
        Rectangle(0, 0, Width, Height);
        Unlock;
      end;
  TBmp:=TBitmap.Create;
  TBmp.Width:=Picture.Width;
  TBmp.Height:=Picture.Height;
  TBmp.Canvas.Brush.Color:=clFuchsia;
  TBmp.Canvas.FillRect(Rect(0, 0, TBmp.Width, TBmp.Height));
  Save:=FDrawing;
  FDrawing:=True;
  try
    FGlyph.Transparent:=False;
    FGlyph.FreeImage;
    TransparentDraw(TBmp, Picture);
    if FToolButton then
      begin
        TBmp.Width:=Picture.Width+10;
        TBmp.Canvas.Brush.Color:=clFuchsia;
        TBmp.Canvas.FillRect(Rect(Picture.Width, 0, TBmp.Width, TBmp.Height));
        if Enabled then
          begin
            TBmp.Canvas.Pen.Color:=$000F0F0F;
            TBmp.Canvas.Brush.Color:=$000F0F0F;
          end
        else
          begin
            TBmp.Canvas.Pen.Color:=clGray;
            TBmp.Canvas.Brush.Color:=clGray;
          end;
        DrawArrow(TBmp.Canvas, sdDown, Point(TBmp.Width-9, (TBmp.Height-4) div 2), 3);
      end;
    ConvertBitmapToTransparent(TBmp, clFuchsia);
    FGlyph.Width:=TBmp.Width;
    FGlyph.Height:=TBmp.Height;
    FGlyph.Canvas.Brush.Color:=clFuchsia;
    FGlyph.Canvas.FillRect(Rect(0, 0, FGlyph.Width, FGlyph.Height));
    if not FEntered and (FInitialState = isDarken) then Darkness(TBmp, Amount[FLightState]);
    if FEntered and (FOnEnterState <> esDoNothing) then
      begin
        if (FInitialState = isNormal) and (FOnEnterState = esLighten) then Lightness(TBmp, Amount[FLightState]);
        if FOnEnterState = esMakeShadow then
          begin
            if FShadowState = ssSkypeStyle then
              begin
                FGlyph.Canvas.Pen.Color:=TColor(ColorToRGB(ShadowColor));
                FGlyph.Canvas.Brush.Color:=TColor(ColorToRGB(ShadowColor));
                FGlyph.Canvas.RoundRect(0, 0, TBmp.Width, TBmp.Height, 8, 8);
              end
            else
              if FShadowState = ssGlyphShadow then
                begin
                  DestPt:=DestRect.TopLeft;
                  for y:=0 to TBmp.Height-1 do
                    for x:=0 to TBmp.Width-1 do
                      if TBmp.Canvas.Pixels[x, y] <> clFuchsia then
                        FGlyph.Canvas.Pixels[DestPt.X+x+1, DestPt.Y+y+1]:=TColor(ColorToRGB(ShadowColor));
                end;
          end;
      end;
    TransparentDraw(FGlyph.Canvas, TBmp, FMovePoint);
    if not Enabled then GrayScale(FGlyph);
    if not Transparent and ParentBkgndColor then
      begin
        ParentColor:=GetParentControlColor(Parent);
        ConvertBitmapToTransparent(FGlyph, ParentColor);
      end
    else
      ConvertBitmapToTransparent(FGlyph, FTransparentColor);
    FGlyph.TransparentColor:=FTransparentColor;
    FGlyph.Transparent:=FTransparent;
    with inherited Canvas do
      begin
        Lock;
        if FTransparent then
          begin
            SetBkColor(Handle, ColorToRGB(FTransparentColor));
            SetBkMode(Handle, Windows.TRANSPARENT);
          end
        else
          SetBkMode(Handle, OPAQUE);
        Draw(DestRect.Left, DestRect.Top, FGlyph);
        SetBkMode(Handle, OPAQUE);
        Unlock;
      end;
  finally
    FDrawing:=Save;
    TBmp.FreeImage;
    TBmp.Free;
  end;
end;

procedure TImageButton.PictureChanged(Sender: TObject);
var
  G: TGraphic;
begin
  if (Picture <> Nil) and (Picture.Width > 0) and (Picture.Height > 0) then
    DoSetBounds(Left, Top, Picture.Width, Picture.Height);
  G:=Picture;
  if G <> Nil then
    begin
      if not ((G is TMetaFile) or (G is TIcon)) then G.Transparent:=FTransparent;
      if not G.Transparent then
        ControlStyle:=ControlStyle + [csOpaque]
      else
        ControlStyle:=ControlStyle - [csOpaque];
      if DoPaletteChange and FDrawing then Update;
    end
  else ControlStyle:=ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;
end;

{
procedure TImageButton.SetCenterState(Value: TPictureCenterState);
begin
  if FCenterState <> Value then
    begin
      FCenterState:=Value;
      PictureChanged(Self);
    end;
end;
}

procedure TImageButton.SetPicture(Value: TBitmap);
begin
  FPicture.Assign(Value);
  FTransparentColor:=FPicture.Canvas.Pixels[0, FPicture.Height-1];
end;

procedure TImageButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
    begin
      FTransparent:=Value;
      FTransparentColor:=FPicture.Canvas.Pixels[0, FPicture.Height-1];
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetParentBkgndColor(Value: Boolean);
begin
  if (Value <> FParentBkgndColor) and not Transparent then
    begin
      FParentBkgndColor:=Value;
      FTransparentColor:=FPicture.Canvas.Pixels[0, FPicture.Height-1];
      PictureChanged(Self);
    end;
end;

procedure TImageButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FToolButton then AWidth:=AWidth+10;
  SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TImageButton.SetInitialState(Value: TPictureInitialState);
begin
  if Value <> FInitialState then
    begin
      FInitialState:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetOnEnterState(Value: TButtonOnEnterState);
begin
  if Value <> FOnEnterState then
    begin
      FOnEnterState:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetPopupState(Value: TButtonOnPopupState);
begin
  if Value <> FPopupState then
    begin
      FPopupState:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetShadowState(Value: TButtonShadowState);
begin
  if Value <> FShadowState then
    begin
      FShadowState:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetLightState(Value: TPictureLightState);
begin
  if Value <> FLightState then
    begin
      FLightState:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetMoveWhenPressed(Value: Boolean);
begin
  if Value <> FMoveWhenPressed then
    begin
      FMoveWhenPressed:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetToolButton(Value: Boolean);
begin
  if Value <> FToolButton then
    begin
      FToolButton:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
    begin
      FShadowColor:=Value;
      PictureChanged(Self);
    end;
end;

procedure TImageButton.CMEnabledChanged(var Message: TMessage);
begin
  PictureChanged(Self);
end;

end.

