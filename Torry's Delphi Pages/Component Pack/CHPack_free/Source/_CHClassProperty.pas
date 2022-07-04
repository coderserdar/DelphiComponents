unit _CHClassProperty;

{ ##############################################################################
  _CHClassProperty

  Autor     :   Christian Hämmerle
  eMail     :   chaemmerle@uni.de
  Internet  :   http://www.Blue-Xplosion.de (German/English)

  Lizenz    :   Freeware

  ############################################################################ }

interface

uses
  Classes, Graphics;

type
  TFocusStyle = (csDot, csSolid);
  TFocusMode = (cmAuto, cmCustom);
  TCompType = (ctLabel, ctButton, ctRadiobutton, ctCheckbox);
  TFillStyle = (fsNormal, fsBitmap, fsGradient, fsEffect);
  TCaptionEffect = (ceGradient, ceBitmap);
  TBoxStyle = (bxNormal, bxFlat);
  TCheckBoxFill = (cfSolid, cfCross, cfHook);
  TRadioButtonFill = (rfSolid, rfDot, rfCross, rfHLine);
  TBoxFillColor = (bcBlack, bcRed, bcGreen, bcBlue, bcYellow, bcLime, bcGray, bcNone);
  TGlyphAlignment = (gaLeft, gaTop, gaRight, gaBottom, gaCenter);
  TGlyphAlignMode = (gmCaption, gmControl, gmCustom);
  TTransMode = (tmAuto, tmCustom);
  TBitmapMode = (bmNormal, bmStretch, bmTile, bmCenter);
  TGradientStyle = (gsHorizontal_L, gsHorizontal_R, gsVertical_T, gsVertical_B,
    gsArrow_L, gsArrow_R, gsArrow_B, gsArrow_T, gsDiagonal_L, gsDiagonal_R, gsRotation);
  TRGBArray = array[0..2] of Byte;
  TGradientStep = -100..100;
  TAngle = 0..360;
  TDirection = (drNone, drUp, drDown, drLeft, drRight, drUpLeft,
    drUpRight, drDownLeft, drDownRight);
  TTextStyle = (tsNone, tsCustom, tsRaised, tsRaisedColor, tsRaisedShadow,
    tsRecessed, tsRecessedColor, tsRecessedShadow, tsShadow);
  TTextAlign = (tgCenter, tgAuto, tgCustom, tgCenterBottom, tgCenterTop, tgCenterLeft, tgCenterRight,
    tgTopLeft, tgTopRight, tgBottomLeft, tgBottomRight);
  TExecuteType = (etNone, etWWW, etEMail, etNews, etEXE);
  TBorderStyle = (bsFlat, bsNormal, bsExtended);
  TButtonMode = (bmButton, bmSpeedbutton);
  TGlyphEnabled = (geNone, geGray);

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Gradient }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHGradient = class(TPersistent)
  private
    FColor0, FColor1, FColor2, FColor3, FColor4, FColor5, FColor6 : TColor;
    FColor7, FColor8, FColor9 : TColor;

    FActive0, FActive1, FActive2, FActive3, FActive4, FActive5, FActive6: Boolean;
    FActive7, FActive8, FActive9 : Boolean;

    FColorArray : array[0..9] of TColor;
    FActiveArray : array[0..9] of Boolean;

    FGradientStyle : TGradientStyle;
    FRotation : TGradientStep;
    FCustomDiagonal : Boolean;

    FOnChange: TNotifyEvent;

  protected
    procedure SetColor(Index: Integer; const Value: TColor);
    procedure SetActive(Index: Integer; Value : Boolean);
    procedure SetGradientStyle(const Value: TGradientStyle);
    procedure SetRotation(const Value: TGradientStep);
    procedure InitColorArray;
    procedure InitActiveArray;
    procedure SetGradientColorArray;
    procedure DoChange(Sender: TObject); dynamic;
  public
    FGradientColorArray : array of TColor;

    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active0 : Boolean Index 0 read FActive0 Write SetActive;
    property Active1 : Boolean Index 1 read FActive1 Write SetActive;
    property Active2 : Boolean Index 2 read FActive2 Write SetActive;
    property Active3 : Boolean Index 3 read FActive3 Write SetActive;
    property Active4 : Boolean Index 4 read FActive4 Write SetActive;
    property Active5 : Boolean Index 5 read FActive5 Write SetActive;
    property Active6 : Boolean Index 6 read FActive6 Write SetActive;
    property Active7 : Boolean Index 7 read FActive7 Write SetActive;
    property Active8 : Boolean Index 8 read FActive8 Write SetActive;
    property Active9 : Boolean Index 9 read FActive9 Write SetActive;

    property Color0 : TColor Index 0 read FColor0 write SetColor;
    property Color1 : TColor Index 1 read FColor1 write SetColor;
    property Color2 : TColor Index 2 read FColor2 write SetColor;
    property Color3 : TColor Index 3 read FColor3 write SetColor;
    property Color4 : TColor Index 4 read FColor4 write SetColor;
    property Color5 : TColor Index 5 read FColor5 write SetColor;
    property Color6 : TColor Index 6 read FColor6 write SetColor;
    property Color7 : TColor Index 7 read FColor7 write SetColor;
    property Color8 : TColor Index 8 read FColor8 write SetColor;
    property Color9 : TColor Index 9 read FColor9 write SetColor;

    property Style : TGradientStyle read FGradientStyle write SetGradientStyle;
    property Rotation: TGradientStep read FRotation write SetRotation;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ CaptionLayout }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHCaptionLayout = class(TPersistent)
  private
    FAngle : TAngle;
    FCosAngle : Double;
    FSinAngle : Double;
    FTextStyle : TTextStyle;
    FTextAlign : TTextAlign;

    FChangingShadow : Boolean;
    FShadowColor : TColor;
    FShadowDepth : Integer;
    FShadowDirection : TDirection;

    FChangingHighlight : Boolean;
    FHighlightColor : TColor;
    FHighlightDepth : Integer;
    FHighlightDirection : TDirection;

    FOnChange: TNotifyEvent;
    FAntialiasing: Boolean;
    FPosY: Integer;
    FPosX: Integer;
    FDisableColor: TColor;
    procedure SetAntialiasing(const Value: Boolean);
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);
    procedure SetDisableColor(const Value: TColor);
  protected
    procedure SetAngle(const Value: TAngle);
    procedure SetShadowDirection(const Value : TDirection);
    procedure SetShadowDepth(const Value : Integer);
    procedure SetShadowColor(const Value : TColor);
    procedure SetHighlightDirection(const Value : TDirection);
    procedure SetHighlightDepth(const Value : Integer);
    procedure SetHighlightColor(const Value : TColor);
    procedure SetTextStyle(const Value : TTextStyle);
    procedure SetTextAlign(const Value : TTextAlign);

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    destructor Destroy; override; 
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alignment : TTextAlign read FTextAlign write SetTextAlign;
    property Angle : TAngle read FAngle write SetAngle;
    property Antialiasing : Boolean read FAntialiasing write SetAntialiasing;
    property DisableColor : TColor read FDisableColor write SetDisableColor;
    property HighlightDirection : TDirection read FHighlightDirection write SetHighlightDirection;
    property HighlightDepth : Integer read FHighlightDepth write SetHighlightDepth;
    property HighlightColor : TColor read FHighlightColor write SetHighlightColor;
    property PosX : Integer read FPosX Write SetPosX;
    property PosY : Integer read FPosY Write SetPosY;
    property ShadowDirection : TDirection read FShadowDirection write SetShadowDirection;
    property ShadowDepth : Integer read FShadowDepth write SetShadowDepth;
    property ShadowColor : TColor read FShadowColor write SetShadowColor;
    property TextStyle : TTextStyle read FTextStyle write SetTextStyle;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ CaptionEffect }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHCaptionEffect = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FCaptionBmp: TBitmap;
    FCaptionEffect: TCaptionEffect;
    procedure SetCaptionBmp(const Value: TBitmap);
    procedure SetCaptionEffect(const Value: TCaptionEffect);
  protected
    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Effect : TCaptionEffect read FCaptionEffect Write SetCaptionEffect;
    property FontBitmap : TBitmap read FCaptionBmp Write SetCaptionBmp;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Border }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHBorder = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor : TColor;
    FBorderHighlight : TColor;
    FBorderShadow : TColor;
    FBorderWidth : Word;
    FBorderStyle : TBorderStyle;
    FBorderSingle : Word;
    FAutoColor: Boolean;
    FAutoPercent: Integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderHighlight(const Value: TColor);
    procedure SetBorderShadow(const Value: TColor);
    procedure SetBorderWidth(const Value: Word);
    procedure SetBorderSingle(const Value: Word);
    procedure SetBorderStyle(const Value : TBorderStyle);
    procedure SetAutoColor(const Value: Boolean);
    procedure SetAutoPercent(const Value: Integer);
  protected
    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AutoColor : Boolean read FAutoColor Write SetAutoColor;
    property AutoPercent : Integer read FAutoPercent Write SetAutoPercent;
    property Color : TColor read FBorderColor write SetBorderColor;
    property HighlightColor : TColor read FBorderHighlight write SetBorderHighlight;
    property ShadowColor : TColor read FBorderShadow write SetBorderShadow;
    property Width : Word read FBorderWidth write SetBorderWidth;
    property SingleWidth : Word read FBorderSingle write SetBorderSingle;
    property Style : TBorderStyle read FBorderStyle write SetBorderStyle;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ KeyPress }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHKeyPress = class(TPersistent)
  private
    FvkEnter, FvkESC, FvkStrg, FvkAlt, FvkShift : Boolean;

    FOnChange: TNotifyEvent;
  protected
    procedure SetKeyPress(Index: Integer; Value : Boolean);

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alt : Boolean Index 0 read FvkAlt Write SetKeyPress;
    property Enter : Boolean Index 1 read FvkEnter Write SetKeyPress;
    property ESC : Boolean Index 2 read FvkESC Write SetKeyPress;
    property Strg : Boolean Index 3 read FvkStrg Write SetKeyPress;
    property Shift : Boolean Index 4 read FvkShift Write SetKeyPress;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Box (Checkbox) }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHBoxCB = class(TPersistent)
  private
    FBoxStyle : TBoxStyle;
    FBoxColor : TBoxFillColor;
    FBoxFill : TCheckBoxFill;
    FBoxFocusColor : TColor;

    FOnChange: TNotifyEvent;
    procedure SetBoxColor(const Value: TBoxFillColor);
    procedure SetBoxFill(const Value: TCheckBoxFill);
    //procedure SetBoxFocuscolor(const Value: TColor);
    procedure SetBoxStyle(const Value: TBoxStyle);
  protected

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Style : TBoxStyle read FBoxStyle Write SetBoxStyle;
    property Color : TBoxFillColor read FBoxColor Write SetBoxColor;
    property Fill : TCheckBoxFill  read FBoxFill Write SetBoxFill;
    //property Focuscolor : TColor read FBoxFocusColor Write SetBoxFocuscolor;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Box (Radiobutton) }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHBoxRB = class(TPersistent)
  private
    FBoxStyle : TBoxStyle;
    FBoxColor : TBoxFillColor;
    FBoxFill : TRadioButtonFill;
    FBoxFocusColor : TColor;

    FOnChange: TNotifyEvent;
    procedure SetBoxColor(const Value: TBoxFillColor);
    procedure SetBoxFill(const Value: TRadioButtonFill);
    //procedure SetBoxFocuscolor(const Value: TColor);
    procedure SetBoxStyle(const Value: TBoxStyle);
  protected

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Style : TBoxStyle read FBoxStyle Write SetBoxStyle;
    property Color : TBoxFillColor read FBoxColor Write SetBoxColor;
    property Fill : TRadioButtonFill  read FBoxFill Write SetBoxFill;
    //property Focuscolor : TColor read FBoxFocusColor Write SetBoxFocuscolor;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Focus-Control }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHFocus = class(TPersistent)
  private
    FFocusShow : Boolean;
    FFocusActive : Boolean;
    FFocusStyle : TFocusStyle;
    FFocusColor : TColor;
    FFocusMode : TFocusMode;

    FOnChange: TNotifyEvent;
    FFocusWidth: Integer;
    FFocusPosTop: Integer;
    FFocusPosLeft: Integer;
    FFocusPosRight: Integer;
    FFocusPosBottom: Integer;
    FFocusStep: Integer;
    procedure SetFocusColor(const Value: TColor);
    procedure SetFocusShow(const Value: Boolean);
    procedure SetFocusStyle(const Value: TFocusStyle);
    procedure SetFocusWidth(const Value: Integer);
    procedure SetFocusPosBottom(const Value: Integer);
    procedure SetFocusPosLeft(const Value: Integer);
    procedure SetFocusPosRight(const Value: Integer);
    procedure SetFocusPosTop(const Value: Integer);
    procedure SetFocusActive(const Value: Boolean);
    procedure SetFocusMode(const Value: TFocusMode);
    procedure SetFocusStep(const Value: Integer);
  protected

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active : Boolean read FFocusActive Write SetFocusActive;
    property Color : TColor read FFocusColor Write SetFocusColor;
    property Mode : TFocusMode read FFocusMode Write SetFocusMode;
    property Show : Boolean read FFocusShow Write SetFocusShow;
    property Step : Integer read FFocusStep Write SetFocusStep;
    property Style : TFocusStyle read FFocusStyle Write SetFocusStyle;
    property PosTop : Integer read FFocusPosTop Write SetFocusPosTop;
    property PosBottom : Integer read FFocusPosBottom Write SetFocusPosBottom;
    property PosLeft : Integer read FFocusPosLeft Write SetFocusPosLeft;
    property PosRight : Integer read FFocusPosRight Write SetFocusPosRight;
    property Width : Integer read FFocusWidth Write SetFocusWidth;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{Margin }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHMargin = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FMarginBottom: Word;
    FMarginTop: Word;
    FMarginLeft: Word;
    FMarginRight: Word;
    FMarginColor: TColor;
    FMarginLineColor: TColor;
    FMarginLineWidth: Word;
    procedure SetMarginBottom(const Value: Word);
    procedure SetMarginLeft(const Value: Word);
    procedure SetMarginRight(const Value: Word);
    procedure SetMarginTop(const Value: Word);
    procedure SetMarginColor(const Value: TColor);
    procedure SetMarginLineColor(const Value: TColor);
    procedure SetMarginLineWidth(const Value: Word);
  protected
    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MBottom : Word read FMarginBottom Write SetMarginBottom;
    property MLeft : Word read FMarginLeft Write SetMarginLeft;
    property MRight : Word read FMarginRight Write SetMarginRight;
    property MTop : Word read FMarginTop Write SetMarginTop;
    property Color : TColor read FMarginColor Write SetMarginColor;
    property LineColor : TColor read FMarginLineColor Write SetMarginLineColor;
    property LineWidth : Word read FMarginLineWidth Write SetMarginLineWidth;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Glyph }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHGlyph = class(TPersistent)
  private
    FPosX : Integer;
    FPosY : Integer;
    FGlyphSpace: Integer;
    FTransColor: TColor;
    FGlyphBmp: TBitmap;
    FTransMode: TTransMode;
    FGlyphAlignment: TGlyphAlignment;
    FGlyphAlignMode : TGlyphAlignMode;
    FOnChange: TNotifyEvent;
    FEnableMode: TGlyphEnabled;

  protected
    procedure SetGlyphAlignment(const Value: TGlyphAlignment);
    procedure SetGlyphAlignMode(const Value: TGlyphAlignMode);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphSpace(const Value : Integer);
    procedure SetTransColor(const Value : TColor);
    procedure SetTransMode(const Value : TTransMode);
    procedure SetPosX(const Value : Integer);
    procedure SetPosY(const Value : Integer);
    procedure SetEnabledMode(const Value: TGlyphEnabled);

    procedure DoChange(Sender: TObject); dynamic;
  public
    FRestoreBmp: TBitmap;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alignment : TGlyphAlignment read FGlyphAlignment write SetGlyphAlignment;
    property AlignMode : TGlyphAlignMode read FGlyphAlignMode Write SetGlyphAlignMode;
    property Glyph: TBitmap read FGlyphBmp write SetGlyph;
    property PosX : Integer read FPosX Write SetPosX;
    property PosY : Integer read FPosY Write SetPosY;
    property Space: Integer read FGlyphSpace write SetGlyphSpace;
    property TransparentColor : TColor read FTransColor Write SetTransColor;
    property TransparentMode : TTransMode read FTransMode Write SetTransMode;
    property EnabledMode : TGlyphEnabled read FEnableMode write SetEnabledMode;
  end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Bitmap }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHBitmap = class(TPersistent)
  private
    FForegroundBmp: TBitmap;
    FBmpMode: TBitmapMode;

    FOnChange: TNotifyEvent;
  protected
    procedure SetForegroundBmp(const Value : TBitmap);
    procedure SetBmpMode(const Value : TBitmapMode);

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Bitmap : TBitmap read FForegroundBmp Write SetForegroundBmp;
    property Mode : TBitmapMode read FBmpMode Write SetBmpMode;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Fill }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHFill = class(TPersistent)
  private
     FFillStyle : TFillStyle;
     FTransparent : Boolean;

     FOnChange: TNotifyEvent;
  protected
    procedure SetFillStyle(const Value : TFillStyle);
    procedure SetTransparent(const Value : Boolean);

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Style : TFillStyle read FFillStyle Write SetFillStyle;
    property Transparent : Boolean read FTransparent Write SetTransparent;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Fill B}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
type
  TCHFillB = class(TPersistent)
  private
     FTransparent : Boolean;

     FOnChange: TNotifyEvent;
  protected
    procedure SetTransparent(const Value : Boolean);

    procedure DoChange(Sender: TObject); dynamic;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Transparent : Boolean read FTransparent Write SetTransparent;
  end;


implementation


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ C a p t i o n L a y o u t }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCaptionLayout.Create;
begin
  inherited Create;

  FTextAlign := tgCenter;
  FAngle := 0;
  FHighlightDirection := drNone;
  FHighlightDepth := 0;
  FHighlightColor := clBtnHighlight;
  FShadowDirection := drNone;
  FShadowDepth := 0;
  FShadowColor := clBtnShadow;
  FTextStyle := tsNone;
  FAntialiasing := True;
  FDisableColor := clBtnShadow;

  FChangingShadow := False;
  FChangingHighlight := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCaptionLayout.Destroy;
begin
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetAngle(const Value: TAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    FCosAngle := Cos(Value * Pi / 180);
    FSinAngle := Sin(Value * Pi / 180);
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetHighlightColor(const Value: TColor);
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetHighlightDepth(const Value: Integer);
begin
  if FHighlightDepth <> Value then
  begin
    FHighlightDepth := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetHighlightDirection(const Value: TDirection);
begin
  if FHighlightDirection <> Value then
  begin
    FHighlightDirection := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetShadowDepth(const Value: Integer);
begin
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetShadowDirection(const Value: TDirection);
begin
  if FShadowDirection <> Value then
  begin
    FShadowDirection := Value;
    FTextStyle := tsCustom;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetTextStyle(const Value: TTextStyle);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetAntialiasing(const Value: Boolean);
begin
  if FAntialiasing <> Value then
  begin
    FAntialiasing := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetPosX(const Value: Integer);
begin
  if FPosX <> Value then
  begin
    FPosX := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetPosY(const Value: Integer);
begin
  if FPosY <> Value then
  begin
    FPosY := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionLayout.SetDisableColor(const Value: TColor);
begin
  if FDisableColor <> Value then
  begin
    FDisableColor := Value;
    DoChange(self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHCaptionEffect }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionEffect.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCaptionEffect.Create;
begin
  inherited Create;

  FCaptionBmp := TBitmap.Create;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCaptionEffect.Destroy;
begin
  FCaptionBmp.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionEffect.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionEffect.SetCaptionBmp(const Value: TBitmap);
begin
  FCaptionBmp.Assign(Value);
  DoChange(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCaptionEffect.SetCaptionEffect(const Value: TCaptionEffect);
begin
  if FCaptionEffect <> Value then
  begin
    FCaptionEffect := Value;
    DoChange(self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ B o r d e r }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

procedure TCHBorder.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHBorder.Create;
begin
  inherited Create;

  FBorderColor := clBlack;
  FBorderHighlight := clWhite;
  FBorderShadow := clGray;
  FBorderWidth := 2;
  FBorderSingle := 1;
  FBorderStyle := bsNormal;
  FAutoPercent := 25;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetAutoColor(const Value: Boolean);
begin
  if FAutoColor <> Value then
  begin
    FAutoColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetAutoPercent(const Value: Integer);
begin
  if FAutoPercent <> Value then
  begin
    FAutoPercent := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderHighlight(const Value: TColor);
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderShadow(const Value: TColor);
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderSingle(const Value: Word);
begin
  if FBorderSingle <> Value then
  begin
    FBorderSingle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBorder.SetBorderWidth(const Value: Word);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ K e y P r e s s }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyPress.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHKeyPress.Create;
begin
  inherited Create;

  FvkEnter := True;
  FvkESC := True;
  FvkStrg := True;
  FvkAlt := True;
  FvkShift := True;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyPress.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHKeyPress.SetKeyPress(Index: Integer; Value: Boolean);
begin
  case Index of
    0: FvkAlt := Value;
    1: FvkEnter := Value;
    2: FvkESC := Value;
    3: FvkStrg := Value;
    4: FvkShift := Value;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ C h e c k B o x }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxCB.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHBoxCB.Create;
begin
  inherited Create;

  FBoxColor := bcBlack;
  FBoxFocusColor := clInfoBk;
  FBoxStyle := bxNormal;
  FBoxFill := cfHook;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxCB.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxCB.SetBoxColor(const Value: TBoxFillColor);
begin
  if FBoxColor <> Value then
  begin
    FBoxColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxCB.SetBoxFill(const Value: TCheckBoxFill);
begin
  if FBoxFill <> Value then
  begin
    FBoxFill := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
//procedure TCHBoxCB.SetBoxFocuscolor(const Value: TColor);
//begin
//  if FBoxFocusColor <> Value then
//  begin
//    FBoxFocusColor := Value;
//    DoChange(Self);
//  end;
//end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxCB.SetBoxStyle(const Value: TBoxStyle);
begin
  if FBoxStyle <> Value then
  begin
    FBoxStyle := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ R a d i o b u t t o n }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

procedure TCHBoxRB.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHBoxRB.Create;
begin
  inherited Create;

  FBoxColor := bcBlack;
  FBoxFocusColor := clInfoBk;
  FBoxStyle := bxNormal;
  FBoxFill := rfDot;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxRB.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxRB.SetBoxColor(const Value: TBoxFillColor);
begin
  if FBoxColor <> Value then
  begin
    FBoxColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxRB.SetBoxFill(const Value: TRadioButtonFill);
begin
  if FBoxFill <> Value then
  begin
    FBoxFill := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
//procedure TCHBoxRB.SetBoxFocuscolor(const Value: TColor);
//begin
//  if FBoxFocusColor <> Value then
//  begin
//    FBoxFocusColor := Value;
//    DoChange(Self);
//  end;
//end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBoxRB.SetBoxStyle(const Value: TBoxStyle);
begin
  if FBoxStyle <> Value then
  begin
    FBoxStyle := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ F o c u s }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

procedure TCHFocus.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFocus.Create;
begin
  inherited Create;

  FFocusActive := True;
  FFocusShow := False;
  FFocusColor := clBlack;
  FFocusStyle := csDot;
  FFocusWidth := 1;
  FFocusMode := cmAuto;
  FFocusStep := 2;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusActive(const Value: Boolean);
begin
  if FFocusActive <> Value then
  begin
    FFocusActive := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusMode(const Value: TFocusMode);
begin
  if FFocusMode <> Value then
  begin
    FFocusMode := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusPosBottom(const Value: Integer);
begin
  FFocusPosBottom := Value;
  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusPosLeft(const Value: Integer);
begin
  FFocusPosLeft := Value;
  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusPosRight(const Value: Integer);
begin
  FFocusPosRight := Value;
  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusPosTop(const Value: Integer);
begin
  FFocusPosTop := Value;
  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusShow(const Value: Boolean);
begin
  if FFocusShow <> Value then
  begin
    FFocusShow := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusStep(const Value: Integer);
begin
  if FFocusStep <> Value then
  begin
    FFocusStep := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusStyle(const Value: TFocusStyle);
begin
  if FFocusStyle <> Value then
  begin
    FFocusStyle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFocus.SetFocusWidth(const Value: Integer);
begin
  if FFocusWidth <> Value then
  begin
    FFocusWidth := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ M a r g i n }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }

procedure TCHMargin.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHMargin.Create;
begin
  inherited Create;

  FMarginBottom := 0;
  FMarginTop := 0;
  FMarginLeft := 0;
  FMarginRight := 0;
  FMarginColor := clInfoBk;
  FMarginLineColor := clBlack;
  FMarginLineWidth := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginBottom(const Value: Word);
begin
  if FMarginBottom <> Value then
  begin
    FMarginBottom := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginColor(const Value: TColor);
begin
  if FMarginColor <> Value then
  begin
    FMarginColor := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginLeft(const Value: Word);
begin
  if FMarginLeft <> Value then
  begin
    FMarginLeft := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginLineColor(const Value: TColor);
begin
  if FMarginLineColor <> Value then
  begin
    FMarginLineColor := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginLineWidth(const Value: Word);
begin
  if FMarginLineWidth <> Value then
  begin
    FMarginLineWidth := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginRight(const Value: Word);
begin
  if FMarginRight <> Value then
  begin
    FMarginRight := Value;
    DoChange(self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHMargin.SetMarginTop(const Value: Word);
begin
  if FMarginTop <> Value then
  begin
    FMarginTop := Value;
    DoChange(self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ G r a d i e n t }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHGradient.Create;
begin
  inherited Create;

  InitActiveArray;
  InitColorArray;

  FColor0 := clGreen;
  FColor1 := clRed;
  FColor2 := clBlue;
  FColor3 := clYellow;
  FColor4 := clOlive;
  FColor5 := clWhite;
  FColor6 := clBlack;
  FColor7 := clGray;
  FColor8 := clLime;
  FColor9 := clPurple;
  FColorArray[0] := FColor0;
  FColorArray[1] := FColor1;
  FColorArray[2] := FColor2;
  FColorArray[3] := FColor3;
  FColorArray[4] := FColor4;
  FColorArray[5] := FColor5;
  FColorArray[6] := FColor6;
  FColorArray[7] := FColor7;
  FColorArray[8] := FColor8;
  FColorArray[9] := FColor9;

  FActive0 := True;
  FActive1 := True;
  FActive2 := True;
  FActiveArray[0] := FActive0;
  FActiveArray[1] := FActive1;
  FActiveArray[2] := FActive2;

  SetGradientColorArray;

  FGradientStyle := gsHorizontal_L;
  FRotation := 100;
  FCustomDiagonal := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Farbwerte setzen }
procedure TCHGradient.SetColor(Index: Integer; const Value: TColor);
begin
  case Index of
    0: FColor0 := Value;
    1: FColor1 := Value;
    2: FColor2 := Value;
    3: FColor3 := Value;
    4: FColor4 := Value;
    5: FColor5 := Value;
    6: FColor6 := Value;
    7: FColor7 := Value;
    8: FColor8 := Value;
    9: FColor9 := Value;
  end;

  FColorArray[Index] := Value;
  SetGradientColorArray;

  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Farben aktivieren/deaktivieren }
procedure TCHGradient.SetActive(Index: Integer; Value: Boolean);
begin
  case Index of
    0: FActive0 := Value;
    1: FActive1 := Value;
    2: FActive2 := Value;
    3: FActive3 := Value;
    4: FActive4 := Value;
    5: FActive5 := Value;
    6: FActive6 := Value;
    7: FActive7 := Value;
    8: FActive8 := Value;
    9: FActive9 := Value;
  end;

  FActiveArray[Index] := Value;
  SetGradientColorArray;

  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.InitActiveArray;
var
  I : Integer;
begin
  for I := 0 to 9 do
  begin
    FActiveArray[I] := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.InitColorArray;
var
  I : Integer;
begin
  for I := 0 to 9 do
  begin
    FColorArray[I] := -1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.SetGradientColorArray;
var
  I, Next : Integer;
begin
  FGradientColorArray := nil;
  Next := 1;
  for I := 0 to 9 do
  begin
    if FActiveArray[I] = True then
    begin
      SetLength(FGradientColorArray, Next);
      FGradientColorArray[Next -1] := FColorArray[I];
      Inc(Next);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.SetGradientStyle(const Value: TGradientStyle);
begin
  if FGradientStyle <> Value then
  begin
    FGradientStyle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGradient.SetRotation(const Value: TGradientStep);
begin
  if FRotation <> Value then
  begin
    FRotation := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ G l y p h }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHGlyph.Create;
begin
  inherited Create;

  FGlyphBmp := TBitmap.Create;
  FGlyphBmp.Width := 0;
  FGlyphBmp.Height := 0;
  FRestoreBmp := TBitmap.Create;
  FRestoreBmp.Width := 0;
  FRestoreBmp.Height := 0;
  FPosX := 0;
  FPosY := 0;
  FGlyphSpace := 5;
  FTransColor := clWhite;
  FTransMode := tmAuto;
  FGlyphAlignment := gaLeft;
  FGlyphAlignMode := gmCaption;
  FEnableMode := geGray;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHGlyph.Destroy;
begin
  FGlyphBmp.Free;
  FRestoreBmp.Free;
  inherited Destroy;
end;

procedure TCHGlyph.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetEnabledMode(const Value: TGlyphEnabled);
begin
  if FEnableMode <> Value then
  begin
    FEnableMode := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetGlyph(const Value: TBitmap);
begin
  FGlyphBmp.Assign(Value);
  FRestoreBmp.Assign(Value);
  DoChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetGlyphAlignment(const Value: TGlyphAlignment);
begin
  if FGlyphAlignment <> Value then
  begin
    FGlyphAlignment := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetGlyphAlignMode(const Value: TGlyphAlignMode);
begin
  if FGlyphAlignMode <> Value then
  begin
    FGlyphAlignMode := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetGlyphSpace(const Value: Integer);
begin
  if FGlyphSpace <> Value then
  begin
    FGlyphSpace:=Value;
    if FGlyphSpace < 0 then
      FGlyphSpace := 0;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetPosX(const Value: Integer);
begin
  if FPosX <> Value then
  begin
    FPosX := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetPosY(const Value: Integer);
begin
  if FPosY <> Value then
  begin
    FPosY := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetTransColor(const Value: TColor);
begin
  if FTransColor <> Value then
  begin
    FTransColor := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHGlyph.SetTransMode(const Value: TTransMode);
begin
  if FTransMode <> Value then
  begin
    FTransMode := Value;
    DoChange(Self);
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ B i t m a p }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBitmap.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHBitmap.Create;
begin
  inherited Create;

  FForegroundBmp := TBitmap.Create;
  FForegroundBmp.Width := 0;
  FForegroundBmp.Height := 0;
  FBmpMode := bmNormal;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHBitmap.Destroy;
begin
  FForegroundBmp.Free;
  inherited Destroy;
end;

procedure TCHBitmap.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBitmap.SetBmpMode(const Value: TBitmapMode);
begin
  if FBmpMode <> Value then
  begin
    FBmpMode := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBitmap.SetForegroundBmp(const Value: TBitmap);
begin
  FForegroundBmp.Assign(Value);
  DoChange(Self);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ F i l l }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFill.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFill.Create;
begin
  inherited Create;

  FFillStyle := fsNormal;
  FTransparent := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFill.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFill.SetFillStyle(const Value: TFillStyle);
begin
  if FFillStyle <> Value then
  begin
    FFillStyle := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFill.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    DoChange(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ F i l l  B}
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFillB.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFillB.Create;
begin
  inherited Create;

  FTransparent := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFillB.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFillB.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    DoChange(Self);
  end;
end;


end.
