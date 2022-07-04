unit RackCtls;

{ RackControls:
  TLEDButton, TButtonPanel, TScrewPanel, TLEDDisplay, TLEDMeter

  (C)opyright 2004 Version 1.20
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  RackControls ist eine Komponentensammlung zur Erstellung von
  Audiorack-ähnlichen Oberflächen. Diese Komponenten sind
  Public Domain, das Urheberrecht liegt aber beim Autor.

  Die Komponente TLEDDisplay ist eine Weiterentwicklung
  der Komponente TLCDDisplay von Luis Iglesias:
  luis.iglesias@vigo.org

  Änderungen, die bei LEDDisplay nachfolgende Nullen bei LeadingZeros=False doch zeichnet
  Ergänzt von Wolfgang Kleinrath

  Eigenschaft FSingleLED ergänzt von U. Conrad }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, {$ENDIF} Classes,
  Graphics, Controls, ExtCtrls, SysUtils, Messages, Forms;

type
  TBorderStyle     = (bsNone, bsSingle);
  TButtonDirection = (bdBottomUp, bdLeftUp, bdNone, bdRightUp, bdTopUp);
  TContrast        = 0..9;
  TDecSeparator    = (dsApostrophe, dsComma, dsDoublePoint, dsHyphen, dsNone, dsPoint, dsSemicolon);
  TLEDStates       = (lsOff, lsOn);
  TMeterDirection  = (mdDown, mdLeft, mdRight, mdUp);
  TNumGlyphs       = 0..4;
  TScrewSize       = 1..8;
  TSegmentStyle    = (ssRectangular, ssBeveled);
  TTextPosition    = (tpAbove, tpBelow, tpNone, tpOnButton);

  TLEDMeter = class;

  TLEDMeterColors = class(TPersistent)
  private
    FBorder,
    FSection1,
    FSection2,
    FSection3  : TColor;
    FOwner     : TLEDMeter;

    procedure SetBorder(NewValue: TColor);
    procedure SetSection1(NewValue: TColor);
    procedure SetSection2(NewValue: TColor);
    procedure SetSection3(NewValue: TColor);

  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source : TPersistent); override;

  published
    property Border: TColor read FBorder write SetBorder;
    property Section1: TColor read FSection1 write SetSection1;
    property Section2: TColor read FSection2 write SetSection2;
    property Section3: TColor read FSection3 write SetSection3;
  end;

  TLEDButton = class(TGraphicControl)
  private
    FBeveled:          boolean;
    FBorderStyle:      TBorderStyle;
    FButtonDirection:  TButtonDirection;
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorLED:         TColor;
    FColorLEDOff:      TColor;
    FColorShadow:      TColor;
    FDepth:            integer;
    FDown:             boolean;
    FFont:             TFont;
    FGlyph:            TBitmap;
    FLEDContrast:      TContrast;
    FNumGlyphs:        TNumGlyphs;
    FShowLED:          boolean;
    FStateOn:          boolean;
    FSwitching:        boolean;
    FTextPosition:     TTextPosition;

    FMouseDown:        boolean;
    FOnClick:          TNotifyEvent;

  protected
    procedure Paint;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
       override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
       override;

    procedure SetBeveled(newValue: boolean);
    procedure SetBorderStyle(newBorderStyle: TBorderStyle);
    procedure SetButtonDirection(NewDirection: TButtonDirection);
    procedure SetColor(newColor: TColor);
    procedure SetColorLED(newColor: TColor);
    procedure SetDepth(newValue: integer);
    procedure SetFont(newFont: TFont);
    procedure SetGlyph(newGlyph: TBitmap);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetNumGlyphs(newNumGlyphs: TNumGlyphs);
    procedure SetShowLED(newValue: boolean);
    procedure SetStateOn(newValue: boolean);
    procedure SetTextPosition(newValue: TTextPosition);

    procedure DrawBorder(Dest:TRect);
    procedure DrawCaption(Dest:TRect);
    procedure DrawGlyph(Dest:TRect);
    procedure DrawLED(var Dest:TRect);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    property Anchors;
    {$ENDIF}
    property Beveled: boolean read FBeveled write SetBeveled;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ButtonDirection: TButtonDirection read FButtonDirection write SetButtonDirection;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property ColorLED: TColor read FColorLED write SetColorLED;
    property Depth: integer read FDepth write SetDepth;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    {$IFDEF SR_Delphi3_Up}
    property PopupMenu;
    {$ENDIF}
    property ShowHint;
    property ShowLED: boolean read FShowLED write SetShowLED;
    property StateOn: boolean read FStateOn write SetStateOn;
    property Switching: boolean read FSwitching write FSwitching;
    property TextPosition: TTextPosition read FTextPosition write SetTextPosition;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TButtonPanel = class(TCustomPanel)
  private
    FBeveled:          boolean;
    FBorderStyle:      TBorderStyle;
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorShadow:      TColor;
    FDepth:            integer;
    FPanelDirection:   TButtonDirection;
    FShowLED:          boolean;

  protected
    procedure Paint;  override;

    procedure SetBeveled(newValue: boolean);
    procedure SetBorderStyle(newBorderStyle: TBorderStyle);
    procedure SetColor(newColor: TColor);
    procedure SetDepth(newValue: integer);
    procedure SetPanelDirection(NewDirection: TButtonDirection);
    procedure SetShowLED(newValue: boolean);

    procedure DrawBorder(Dest:TRect);
    procedure DrawCaption(Dest:TRect);
    procedure DrawLED(var Dest:TRect);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    property Alignment;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property Beveled: boolean read FBeveled write SetBeveled;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property Ctl3D;
    property Depth: integer read FDepth write SetDepth;
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi3_Up}
    property FullRepaint;
    {$ENDIF}
    property Font;
    property Locked;
    property PanelDirection: TButtonDirection read FPanelDirection write SetPanelDirection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowLED: boolean read FShowLED write SetShowLED;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF SR_Delphi3_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

  TScrewPanel = class(TCustomPanel)
  private
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorShadow:      TColor;
    FMargin:           integer;
    FScrewSize:        TScrewSize;
    FShowScrews:       boolean;

  protected
    procedure Paint;  override;

    procedure SetColor(newColor: TColor);
    procedure SetMargin(newValue: integer);
    procedure SetScrewSize(newValue: TScrewSize);
    procedure SetShowScrews(newValue: boolean);

    procedure DrawScrew(X,Y:integer);
    procedure DrawBevel(ARect:TRect;Raised:boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    property Alignment;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi3_Up}
    property FullRepaint;
    {$ENDIF}
    property Font;
    property Locked;
    property Margin: integer read FMargin write SetMargin;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrewSize: TScrewSize read FScrewSize write SetScrewSize;
    property ShowHint;
    property ShowScrews: boolean read FShowScrews write SetShowScrews;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF SR_Delphi3_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

  TLEDDisplay = class(TGraphicControl)
  private
    FBevelStyle      : TPanelBevel;
    FBorderStyle     : TBorderStyle;
    FColorBackGround,
    FColorLED        : TColor;
    FDecSeparator    : TDecSeparator;
    FDigit           : array [0..9] of TBitmap;
    FDigitHeight     : integer;
    FDigitShapeColor : TColor;
    FDigitWidth      : integer;
    FDrawDigitShapes : boolean;
    FFractionDigits  : integer;
    FLEDContrast     : TContrast;
    FSegmentOffColor : TColor;
    FLineWidth,
    FNumDigits       : integer;
    FLeadingZeros    : boolean;
    FSegCl           : array [0..9, 1..7] of TColor;
    FSegmentStyle    : TSegmentStyle;
    FValue           : extended;

    FOnChange        : TNotifyEvent;

    procedure SetBevelStyle (newValue: TPanelBevel);
    procedure SetBorderStyle (newValue: TBorderStyle);
    procedure SetColorBackGround (newValue: TColor);
    procedure SetColorLED (newValue: TColor);
    procedure SetDecSeparator (newValue: TDecSeparator);
    procedure SetDigitHeight (newValue: integer);
    procedure SetDigitWidth (newValue: integer);
    procedure SetDrawDigitShapes(newValue: boolean);
    procedure SetFractionDigits (newValue: integer);
    procedure SetLeadingZeros (newValue: boolean);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetLineWidth (newValue: integer);
    procedure SetNumDigits (newValue: integer);
    procedure SetSegmentStyle (newValue: TSegmentStyle);
    procedure SetValue (newValue: extended);

    procedure CreateDigitBitmaps;
    procedure AssignColors (seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);

  protected
    procedure Paint; override;
    procedure Change; dynamic;


  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ColorBackGround: TColor read FColorBackGround write setColorBackGround default clOlive;
    property ColorLED: TColor read FColorLED write setColorLED default cllime;
    property DecSeparator: TDecSeparator read FDecSeparator write setDecSeparator;
    property DigitHeight: integer read FDigitHeight write setDigitHeight default 30;
    property DigitWidth: integer read FDigitWidth write setDigitWidth default 20;
    property DigitLineWidth: integer read FLineWidth write setLineWidth default 3;
    property DrawDigitShapes: boolean read FDrawDigitShapes write SetDrawDigitShapes;
    property FractionDigits: integer read FFractionDigits write setFractionDigits default 0;
    property Height default 36;
    property LeadingZeros: boolean read FLeadingZeros write setLeadingZeros default true;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property NumDigits: integer read FNumDigits write setNumDigits default 6;
    property SegmentStyle: TSegmentStyle read FSegmentStyle write setSegmentStyle;
    property Value: extended read FValue write setValue;
    property Visible;
    property Width default 168;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TLEDMeter = class(TGraphicControl)
  private
    FBevelStyle     : TPanelBevel;
    FColors         : TLEDMeterColors;
    FColor1Off,
    FColor2Off,
    FColor3Off      : TColor;
    FDirection      : TMeterDirection;
    FFallbackDelay  : byte;
    FFallbackTimer  : TTimer;
    FLEDContrast    : TContrast;
    FLEDPosition,
    FMax,FMin,
    FNumDigits,
    FPeakPosition   : integer;
    FPeakShowTime   : TDateTime;
    FPeakHoldTime   : byte;
    FPosition,
    FSection2Value,
    FSection3Value  : integer;
    FSingleLED      : boolean;

    FOnChange       : TNotifyEvent;

    procedure CalculateOffColors;
    procedure SetBevelStyle(newVal : TPanelBevel);
    procedure SetColors(newVal: TLEDMeterColors);
    procedure SetDirection(newVal : TMeterdirection);
    procedure SetFallbackDelay(newVal : byte);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetMax(newVal : integer);
    procedure SetMin(newVal : integer);
    procedure SetNumDigits(newVal : integer);
    procedure SetPosition(newVal : integer);
    procedure SetSingleLED(newVal : boolean);
    procedure SetSection2Value(newVal : integer);
    procedure SetSection3Value(newVal : integer);

  protected
    procedure Paint;override;
    procedure Change; dynamic;
    function GetLEDColor(const AIndex,YellowIdx,RedIdx:integer;const LEDState:TLEDStates):TColor;
    procedure TimerExpired(Sender: TObject);

  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy ; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property Colors : TLEDMeterColors read FColors write SetColors;
    property Cursor;
    property Direction: TMeterDirection read FDirection write SetDirection;
    property DragCursor;
    property DragMode;
    property FallbackDelay: byte read FFallbackDelay write SetFallbackDelay;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property Max: integer read FMax write SetMax;
    property Min: integer read FMin write SetMin;
    property NumDigits: integer read FNumDigits write SetNumDigits;
    property PeakHoldTime: byte read FPeakHoldTime write FPeakHoldTime;
    property Position: integer read FPosition write SetPosition;
    property SingleLED: boolean read FSingleLED write SetSingleLED;
    property Section2Value: integer read FSection2Value write SetSection2Value;
    property Section3Value: integer read FSection3Value write SetSection3Value;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
uses rrColors {$IFNDEF SR_Delphi5_Up}, SRUtils{$ENDIF};
{$ELSE}
{$R *.D16}
uses SRUtils;
{$ENDIF}


const
  DefaultWidth   = 45;
  DefaultHeight  = 45;
  DefaultDepth   = 3;
  FHLContrast    = 5;
  FShContrast    = 4;
  FShapeContrast = 3;


function IsAccellerator(VK: Word; const Str: string): Boolean;
var P : Integer;
begin
  P:=Pos('&', Str);
  Result:=(P <> 0) and (P < Length(Str)) and
          (Upcase(Str[P + 1])=Upcase(Char(VK)));
end; {IsAccellerator}

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  { Rot }
  Red:=GetRValue(RGBColor);
  NewR:=Red+(Percentage*Red div 100);
  if NewR>255 then begin
    Overflow:=NewR-255;
    NewG:=Overflow;
    NewB:=Overflow;
  end
  else begin
    NewG:=0;
    NewB:=0;
  end;
  { Grün }
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  { Blau }
  Blue:=GetBValue(RGBColor);
  NewB:=NewB+Blue+(Percentage*Blue div 100);
  if NewB>255 then begin
    Overflow:=NewB-255;
    if NewG<=255 then
      NewR:=NewR+Overflow;
  end;
  if NewR>255 then
    NewR:=255;
  if NewG>255 then
    NewG:=255;
  if NewB>255 then
    NewB:=255;
  if NewR<0 then
    NewR:=0;
  if NewG<0 then
    NewG:=0;
  if NewB<0 then
    NewB:=0;
  Result:=NewR+(NewG shl 8)+(NewB shl 16);
end; {ChangeBrightness}
{$ENDIF}

function GetIntermediateColor(Color1,Color2:TColor;AContrast:integer):TColor;
{$IFDEF SR_Delphi2_Up}
var YR,YG,YB,SR,
    SG,SB,DR,DG,DB,
    StartClr,EndClr : integer;
{$ENDIF}
begin
  {$IFDEF SR_Delphi1}
  Result:=ChangeBrightness(Color1, -100 div 10*AContrast);
  {$ELSE}
  StartClr:=ColorToRGB(Color1);
  YR := GetRValue(StartClr);
  YG := GetGValue(StartClr);
  YB := GetBValue(StartClr);
  SR := YR;
  SG := YG;
  SB := YB;
  EndClr:=ColorToRGB(Color2);
  DR := GetRValue(EndClr)-SR;
  DG := GetGValue(EndClr)-SG;
  DB := GetBValue(EndClr)-SB;
  YR := SR + round(DR / 9 * AContrast);
  YG := SG + round(DG / 9 * AContrast);
  YB := SB + round(DB / 9 * AContrast);
  Result := RGB( YR, YG, YB);
  {$ENDIF}
end; {GetIntermediateColor}

function GetOffColor(const OnColor:TColor;const AContrast:TContrast):TColor;
var Dummy : TCOlor;
begin
  {$IFDEF SR_Delphi1}
  Result:=ChangeBrightness(OnColor, -100 div 10*AContrast);
  {$ELSE}
  Get3DColors(OnColor, Dummy, Result,(10-AContrast)/10,(10-AContrast)/10);
  {$ENDIF}
end; {CalculateOffColor}

procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  {$IFDEF SR_Delphi1}
  HighlightColor:=ChangeBrightness(FaceColor,100 div 10*HLContrast);
  ShadowColor:=ChangeBrightness(FaceColor,-100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FaceColor,HighlightColor,ShadowColor,(10-HLContrast)/10,(10-ShContrast)/10);
  {$ENDIF}
end; {AssignBevelColors}


{ Klasse TLEDMeterColors }
constructor TLEDMeterColors.Create(AOwner: TComponent);
begin
  inherited Create;

  if AOwner is TLEDMeter then
    FOwner := TLEDMeter(AOwner)
  else
    FOwner:=nil;
  FBorder:=clBlack;
  FSection1:=clLime;
  FSection2:=clYellow;
  FSection3:=clRed;
end;

procedure TLEDMeterColors.Assign(Source: TPersistent);
begin
  Border:=TLEDMeterColors(Source).Border;
  Section1:=TLEDMeterColors(Source).Section1;
  Section2:=TLEDMeterColors(Source).Section2;
  Section3:=TLEDMeterColors(Source).Section3;
end;

procedure TLEDMeterColors.SetBorder(NewValue: TColor);
begin
  if NewValue<>FBorder then begin
    FBorder:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TLEDMeterColors.SetSection1(NewValue: TColor);
begin
  if NewValue<>FSection1 then begin
    FSection1:=NewValue;
    if Assigned(FOwner) then
      FOwner.CalculateOffColors;
  end;
end;

procedure TLEDMeterColors.SetSection2(NewValue: TColor);
begin
  if NewValue<>FSection2 then begin
    FSection2:=NewValue;
    if Assigned(FOwner) then
      FOwner.CalculateOffColors;
  end;
end;

procedure TLEDMeterColors.SetSection3(NewValue: TColor);
begin
  if NewValue<>FSection3 then begin
    FSection3:=NewValue;
    if Assigned(FOwner) then
      FOwner.CalculateOffColors;
  end;
end;


{ Komponente TLEDButton }
constructor TLEDButton.Create(AOwner: TComponent);
var Dummy : TColor;
begin
  inherited Create(AOwner);

  { Vorgabewerte setzen }
  FBeveled:=true;
  FBorderStyle:=bsSingle;
  FButtonDirection:=bdBottomUp;
  FColor:=clGray;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FColorLED:=clBlue;
  FLEDContrast:=6;
  AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
  FDepth:=DefaultDepth;
  FDown:=false;
  FFont:=TFont.Create;
  FGlyph:=TBitmap.Create;
  FNumGlyphs:=1;
  FShowLED:=true;
  FStateOn:=false;
  FSwitching:=true;
  FTextPosition:=tpNone;
  Height:=DefaultHeight;
  Width:=DefaultWidth;

  FMouseDown:=False;
end;

destructor  TLEDButton.Destroy;
begin
  FFont.Free;
  FGlyph.Free;
  inherited Destroy;
end;

procedure TLEDButton.SetBeveled(NewValue: boolean);
begin
  if FBeveled<>NewValue then begin
    FBeveled:=NewValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetBorderStyle(NewBorderStyle: TBorderStyle);
begin
  if FBorderStyle<>NewBorderStyle then begin
    FBorderStyle:=NewBorderStyle;
    Invalidate;
  end;
end;

procedure TLEDButton.SetButtonDirection(NewDirection: TButtonDirection);
begin
  if FButtonDirection<>NewDirection then begin
    FButtonDirection:=NewDirection;
    Invalidate;
  end;
end;

procedure TLEDButton.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetColorLED(newColor: TColor);
var Dummy : TColor;
begin
  if FColorLED<>newColor then begin
    FColorLED:=newColor;
    AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetDepth(newValue: integer);
begin
  if FDepth<>newValue then begin
    FDepth:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetFont(newFont: TFont);
begin
  FFont.Assign(NewFont);
  Invalidate;
end;

procedure TLEDButton.SetGlyph(newGlyph: TBitmap);
begin
  if(Assigned(FGlyph)) then begin
    FGlyph.Assign(newGlyph);

    if (csDesigning in ComponentState) then begin
      { Glyph 1: Normal, 2: Disabled, 3: Down;
        Muß die Ausmaße (Height * NumGlyphs) = Width  haben}
      if (newGlyph.width mod newGlyph.height = 0) then
        FNumGlyphs:= newGlyph.width div newGlyph.height
      else
        FNumGlyphs:= 1;
    end;

    Invalidate;
  end;
end;

procedure TLEDButton.SetLEDContrast(newContrast: TContrast);
var Dummy : TColor;
begin
  if (FLEDContrast<>newContrast) and (newContrast>=0) and (newContrast<10) then begin
    FLEDContrast:=newContrast;
    AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetNumGlyphs(newNumGlyphs: TNumGlyphs);
begin
  if FNumGlyphs<>newNumGlyphs then begin
    FNumGlyphs:=newNumGlyphs;
    Invalidate;
  end;
end;

procedure TLEDButton.SetShowLED(newValue: boolean);
begin
  if FShowLED<>newValue then begin
    FShowLED:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetStateOn(newValue: boolean);
begin
  if FStateOn<>newValue then begin
    FStateOn:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetTextPosition(newValue: TTextPosition);
begin
  if FTextPosition<>newValue then begin
    FTextPosition:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.DrawBorder(Dest:TRect);
var i : integer;
begin
  Dest:=GetClientRect;
  if FTextPosition=tpAbove then
    Dest.Top:=Dest.Top+Canvas.TextWidth('W')+2;
  if FTextPosition=tpBelow then
    Dest.Bottom:=Dest.Bottom-Canvas.TextWidth('W')-2;
  with Canvas do begin
    if FBorderStyle=bsSingle then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Dest);
      InflateRect(Dest,-1,-1);
    end;
    Pen.Width:=1;
    if FButtonDirection=bdBottomUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Left,Dest.Top);
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          if FDown then
            LineTo(Dest.Left+(i div 2),Dest.Bottom-1)
          else
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          if FDown then
            LineTo(Dest.Right-(i div 2)-1,Dest.Bottom-1)
          else
            LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      { unten }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left,Dest.Bottom-i-1);
            LineTo(Dest.Right-1,Dest.Bottom-i-1);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Bottom-i-1);
            LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
          end;
        end;
    end;
    if FButtonDirection=bdTopUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left,Dest.Top+i);
            LineTo(Dest.Right-1,Dest.Top+i);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Top+i);
            LineTo(Dest.Right-i-1,Dest.Top+i);
          end;
        end;
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Left+(i div 2),Dest.Top)
          else
            LineTo(Dest.Left+i,Dest.Top+i);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Right-(i div 2)-1,Dest.Top)
          else
            LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      { unten }
      MoveTo(Dest.Right-1,Dest.Bottom-1);
      LineTo(Dest.Left,Dest.Bottom-1);
    end;
    if FButtonDirection=bdLeftUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          if FDown then
            LineTo(Dest.Left,Dest.Top+(i div 2))
          else
            LineTo(Dest.Left+i,Dest.Top+i);
        end;
      { links }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left+i,Dest.Top);
            LineTo(Dest.Left+i,Dest.Bottom-1);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Top+i);
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
          end;
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Right-1,Dest.Bottom-1);
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Bottom-1);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Left,Dest.Bottom-(i div 2)-1)
          else
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
    end;
    if FButtonDirection=bdRightUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          if FDown then
            LineTo(Dest.Right-1,Dest.Top+(i div 2))
          else
            LineTo(Dest.Right-1-i,Dest.Top+i);
        end;
      { links }
      MoveTo(Dest.Left,Dest.Top);
      LineTo(Dest.Left,Dest.Bottom-1);
      Pen.Color:=FColorShadow;
      { rechts }
      if FDown then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Right-1-i,Dest.Top);
            LineTo(Dest.Right-1-i,Dest.Bottom-1);
          end
          else begin
            MoveTo(Dest.Right-1-i,Dest.Top+i);
            LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
          end;
        end;
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Right-1,Dest.Bottom-(i div 2)-1)
          else
            LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
    end;
  end;
end;

procedure TLEDButton.DrawCaption(Dest:TRect);
var OutText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsClear;
    StrPCopy(OutText, Caption);
    if not Enabled then
      Font.Color:=clGrayText;
    if FTextPosition=tpAbove then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_top or dt_singleline);
    if FTextPosition=tpBelow then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_bottom or dt_singleline);
    if FTextPosition=tpOnButton then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_vcenter or dt_singleline);
  end;
end;

procedure TLEDButton.DrawGlyph(Dest:TRect);
var
  Source    : TRect;
  outWidth  : integer;
  outHeight : integer;
begin
  with Canvas do begin
    { Größe des Destination-Rechtecks }
    outWidth:=  FGlyph.Width div FNumGlyphs;
    outHeight:= FGlyph.Height;
    with Source do begin
      Top:=0;
      Bottom:=FGlyph.Height;
      { Glyph 1: Normal, 2: Disabled, 3: Down;}
      if Enabled then begin
        if FStateOn and (FNumGlyphs>2) then
          Left:=(outWidth*2)+1
        else
          Left:=0;
      end
      else
        Left:=outWidth+1;
      Right:= Left+outWidth;
    end;
    Dest.Left:=  ((Dest.Right +Dest.Left - outWidth)  shr 1);
    Dest.Right:= Dest.Left+outWidth;
    Dest.Top:=   ((Dest.Bottom + Dest.Top - outHeight) shr 1);
    Dest.Bottom:=Dest.Top+outHeight;
    Pen.Color := Color;
    BrushCopy(Dest,FGlyph,Source,FGlyph.Canvas.Pixels[0,FGlyph.Height-1]);
  end;
end;

procedure TLEDButton.DrawLED(var Dest:TRect);
begin
  with Canvas do begin
    if FStateOn then
      Brush.Color:=FColorLED
    else
      Brush.Color:=FColorLEDOff;
    if not Enabled then
      Brush.Color:=FColor;
    case ButtonDirection of
      bdLeftUp : begin
        if FDown then
          OffsetRect(Dest,-FDepth div 2,0);
        Rectangle(Dest.Left+FDepth+9,Dest.Top+FDepth+3,Dest.Left+FDepth+4,Dest.Bottom-FDepth-3);
        Dest.Left:=Dest.Left+FDepth+9;
      end;
      bdRightUp : begin
        if FDown then
          OffsetRect(Dest,FDepth div 2,0);
        Rectangle(Dest.Right-FDepth-9,Dest.Top+FDepth+3,Dest.Right-FDepth-4,Dest.Bottom-FDepth-3);
        Dest.Right:=Dest.Right-FDepth-9;
      end;
      bdTopUp : begin
        if FDown then
          OffsetRect(Dest,0,-FDepth div 2);
        Rectangle(Dest.Left+FDepth+3,Dest.Top+FDepth+4,Dest.Right-FDepth-3,Dest.Top+FDepth+9);
        Dest.Top:=Dest.Top+FDepth+7;
      end;
      else begin
        if FDown then
          OffsetRect(Dest,0,FDepth div 2);
        Rectangle(Dest.Left+FDepth+3,Dest.Bottom-FDepth-9,Dest.Right-FDepth-3,Dest.Bottom-FDepth-4);
        Dest.Bottom:=Dest.Bottom-FDepth-7;
      end;
    end;
  end;
end;

procedure TLEDButton.Paint;
var ARect : TRect;
begin
  Canvas.Font.Assign(Font);
  with Canvas do begin
    ARect:=GetClientRect;
    if (Caption<>'') and (FTextPosition<>tpOnButton) and (FTextPosition<>tpNone) then
      DrawCaption(ARect);
    if FTextPosition=tpAbove then
      ARect.Top:=ARect.Top+TextWidth('W')+2;
    if FTextPosition=tpBelow then
      ARect.Bottom:=ARect.Bottom-TextWidth('W')-2;
    Brush.Style:=bsSolid;
    Brush.Color:=FColor;
    FillRect(ARect);
    DrawBorder(ARect);
    Pen.Color:=clBlack;
    if FShowLED then
      DrawLED(ARect);
    if (Caption<>'') and (FTextPosition=tpOnButton) then
      DrawCaption(ARect);
    Brush.Color:=Self.Color;
    if Assigned(FGlyph) and (FNumGlyphs > 0) and (FTextPosition<>tpOnButton) then
      DrawGlyph(ARect);
  end;
end;

procedure TLEDButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      if Enabled then begin
        Click;
        if FSwitching then
          FStateOn:=not FStateOn;
        Invalidate;
      end;
      Result := 1;
    end
    else
      inherited;
  end;
end;

procedure TLEDButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    FDown:=true;
    Invalidate;
  end;
  FMouseDown:= True;
end;

procedure TLEDButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    FDown:=false;
    if FSwitching then
      FStateOn:=not FStateOn;
    Paint;
    if Assigned(FOnClick) then
       FOnClick(Self);
  end;
  FMouseDown:= False;
end;

procedure TLEDButton.CMTextChanged(var msg: TMessage);
begin
  Invalidate;
end;


{ Komponente TButtonPanel }
constructor TButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Vorgabewerte setzen }
  FBeveled:=true;
  FBorderStyle:=bsSingle;
  FPanelDirection:=bdBottomUp;
  FColor:=clGray;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FDepth:=DefaultDepth;
  FShowLED:=true;
  Height:=DefaultHeight;
  Width:=DefaultWidth;
end;

destructor  TButtonPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TButtonPanel.SetBeveled(NewValue: boolean);
begin
  if FBeveled<>NewValue then begin
    FBeveled:=NewValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetBorderStyle(NewBorderStyle: TBorderStyle);
begin
  if FBorderStyle<>NewBorderStyle then begin
    FBorderStyle:=NewBorderStyle;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetPanelDirection(NewDirection: TButtonDirection);
begin
  if FPanelDirection<>NewDirection then begin
    FPanelDirection:=NewDirection;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TButtonPanel.SetDepth(newValue: integer);
begin
  if FDepth<>newValue then begin
    FDepth:=newValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetShowLED(newValue: boolean);
begin
  if FShowLED<>newValue then begin
    FShowLED:=newValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.DrawBorder(Dest:TRect);
var i : integer;
begin
  Dest:=GetClientRect;
  with Canvas do begin
    if FBorderStyle=bsSingle then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Dest);
      InflateRect(Dest,-1,-1);
    end;
    Pen.Width:=1;
    if FPanelDirection=bdBottomUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Left,Dest.Top);
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      { unten }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left,Dest.Bottom-i-1);
          LineTo(Dest.Right-1,Dest.Bottom-i-1);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Bottom-i-1);
          LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      end;
    end;
    if FPanelDirection=bdTopUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left,Dest.Top+i);
          LineTo(Dest.Right-1,Dest.Top+i);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Top+i);
          LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      end;
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          LineTo(Dest.Left+i,Dest.Top+i);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      { unten }
      MoveTo(Dest.Right-1,Dest.Bottom-1);
      LineTo(Dest.Left,Dest.Bottom-1);
    end;
    if FPanelDirection=bdLeftUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          LineTo(Dest.Left+i,Dest.Top+i);
        end;
      { links }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left+i,Dest.Top);
          LineTo(Dest.Left+i,Dest.Bottom-1);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Top+i);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      end;
      Pen.Color:=FColorShadow;
      { rechts }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Right-1,Dest.Bottom-1);
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Bottom-1);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
    end;
    if FPanelDirection=bdRightUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          LineTo(Dest.Right-1-i,Dest.Top+i);
        end;
      { links }
      MoveTo(Dest.Left,Dest.Top);
      LineTo(Dest.Left,Dest.Bottom-1);
      Pen.Color:=FColorShadow;
      { rechts }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Right-1-i,Dest.Top);
          LineTo(Dest.Right-1-i,Dest.Bottom-1);
        end
        else begin
          MoveTo(Dest.Right-1-i,Dest.Top+i);
          LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
      end;
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
    end;
  end;
end;

procedure TButtonPanel.DrawCaption(Dest:TRect);
var OutText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsClear;
    StrPCopy(OutText, Caption);
    DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_vcenter or dt_singleline);
  end;
end;

procedure TButtonPanel.DrawLED(var Dest:TRect);
begin
  with Canvas do begin
    Brush.Color:=clWindowFrame;
    case PanelDirection of
      bdLeftUp : begin
        FrameRect(Rect(Dest.Left+FDepth+9,Dest.Top+FDepth+3,Dest.Left+FDepth+4,Dest.Bottom-FDepth-3));
        Dest.Left:=Dest.Left+FDepth+9;
      end;
      bdRightUp : begin
        FrameRect(Rect(Dest.Right-FDepth-9,Dest.Top+FDepth+3,Dest.Right-FDepth-4,Dest.Bottom-FDepth-3));
        Dest.Right:=Dest.Right-FDepth-9;
      end;
      bdTopUp : begin
        FrameRect(Rect(Dest.Left+FDepth+3,Dest.Top+FDepth+4,Dest.Right-FDepth-3,Dest.Top+FDepth+9));
        Dest.Top:=Dest.Top+FDepth+7;
      end;
      else begin
        FrameRect(Rect(Dest.Left+FDepth+3,Dest.Bottom-FDepth-9,Dest.Right-FDepth-3,Dest.Bottom-FDepth-4));
        Dest.Bottom:=Dest.Bottom-FDepth-7;
      end;
    end;
  end;
end;

procedure TButtonPanel.Paint;
var ARect : TRect;
begin
  Canvas.Font.Assign(Font);
  with Canvas do begin
    ARect:=GetClientRect;
    Brush.Style:=bsSolid;
    Brush.Color:=FColor;
    FillRect(ARect);
    DrawBorder(ARect);
    Pen.Color:=clBlack;
    if FShowLED then
      DrawLED(ARect);
    if Caption<>'' then
      DrawCaption(ARect);
  end;
end;


{ Komponente TScrewPanel }
constructor TScrewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {Vorgabewerte setzen}
  FColor:=clBtnFace;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FMargin:=2;
  FScrewSize:=2;
  FShowScrews:=true;
  Height:=DefaultHeight;
  Width:=DefaultWidth;
end;

destructor TScrewPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TScrewPanel.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TScrewPanel.SetMargin(newValue: integer);
begin
  if (NewValue<(Width-FScrewSize)) and (NewValue<(Height-FScrewSize)) and (FMargin<>newValue) then begin
    FMargin:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.SetScrewSize(newValue: TScrewSize);
begin
  if (NewValue<Width) and (NewValue<Height) and (FScrewSize<>newValue) then begin
    FScrewSize:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.SetShowScrews(newValue: boolean);
begin
  if FShowScrews<>newValue then begin
    FShowScrews:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.DrawScrew(X,Y:integer);
var Size : integer;
begin
  Size:=FScrewSize*4;
  with Canvas do begin
    Pen.Color:=FColorShadow;
    Brush.Color:=clSilver;
    Ellipse(X,Y,X+Size,Y+Size);
    Arc(X,Y,X+Size,Y+Size,
        X+((Size div 4)*3),Y+(Size div 4),
        X+(Size div 4),Y+((Size div 4)*3));
    Pen.Color:=clGray;
    MoveTo(X+(Size div 4)-1,Y+((Size div 4)*3)-1);
    LineTo(X+((Size div 4)*3),Y+(Size div 4)-2);
    Pen.Color:=FColorHighlight;
    Arc(X,Y,X+Size,Y+Size,
        X+(Size div 4),Y+((Size div 4)*3),
        X+((Size div 4)*3),Y+(Size div 4));
    Pen.Color:=clWhite;
    MoveTo(X+(Size div 4),Y+((Size div 4)*3));
    LineTo(X+((Size div 4)*3)+1,Y+(Size div 4)-1);
  end;
end;

procedure TScrewPanel.DrawBevel(ARect:TRect;Raised:boolean);
begin
  with Canvas do begin
    Pen.Width:=BevelWidth;
    if Raised then
      Pen.Color:=FColorHighlight
    else
      Pen.Color:=FColorShadow;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Left,ARect.Top);
    LineTo(ARect.Left,ARect.Bottom-1);
    if Raised then
      Pen.Color:=FColorShadow
    else
      Pen.Color:=FColorHighlight;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Right-1,ARect.Bottom-1);
    LineTo(ARect.Left,ARect.Bottom-1);
  end;
end;

procedure TScrewPanel.Paint;
var ARect   : TRect;
    Border  : integer;
    outText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsSolid;
    Brush.Color:=Self.Color;
    ARect:=GetClientRect;
    FillRect(ARect);
    if BevelOuter<>bvNone then begin
      DrawBevel(ARect,BevelOuter=bvRaised);
      Border:=BevelWidth+BorderWidth;
    end
    else
      Border:=BorderWidth;
    InflateRect(ARect,-Border,-Border);
    if BevelInner<>bvNone then begin
      DrawBevel(ARect,BevelInner=bvRaised);
      InflateRect(ARect,-BevelWidth,-BevelWidth);
    end;
    if FShowScrews then begin
      DrawScrew(ARect.Left+FMargin,ARect.Top+FMargin);
      DrawScrew(ARect.Right-FMargin-(FScrewSize*4),ARect.Top+FMargin);
      DrawScrew(ARect.Left+FMargin,ARect.Bottom-FMargin-(FScrewSize*4));
      DrawScrew(ARect.Right-FMargin-(FScrewSize*4),ARect.Bottom-FMargin-(FScrewSize*4));
    end;
    Font:=Self.Font;
    Brush.Style:=bsClear;
    StrPCopy(outText,Caption);
    if Alignment=taCenter then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_CENTER);
    if Alignment=taLeftJustify then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    if Alignment=taRightJustify then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_RIGHT);
  end;
end;


{ Komponente TLEDDisplay }
constructor TLEDDisplay.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);

  FBevelStyle:= bvLowered;
  FBorderStyle:= bsSingle;
  FColorBackGround:= clBlack;
  FColorLED:= clLime;
  FLEDContrast:=6;
  FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
  FDigitShapeColor := GetIntermediateColor(FColorBackground, clBlack, FShapeContrast);
  FDrawDigitShapes := true;
  FDecSeparator:= dsComma;
  FDigitHeight:= 30;
  FDigitWidth:= 20;
  FLineWidth:= 3;
  FLeadingZeros:= true;
  FNumDigits:= 6;
  FSegmentStyle:= ssBeveled;
  FValue:= 0;
  Height:= 36;
  Width:= 168;

  CreateDigitBitmaps;
end;

destructor TLEDDisplay.Destroy;
begin
  inherited destroy;
end;

procedure TLEDDisplay.AssignColors(seg:integer; s1,s2,s3,s4,s5,s6,s7:Boolean);
begin
  if s1 then
    FSegCl[seg, 1] := FColorLED
  else
    FSegCl[seg, 1] := FSegmentOffColor;
  if s2 then
    FSegCl[seg, 2] := FColorLED
  else
    FSegCl[seg, 2] := FSegmentOffColor;
  if s3 then
    FSegCl[seg, 3] := FColorLED
  else
    FSegCl[seg, 3] := FSegmentOffColor;
  if s4 then
    FSegCl[seg, 4] := FColorLED
  else
    FSegCl[seg, 4] := FSegmentOffColor;
  if s5 then
    FSegCl[seg, 5] := FColorLED
  else
    FSegCl[seg, 5] := FSegmentOffColor;
  if s6 then
    FSegCl[seg, 6] := FColorLED
  else
    FSegCl[seg, 6] := FSegmentOffColor;
  if s7 then
    FSegCl[seg, 7] := FColorLED
  else
    FSegCl[seg, 7] := FSegmentOffColor;
end;

procedure TLEDDisplay.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLEDDisplay.CreateDigitBitmaps;
var
  TL, TR, TBL, TBR,
  ML, MTL, MTR, MR,
  MBL, MBR, BL, BTL,
  BTR, BR            : TPoint;
  c, wAlt, LineW     : integer;
begin
  LineW := FLineWidth+2;
  wAlt := FDigitHeight;
  { Polygonpunkte zuweisen }
  TL.x := 0;
  TL.y := 0;
  TR.x := FDigitWidth-1;
  TR.y := 0;
  TBL.x := LineW - 1;
  TBL.y := LineW -1;
  TBR.x := FDigitWidth - LineW;
  TBR.y := TBL.y;
  ML.x := 0;
  ML.y := wAlt div 2;
  MTL.x := TBL.x;
  MTL.y := ML.y - (LineW div 2);
  MTR.x := TBR.x;
  MTR.y := MTL.y;
  MR.x := TR.x;
  MR.y := ML.y;
  MBL.x := TBL.x;
  MBL.y := ML.y + (LineW div 2);
  MBR.x := MTR.x; MBR.y := MBL.y;
  BL.x := 0;
  BL.y := wAlt - 1;
  BR.x := TR.x;
  BR.y := BL.y;
  BTL.x := TBL.x;
  BTL.y := wAlt - LineW;
  BTR.x := TBR.x;
  BTR.y := BTL.y;

  { Segmentfarben zuweisen }
  AssignColors (0,true,true,true,false,true,true,true);
  AssignColors (1,false,false,true,false,false,true,false);
  AssignColors (2,true,false,true,true,true,false,true);
  AssignColors (3,true,false,true,true,false,true,true);
  AssignColors (4,false,true,true,true,false,true,false);
  AssignColors (5,true,true,false,true,false,true,true);
  AssignColors (6,false,true,false,true,true,true,true);
  AssignColors (7,true,false,true,false,false,true,false);
  AssignColors (8,true,true,true,true,true,true,true);
  AssignColors (9,true,true,true,true,false,true,true);

  { Bitmap erstellen }
  for c := 0 to 9 do begin
    FDigit[c].free;
    FDigit[c] := TBitmap.create;
    FDigit[c].width := FDigitWidth;
    FDigit[c].height := wAlt;
    with FDigit[c].canvas do begin
      if FDrawDigitShapes then
        Pen.Color := FDigitShapeColor
      else
        Pen.Color := FColorBackGround;
      Brush.Color := FColorBackGround;
      Brush.style := bsSolid;
      Pen.Width := 1;
      Rectangle (TL.x, TL.y, BR.x+1, BR.y+1);
      if FSegmentStyle=ssRectangular then
        Pen.Width:=FLineWidth;
      { Segment 1 }
      Brush.Color := FSegCl[c, 1];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 1];
        MoveTo(FLineWidth, FLineWidth div 2);
        LineTo(FDigit[c].Width-FLineWidth-1, FLineWidth div 2);
      end
      else
        Polygon ([TL, TR, TBR, TBL]);
      { Segment 2 }
      Brush.Color := FSegCl[c, 2];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 2];
        MoveTo(FLineWidth div 2, FLineWidth*3 div 2);
        LineTo(FLineWidth div 2, (FDigit[c].Height div 2)-FLineWidth);
      end
      else
        Polygon ([TL, TBL, MTL, ML]);
      { Segment 3 }
      Brush.Color := FSegCl[c, 3];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 3];
        MoveTo(FDigit[c].Width-(FLineWidth div 2)-1, FLineWidth*3 div 2);
        LineTo(FDigit[c].Width-(FLineWidth div 2)-1, (FDigit[c].Height div 2)-FLineWidth);
      end
      else
        Polygon ([TR, MR, MTR, TBR]);
      { Segment 4 }
      Brush.Color := FSegCl[c, 4];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 4];
        MoveTo(FLineWidth, FDigit[c].Height div 2);
        LineTo(FDigit[c].Width-FLineWidth, FDigit[c].Height div 2);
      end
      else
        Polygon ([ML, MTL, MTR, MR, MBR, MBL]);
      { Segment 5 }
      Brush.Color := FSegCl[c, 5];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 5];
        MoveTo(FLineWidth div 2, (FDigit[c].Height div 2)+FLineWidth);
        LineTo(FLineWidth div 2, FDigit[c].Height-(FLineWidth*3 div 2));
      end
      else
        Polygon ([ML, MBL, BTL, BL]);
      { Segment 6 }
      Brush.Color := FSegCl[c, 6];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 6];
        MoveTo(FDigit[c].Width-(FLineWidth div 2)-1, (FDigit[c].Height div 2)+FLineWidth);
        LineTo((FDigit[c].Width-FLineWidth div 2)-1, FDigit[c].Height-(FLineWidth*3 div 2));
      end
      else
        Polygon ([MR, BR, BTR, MBR]);
      { Segment 7 }
      Brush.Color := FSegCl[c, 7];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 7];
        MoveTo(FLineWidth, FDigit[c].Height-(FLineWidth div 2)-1);
        LineTo(FDigit[c].Width-FLineWidth, FDigit[c].Height-(FLineWidth div 2)-1);
      end
      else
        Polygon ([BL, BTL, BTR, BR]);
    end;
  end;
end;

procedure TLEDDisplay.Paint;
var
  Area        : TRect;
  outText     : string;
  DigitSpace,
  DigitLeft,
  DigitTop,
  DigitNum,
  SepLeft,
  SepTop,
  SepWidth,
  SepPosition : integer;
  ANZeroDigit : Boolean;
begin
  Area := getClientRect;
  try
    outText:=FloatToStrF(FValue,ffFixed,18,FFractionDigits);
  except
    outText:='';
  end;
  SepPosition:=Pos(DecimalSeparator,outText);
  if SepPosition>0 then
    delete(outText,SepPosition,1);
  while length(outText)<FNumDigits do begin
    outText:='0'+outText;
    if SepPosition>0 then
      inc(SepPosition);
  end;
  ANZeroDigit:=False; { bis jetzt noch keine Ziffer von 1..9 }
  with Canvas do begin
    Brush.Color:=FColorBackGround;
    FillRect(Area);
    DigitSpace:=Self.Width div FNumDigits;
    DigitTop:=(Self.Height-DigitHeight) div 2;
    DigitLeft:=(DigitSpace-FDigitwidth) div 2;
    Brush.Color:=FColorLED;
    Pen.Color:=FColorLED;
    { Bitmaps und DecSeperator zeichnen }
    for DigitNum:=1 to FNumDigits do begin
      { nachfolgende Nullen müssen gezeichnet werden! }
      if FLeadingZeros or (StrToInt(outText[DigitNum])<>0) or ANZeroDigit then begin
        Draw(DigitLeft, DigitTop, FDigit[StrToInt(outText[DigitNum])]);
        ANZeroDigit:=True; { spätestens jetzt isse da... }
      end;
      inc(DigitLeft, DigitSpace);
      if DigitNum=(SepPosition-1) then begin
        { Dezimalseparator }
        Pen.Width:=1;
        if FDrawDigitShapes then
          Pen.Color:=FDigitShapeColor
        else
          Pen.Color:=FColorBackground;
        SepLeft:=DigitLeft-6;
        if (FDecSeparator=dsDoublePoint) or (FDecSeparator=dsSemicolon) then begin
          SepTop:=DigitTop+((Self.Height-4) div 3)-2;
          Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);
        end;
        case FDecSeparator of
          dsDoublePoint,
          dsSemicolon   : SepTop:=DigitTop+((Self.Height-4)*2 div 3)-2;
          dsPoint       : SepTop:=DigitTop+FDigitHeight-FLineWidth-1;
          dsComma       : SepTop:=DigitTop+FDigitHeight-(FLineWidth*2)-1;
          dsApostrophe  : SepTop:=DigitTop;
          else            SepTop:=(Self.Height-FLineWidth) div 2;
        end;
        if (FDecSeparator=dsSemicolon) or (FDecSeparator=dsComma) or (FDecSeparator=dsApostrophe) then
          Polygon([Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth div 2)),
                   Point(SepLeft+FLineWidth, SepTop+(FLineWidth div 2)),
                   Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth * 2))]);
        if (FDecSeparator=dsDoublePoint) or (FDecSeparator=dsPoint) or
         (FDecSeparator=dsSemicolon) or (FDecSeparator=dsComma) or (FDecSeparator=dsApostrophe) then
          Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);
        if FDecSeparator=dsHyphen then begin
          SepWidth:=round((DigitSpace-FDigitWidth)/3*2);
          SepLeft:=DigitLeft-(DigitSpace-FDigitWidth)+((DigitSpace-FDigitWidth-SepWidth) div 2);
          Rectangle(SepLeft, SepTop, SepLeft+SepWidth, SepTop+FLineWidth+1);
        end;
      end;
    end;
    { Bevel zeichnen }
    if BevelStyle<>bvNone then begin
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnHighlight
      else
        Pen.Color:=clBtnShadow;
      MoveTo(Area.Right-1,Area.Top);
      LineTo(Area.Left,Area.Top);
      LineTo(Area.Left,Area.Bottom-1);
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnShadow
      else
        Pen.Color:=clBtnHighlight;
      MoveTo(Area.Left,Area.Bottom-1);
      LineTo(Area.Right-1,Area.Bottom-1);
      LineTo(Area.Right-1,Area.Top);
      InflateRect(Area,-1,-1);
    end;
    { Border zeichnen }
    if BorderStyle<>bsNone then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Area);
    end;
  end;
end;

procedure TLEDDisplay.SetBevelStyle(newValue: TPanelBevel);
begin
  if FBevelStyle<>newValue then begin
    FBevelStyle:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetBorderStyle(newValue: TBorderStyle);
begin
  if FBorderStyle<>newValue then begin
    FBorderStyle:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorBackGround(newValue: TColor);
begin
  if FColorBackGround<>NewValue then begin
    FColorBackGround:=NewValue;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorLED(newValue: TColor);
begin
  if FColorLED<>newValue then begin
    FColorLED:=newValue;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDecSeparator(newValue: TDecSeparator);
begin
  if FDecSeparator<>newValue then begin
    FDecSeparator:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitHeight(newValue: integer);
begin
  if FDigitHeight<>newValue then begin
    FDigitHeight:=newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitWidth(newValue: integer);
begin
  if FDigitWidth<>newValue then begin
    FDigitWidth:=newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDrawDigitShapes(newValue: boolean);
begin
  if newValue<>FDrawDigitShapes then begin
    FDrawDigitShapes := newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetFractionDigits(newValue: integer);
begin
  if FFractionDigits<>newValue then begin
    FFractionDigits:=newValue;
    if FFractionDigits>(FNumDigits-1) then
      FFractionDigits:=FNumDigits-1;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLeadingZeros(newValue: boolean);
begin
  if FLeadingZeros<>newValue then begin
    FLeadingZeros:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLEDContrast(newContrast: TContrast);
begin
  if (FLEDContrast<>newContrast) and (newContrast>=0) and (newContrast<10) then begin
    FLEDContrast:=newContrast;
    FSegmentOffColor := GetIntermediateColor(FColorLED, FColorBackground, FLEDContrast);
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLineWidth(newValue: integer);
begin
  if FLineWidth<>newValue then begin
    FLineWidth:=newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetNumDigits(newValue: integer);
begin
  if FNumDigits<>newValue then begin
    FNumDigits:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetSegmentStyle(newValue: TSegmentStyle);
begin
  if FSegmentStyle<>newValue then begin
    FSegmentStyle:=newValue;
    CreateDigitBitmaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetValue(newValue: extended);
begin
  if FValue<>NewValue then begin
    FValue:=NewValue;
    CreateDigitBitmaps;
    Invalidate;
    Change;
  end;
end;

{ Komponente TLEDMeter }

constructor TLEDMeter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColors := TLEDMeterColors.Create(Self);
  FFallbackTimer := TTimer.Create(Self);
  FFallbackTimer.OnTimer:=TimerExpired;
  FFallbackTimer.Enabled:=false;


  FBevelStyle:=bvlowered;
  FDirection:=mdRight;
  FMax:=100;
  FMin:=0;
  FNumDigits:=20;
  FPeakHoldTime:=0;
  FPosition:=0;
  FSection2Value:=75;
  FSection3Value:=90;
  Height:=16;
  Width:=143;
end; {Create}

destructor TLEDMeter.Destroy;
begin
  if assigned(FColors) then
    FreeAndNil(FColors);
  if assigned(FFallbackTimer) then
    FreeAndNil(FFallbackTimer);
  inherited Destroy;
end; {Destroy}

procedure TLEDMeter.CalculateOffColors;
begin
  FColor1Off:=GetOffColor(FColors.Section1, FLEDContrast);
  FColor2Off:=GetOffColor(FColors.Section2, FLEDContrast);
  FColor3Off:=GetOffColor(FColors.Section3, FLEDContrast);
  Invalidate;
end; {CalculateOffColors}

procedure TLEDMeter.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end; {Change}

function TLEDMeter.GetLEDColor(const AIndex,YellowIdx,RedIdx:integer;const LEDState:TLEDStates):TColor;
begin
  if LEDState=lsOff then begin
    if AIndex<YellowIdx then
      Result:=FColor1Off
    else begin
      if AIndex<RedIdx then
        Result:=FColor2Off
      else
        Result:=FColor3Off;
    end;
  end
  else begin
    if AIndex<YellowIdx then
      Result:=FColors.Section1
    else begin
      if AIndex<RedIdx then
        Result:=FColors.Section2
      else
        Result:=FColors.Section3;
    end;
  end;
end; {GetLEDColor}

procedure TLEDMeter.Paint;
var ARect       : TRect;
    LEDNr,
    ARange,
    PeakIdx,
    PositionIdx,
    FirstYel,
    FirstRed,
    DigitLeft,
    DigitTop,
    DigitWidth,
    DigitHeight,
    BevelWidth  : integer;
begin
  with Canvas do begin
    ARange:=FMax-FMin;
    { LED-Ausmaße berechnen }
    if BevelStyle<>bvNone then
      BevelWidth:=1
    else
      BevelWidth:=0;
    if (FDirection=mdRight) or (FDirection=mdLeft) then begin
      DigitWidth:=(Width-(2*BevelWidth)) div FNumDigits;
      DigitHeight:=Height-(2*BevelWidth);
      DigitLeft:=BevelWidth;
      DigitTop:=BevelWidth;
    end
    else begin
      DigitWidth:=Width-(2*BevelWidth)-1;
      DigitHeight:=(Height-(2*BevelWidth)) div FNumDigits;
      DigitTop:=BevelWidth;
      DigitLeft:=BevelWidth;
    end;
    { Erste LED über / unter FPosition ermitteln }
    if (ARange>0) and (FLEDPosition>0) then
      PositionIdx:=round(FNumDigits/(ARange/FLEDPosition))
    else
      PositionIdx:=0;
    if (ARange>0) and (FPeakPosition>0) then
      PeakIdx:=round(FNumDigits/(ARange/FPeakPosition))-1
    else
      PeakIdx:=0;
    { Positionen der grünen / gelben / roten LEDs ermitteln }
    if (ARange>0) and (FSection2Value>0) then
      FirstYel:=round(FNumDigits/(ARange/FSection2Value))
    else
      FirstYel:=0;
    if (ARange>0) and (FSection3Value>0) then
      FirstRed:=round(FNumDigits/(ARange/FSection3Value))
    else
      FirstRed:=0;
    { LEDs zeichnen }
    for LEDNr:=0 to FNumDigits-1 do begin
      { LED-Farbe ermitteln }
      if LEDNr>=PositionIdx then begin
        { LEDs über FPosition }
        if (FPeakHoldTime>0) and (LEDNr=PeakIdx) and (PeakIdx>0) then
          Brush.Color:=GetLEDColor(LEDNr, FirstYel, FirstRed, lsOn)
        else
          Brush.Color:=GetLEDColor(LEDNr, FirstYel, FirstRed, lsOff);
      end
      else begin
        { LEDs unter FPosition }
        if FSingleLED then begin
          if (LEDNr=PositionIdx-1) then
            Brush.Color:=GetLEDColor(LEDNr, FirstYel, FirstRed, lsOn)
          else
            Brush.Color:=GetLEDColor(LEDNr, FirstYel, FirstRed, lsOff);
        end
        else
          Brush.Color:=GetLEDColor(LEDNr, FirstYel, FirstRed, lsOn)
      end;
      { LED zeichnen }
      if FDirection=mdRight then
        DigitLeft:=BevelWidth+(LEDNr*DigitWidth);
      if FDirection=mdLeft then
        DigitLeft:=Width-BevelWidth-((LEDNr+1)*DigitWidth)-1;
      if FDirection=mdUp then
        DigitTop:=Height-BevelWidth-((LEDNr+1)*DigitHeight);
      if FDirection=mdDown then
        DigitTop:=BevelWidth+(LEDNr*DigitHeight);
      Pen.Color:=FColors.Border;
      Rectangle(DigitLeft, DigitTop, DigitLeft+DigitWidth+1, DigitTop+DigitHeight);
    end;
    if BevelStyle<>bvNone then begin
      { Bevel zeichnen }
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnHighlight
      else
        Pen.Color:=clBtnShadow;
      ARect:=GetClientRect;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-1);
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnShadow
      else
        Pen.Color:=clBtnHighlight;
      MoveTo(ARect.Left, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Top);
    end;
  end;
end; {Paint}

procedure TLEDMeter.SetBevelStyle(newVal: TPanelBevel);
begin
  if newVal<>FBevelStyle then begin
    FBevelStyle:=newVal;
    Invalidate;
  end;
end; {SetBevelStyle}

procedure TLEDMeter.SetColors(newVal: TLEDMeterColors);
begin
  with FColors do begin
    Border:=newVal.Border;
    Section1:=newVal.Section1;
    Section2:=newVal.Section2;
    Section3:=newVal.Section3;
  end;
  CalculateOffColors;
end; {SetColors}

procedure TLEDMeter.SetDirection(newVal: TMeterDirection);
begin
  if newVal<>FDirection then begin
    FDirection:=newVal;
    Invalidate;
  end;
end; {SetDirection}

procedure TLEDMeter.SetFallbackDelay(newVal : byte);
begin
  if newVal<>FFallbackDelay then begin
    FFallbackDelay:=newVal;
    FFallbackTimer.Interval:=FFallbackDelay;
  end;
end; {SetFallbackDelay}

procedure TLEDMeter.SetLEDContrast(newContrast: TContrast);
begin
  if newContrast<>FLEDContrast then begin
    FLEDContrast:=newContrast;
    CalculateOffColors;
  end;
end; {SetLEDContrast}

procedure TLEDMeter.SetMax(newVal: integer);
begin
  if newVal<>FMax then begin
    FMax:=newVal;
    if newVal<FSection2Value then
      FSection2Value:=newVal;
    if newVal<FSection3Value then
      FSection3Value:=newVal;
    Invalidate;
  end;
end; {SetMax}

procedure TLEDMeter.SetMin(newVal: integer);
begin
  if newVal<>FMin then begin
    FMin:=newVal;
    if newVal>FSection2Value then
      FSection2Value:=newVal;
    if newVal>FSection3Value then
      FSection3Value:=newVal;
    Invalidate;
  end;
end; {SetMin}

procedure TLEDMeter.SetNumDigits(newVal: integer);
begin
  if newVal<>FNumDigits then begin
    FNumDigits:=newVal;
    Invalidate;
  end;
end; {SetNumDigits}

procedure TLEDMeter.SetPosition(newVal: integer);
var OldPos,
    Diff   : integer;
begin
  if newVal<>FPosition then begin
    if newVal>FMax then
      newVal:=FMax;
    if newVal<FMin then
      newVal:=FMin;
    OldPos:=FPosition;
    FPosition:=newVal;
    if FPosition>=FPeakPosition then begin
      FPeakPosition:=FPosition;
      FPeakShowTime:=Now;
    end
    else begin
      Diff:=round((Now-FPeakShowTime)*SecsPerDay*10);
      if Diff>FPeakHoldTime then begin
        FPeakPosition:=FPosition;
        FPeakShowTime:=Now;
      end;
    end;
    if (FFallbackDelay>0) and (FPosition<OldPos) then begin
      FLEDPosition:=OldPos;
      FFallbackTimer.Enabled:=true;
    end
    else
      FLEDPosition:=FPosition;
    Paint;
    Change;
  end;
end; {SetPosition}

procedure TLEDMeter.SetSection2Value(newVal: integer);
begin
  if newVal<>FSection2Value then begin
    if newVal>Max then
      newVal:=Max;
    if newVal<Min then
      newVal:=Min;
    if newVal>FSection3Value then
      newVal:=FSection3Value;
    FSection2Value:=newVal;
    Invalidate;
  end;
end; {SetSection2Value}

procedure TLEDMeter.SetSection3Value(newVal: integer);
begin
  if newVal<>FSection3Value then begin
    if newVal>Max then
      newVal:=Max;
    if newVal<Min then
      newVal:=Min;
    if newVal<FSection2Value then
      newVal:=FSection2Value;
    FSection3Value:=newVal;
    Invalidate;
  end;
end; {SetSection3Value}

procedure TLEDMeter.SetSingleLED(newVal: boolean);
begin
  if newVal<>FSingleLED then begin
    FSingleLED:=newVal;
    Invalidate;
  end;
end; {SetSingleLED}

procedure TLEDMeter.TimerExpired(Sender: TObject);
begin
  if FLEDPosition>FPosition then begin
    dec(FLEDPosition);
    Paint;
  end
  else
    FFallbackTimer.Enabled:=false;
end; {TimerExpired}

procedure Register;
begin
  RegisterComponents('Simon', [TLEDButton]);
  RegisterComponents('Simon', [TButtonPanel]);
  RegisterComponents('Simon', [TScrewPanel]);
  RegisterComponents('Simon', [TLEDDisplay]);
  RegisterComponents('Simon', [TLEDMeter]);
end;

end.
