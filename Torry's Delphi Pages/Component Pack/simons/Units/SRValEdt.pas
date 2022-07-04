unit SRValEdt;

{ TSRValueEdit (C)opyright 2004 Version 1.01
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRValueEdit ist eine Kombination aus
  einem Drehknopf für die optische Anzeige des eingestellten
  Wertes und zwei weiteren Komponenten zur Einstellung des Wertes:

  - Einer TNumericEdit-Komponente für die alphanumerische
    Eingabe eines Zahlenwerts. Zahlenwerte können direkt eingetippt
    werden oder man kann über SpinButtons einzelne Stellen ändern.

  - Einer TSliderEdit-Komponente für die Einstellung eines
    Zahlenwerts mit der Maus. Dazu wird einfach ein Schiebesteller
    an die gewünschte Position gezogen.

  Zusätzlich werden die letzten vier eingestellten Werte gespeichert
  und können über ein Popup-Menü wieder abgerufen werden. Auf diese
  Weise stellt sie den optimalen Kompromiss zwischen minimalem
  Platzbedarf und optimaler Bedienbarkeit mit Maus und Tastatur zur
  Auswahl eines Zahlenwertes dar. 

  Die Komponenten sind Public Domain, das Urheberrecht liegt aber
  beim Autor. }


interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  Forms, Menus;

type
  TNESpinBtnClick  = Procedure (Sender: TObject; BtnIndex: integer; UpBtn: boolean) of object;
  TNumDigits       = 1..20;
  TScrewSize       = 5..15;
  TRulerDirection  = (rdHorizontal, rdVertical);
  TTickStyle       = (tsNone, tsMinMax, tsAll);
  TTriangleDest    = (tdTop, tdRight, tdBottom, tdLeft);


  TColors = class(TPersistent)
  private
    FBackground,
    FBorderHL,
    FBorderSh,
    FNumBackground,
    FNumFrame,
    FRuler,
    FSliderBorder,
    FSliderTop,
    FSliderMark,
    FTickMarks        : TColor;
    FOwner            : TCustomControl;

    procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
    procedure SetBackground(NewValue: TColor);
    procedure SetNumBackground(NewValue: TColor);
    procedure SetNumFrame(NewValue: TColor);
    procedure SetRuler(NewValue: TColor);
    procedure SetSliderBorder(NewValue: TColor);
    procedure SetSliderMark(NewValue: TColor);
    procedure SetSliderTop(NewValue: TColor);
    procedure SetTickMarks(NewValue: TColor);

  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source : TPersistent); override;

  published
    property Background: TColor read FBackground write SetBackground;
    property NumBackground: TColor read FNumBackground write SetNumBackground;
    property NumFrame: TColor read FNumFrame write SetNumFrame;
    property Ruler: TColor read FRuler write SetRuler;
    property SliderBorder: TColor read FSliderBorder write SetSliderBorder;
    property SliderTop: TColor read FSliderTop write SetSliderTop;
    property SliderMark: TColor read FSliderMark write SetSliderMark;
    property TickMarks: TColor read FTickMarks write SetTickMarks;

  end;

  TSRValueEdit = class;

  TNumericEdit = class(TCustomControl)
  private
    FAutoSize         : boolean;
    FBtnHeight        : integer;
    FDrawBuffer       : TBitmap;
    FColor            : TColor;
    FDecSeparator     : char;
    FDecSeparatorPos  : byte;
    FDigitWidth,
    FDownIdxTop,
    FDownIdxBtm       : integer;
    FEditPos          : byte;
    FFrameColor       : TColor;
    FHasFocus         : boolean;
    FMinValue,
    FMaxValue         : extended;
    FNumDigits        : TNumDigits;
    FValue            : extended;
    FValueEdit        : TSRValueEdit;
    FOnChange         : TNotifyEvent;
    FOnSpinBtnClick   : TNESpinBtnClick;

    procedure CalcControlHeight;
    procedure CalcDigitWidth;
    function GetMaxValue:extended;
    procedure IncreaseDigitValue(DigitIndex:byte;Increment:integer);
    procedure SetAutoSize(newValue: boolean);
    procedure SetBtnHeight(newValue: integer);
    procedure SetColor(newValue: TColor);
    procedure SetDecSeparator(newValue: char);
    procedure SetDecSeparatorPos(newValue: byte);
    procedure SetDigitValue(DigitIndex:byte;Value:byte);
    procedure SetEditPos(newValue: byte);
    procedure SetFrameColor(newValue: TColor);
    procedure SetMaxValue(newValue: extended);
    procedure SetMinValue(newValue: extended);
    procedure SetNumDigits(newValue: TNumDigits);
    procedure SetValue(newValue: extended);
    procedure SpinButtonDown(ButtonIndex:integer;SpinUp:boolean);
    procedure SpinButtonUp(ButtonIndex:integer;SpinUp:boolean);

    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  protected
    procedure Change; dynamic;
    procedure DoSpinBtnClick(BtnIndex:integer;UpBtn:boolean);
    procedure DrawDigit(ACanvas:TCanvas;ARect:TRect;AValue:byte;Editing:boolean);
    function GetDigitValue(DigitIndex:byte):byte;
    function GetSpinButtonIndex(X,Y:integer;var UpperRow:boolean):integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    property BtnHeight: integer read FBtnHeight write SetBtnHeight;
    property Color: TColor read FColor write SetColor;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    property Ctl3D;
    property DecSeparator: char read FDecSeparator write SetDecSeparator;
    property DecSeparatorPos: byte read FDecSeparatorPos write SetDecSeparatorPos;
    property DragCursor;
    {$IFDEF SR_Delphi5_Up}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property EditPos: byte read FEditPos write SetEditPos;
    property Enabled;
    property Font;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property MaxValue: extended read FMaxValue write SetMaxValue;
    property MinValue: extended read FMinValue write SetMinValue;
    property NumDigits: TNumDigits read FNumDigits write SetNumDigits;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Value: extended read FValue write SetValue;
    property Visible;

    {$IFDEF SR_Delphi5_Up}
    property OnCanResize;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF SR_Delphi5_Up}
    property OnConstrainedResize;
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnResize;
    {$ENDIF}
    property OnSpinBtnClick: TNESpinBtnClick read FOnSpinBtnClick write FOnSpinBtnClick;
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;


  TSliderEdit = class(TCustomControl)
  private
    FAutoHide         : boolean;
    FBevelStyle       : TPanelBevel;
    FBevelWidth,
    FBorderWidth      : integer;
    FColors           : TColors;
    FDisplayHeight,
    FDisplayWidth,
    FDownIndex        : integer;
    FDrawBuffer       : TBitmap;
    FFocusRect,
    FHasFocus         : boolean;
    FLargeChange,
    FMaxValue,
    FMediumChange,
    FMinValue         : integer;
    FMouseDown        : boolean;
    FRulerDirection   : TRulerDirection;
    FSliderWidth,
    FSmallChange,
    FSpinBtnHeight    : integer;
    FTickStyle        : TTickStyle;
    FValue            : integer;
    FValueEdit        : TSRValueEdit;
    FOnChange         : TNotifyEvent;

    procedure SetBevelStyle(newValue: TPanelBevel);
    procedure SetBevelWidth(newValue: integer);
    procedure SetBorderWidth(newValue: integer);
    procedure SetColors(newValue: TColors);
    procedure SetDisplayWidth(newValue: integer);
    procedure SetFocusRect(newValue: boolean);
    procedure SetMaxValue(newValue: integer);
    procedure SetMinValue(newValue: integer);
    procedure SetRulerDirection(newValue: TRulerDirection);
    procedure SetSliderWidth(newValue: integer);
    procedure SetSpinBtnHeight(newValue: integer);
    procedure SetTickStyle(newValue: TTickStyle);
    procedure SetValue(newValue: integer);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  protected
    procedure CalcDisplayHeight;
    procedure Change; dynamic;
    procedure DoDrawFocusRect(ACanvas:TCanvas);
    procedure DoSpinBtnClick(const UpBtn:boolean;Shift:TShiftState);
    procedure DrawValueDisplay(ACanvas:TCanvas;ARect:TRect);
    procedure DrawResetButtons(ACanvas:TCanvas;ARect:TRect);
    procedure DrawRuler(ACanvas:TCanvas;ARect:TRect;DrawFocus:boolean);
    function GetRulerLength:integer;
    function GetSliderOffset(const AValue:integer):integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ValueEdit: TSRValueEdit read FValueEdit write FValueEdit;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BevelWidth: integer read FBevelWidth write SetBevelWidth;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property Colors: TColors read FColors write SetColors;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    property Ctl3D;
    property DisplayWidth: integer read FDisplayWidth write SetDisplayWidth;
    property DragCursor;
    {$IFDEF SR_Delphi5_Up}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property FocusRect: boolean read FFocusRect write SetFocusRect;
    property Font;
    property LargeChange: integer read FLargeChange write FLargeChange;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property MediumChange: integer read FMediumChange write FMediumChange;
    property MinValue: integer read FMinValue write SetMinValue;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property SmallChange: integer read FSmallChange write FSmallChange;
    property RulerDirection: TRulerDirection read FRulerDirection write SetRulerDirection;
    property SliderWidth: integer read FSliderWidth write SetSliderWidth;
    property SpinBtnHeight: integer read FSpinBtnHeight write SetSpinBtnHeight;
    property TabOrder;
    property TabStop;
    property TickStyle : TTickStyle read FTickStyle write SetTickStyle;
    property Value: integer read FValue write SetValue;
    property Visible;

    {$IFDEF SR_Delphi5_Up}
    property OnCanResize;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF SR_Delphi5_Up}
    property OnConstrainedResize;
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnResize;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}

  end;


  TSRValueEdit = class(TCustomControl)
  private
    FBevelStyle       : TPanelBevel;
    FBevelWidth,
    FBorderWidth      : integer;
    FCaptureMouse     : boolean;
    FColors           : TColors;
    FDecSeparator     : char;
    FDecSeparatorPos  : byte;
    FFocusRect        : boolean;
    FHistoryCount     : byte;
    FHistoryMenu      : TPopupMenu;
    FHistoryValue     : array [0..3] of extended;
    FNumDigits        : TNumDigits;
    FNumericEdit      : TNumericEdit;
    FMaxValue,
    FMinValue         : extended;
    FNumEditOffset,
    FOldOffset        : integer;
    FOldvalue         : extended;
    FSliderWidth      : integer;
    FScrewPos         : array [0..4] of byte;
    FScrewSize        : TScrewSize;
    FShowScrews       : boolean;
    FSliderEdit       : TSliderEdit;
    FSliderEditLength,
    FSliderEditWidth  : integer;
    FRulerDirection   : TRulerDirection;
    FSpinBtnHeight,
    FSpinControlWidth : integer;
    FTickStyle        : TTickStyle;
    FValue            : extended;
    FValueHistory     : boolean;
    FOnChange,
    FOnHideSlider,
    FOnHistoryClick,
    FOnShowSlider     : TNotifyEvent;
    FOnSpinBtnClick   : TNESpinBtnClick;

    procedure DestroySliderEdit;
    procedure SetBevelStyle(newValue: TPanelBevel);
    procedure SetBevelWidth(newValue: integer);
    procedure SetBorderWidth(newValue: integer);
    procedure SetColors(newValue: TColors);
    procedure SetDecSeparator(newValue: char);
    procedure SetDecSeparatorPos(newValue: byte);
    procedure SetFocusRect(newValue: boolean);
    procedure SetMaxValue(newValue: extended);
    procedure SetMinValue(newValue: extended);
    procedure SetNumDigits(newValue: TNumDigits);
    procedure SetControlsPosAndWidth;
    procedure SetRulerDirection(newValue: TRulerDirection);
    procedure SetSliderEditWidth(newValue: integer);
    procedure SetSpinBtnHeight(newValue: integer);
    procedure SetSpinControlWidth(newValue: integer);
    procedure SetScrewSize(newValue: TScrewSize);
    procedure SetShowScrews(newValue: boolean);
    procedure SetTickStyle(newValue: TTickStyle);
    procedure SetValue(newValue: extended);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  protected
    procedure AddHistoryItem(ValueNr:byte);
    procedure AddValueToHistory(AValue:extended);
    procedure CalcBorderWidth;
    procedure Change; dynamic;
    procedure DoDrawFocusRect(IsFocused:boolean); dynamic;
    procedure DoSpinBtnClick(BtnIndex:integer;UpBtn:boolean);
    procedure DrawRadioControl(IsFocused:boolean);
    procedure DrawScrew(X,Y,Nr:integer);
    function GetSpinControlRect(CRect:TRect;IsClientRect:boolean):TRect;
    procedure HistoryItemClick(Sender: TObject);
    function IsInsideSpinControl(X,Y: Integer):boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ShowSliderEdit(AutoHide: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BevelWidth: integer read FBevelWidth write SetBevelWidth;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property CaptureMouse: boolean read FCaptureMouse write FCaptureMouse;
    property Colors: TColors read FColors write SetColors;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    property Ctl3D;
    property DecSeparator: char read FDecSeparator write SetDecSeparator;
    property DecSeparatorPos: byte read FDecSeparatorPos write SetDecSeparatorPos;
    property DragCursor;
    {$IFDEF SR_Delphi5_Up}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property FocusRect: boolean read FFocusRect write SetFocusRect;
    property Font;
    property MaxValue: extended read FMaxValue write SetMaxValue;
    property MinValue: extended read FMinValue write SetMinValue;
    property NumDigits: TNumDigits read FNumDigits write SetNumDigits;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ScrewSize: TScrewSize read FScrewSize write SetScrewSize;
    property ShowScrews: boolean read FShowScrews write SetShowScrews;
    property RulerDirection: TRulerDirection read FRulerDirection write SetRulerDirection;
    property SliderEditLength: integer read FSliderEditLength write FSliderEditLength;
    property SliderEditWidth: integer read FSliderEditWidth write SetSliderEditWidth;
    property SliderWidth: integer read FSliderWidth write FSliderWidth;
    property SpinBtnHeight: integer read FSpinBtnHeight write SetSpinBtnHeight;
    property SpinControlWidth: integer read FSpinControlWidth write SetSpinControlWidth;
    property TabOrder;
    property TabStop;
    property TickStyle : TTickStyle read FTickStyle write SetTickStyle;
    property Value: extended read FValue write SetValue;
    property ValueHistory: boolean read FValueHistory write FValueHistory;
    property Visible;

    {$IFDEF SR_Delphi5_Up}
    property OnCanResize;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF SR_Delphi5_Up}
    property OnConstrainedResize;
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHideSlider: TNotifyEvent read FOnHideSlider write FOnHideSlider;
    property OnHistoryClick: TNotifyEvent read FOnHistoryClick write FOnHistoryClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnResize;
    {$ENDIF}
    property OnShowSlider: TNotifyEvent read FOnShowSlider write FOnShowSlider;
    property OnSpinBtnClick: TNESpinBtnClick read FOnSpinBtnClick write FOnSpinBtnClick;
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
uses SRUtils, rrColors;
{$ELSE}
{$R *.D16}
uses SRUtils;
{$ENDIF}

const
  DefaultRulerWidth   = 36;
  DefaultRulerLength  = 140;
  MaxTriangleHeight   = 6;
  MaxTriangleWidth    = 10;
  MaxHistoryItems     = 4;
  DefaultThumbWidth   = 14;
  FHLContrast         = 5;
  FShContrast         = 4;
  ResetBtnHeight      = 12;
  ResetBtnWidth       = 15;

function XKoord(XMittel,XRadius,Grad:integer):integer;
begin
  Result:=round(XMittel-(sin(Grad*Pi/180)*XRadius));
end; {XKoord}

function YKoord(YMittel,YRadius,Grad:integer):integer;
begin
  Result:=round(YMittel-(cos(Grad*Pi/180)*YRadius));
end; {YKoord}

function Power10(Exponent:integer):integer;
var i : integer;
begin
  Result:=1;
  for i:=1 to Exponent do
    Result:=Result*10;
end; {Power10}

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

procedure DrawBevel(ACanvas:TCanvas;ARect:TRect;Raised,Edge:boolean);
begin
  with ACanvas do begin
    if Edge then begin
      Pen.Color:=clBtnFace;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-1);
      Pen.Color:=clWindowFrame;
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Top);
      InflateRect(ARect, -1, -1);
    end;
    if Raised then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Right-1, ARect.Top);
    LineTo(ARect.Left, ARect.Top);
    LineTo(ARect.Left, ARect.Bottom-1);
    if Raised then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    LineTo(ARect.Right-1, ARect.Bottom-1);
    LineTo(ARect.Right-1, ARect.Top);
  end;
end; {DrawBevel}

procedure DrawColorBevel(ACanvas:TCanvas;ARect:TRect;Raised,Edge:boolean;ColorHL,ColorSh:TColor);
begin
  with ACanvas do begin
    if Edge then begin
      Pen.Color:=clBtnFace;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-1);
      Pen.Color:=clWindowFrame;
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Top);
      InflateRect(ARect, -1, -1);
    end;
    if Raised then
      Pen.Color:=ColorHL
    else
      Pen.Color:=ColorSh;
    MoveTo(ARect.Right-1, ARect.Top);
    LineTo(ARect.Left, ARect.Top);
    LineTo(ARect.Left, ARect.Bottom-1);
    if Raised then
      Pen.Color:=ColorSh
    else
      Pen.Color:=ColorHL;
    LineTo(ARect.Right-1, ARect.Bottom-1);
    LineTo(ARect.Right-1, ARect.Top);
  end;
end; {DrawBevel}

procedure DrawDotLine(ACanvas:TCanvas;X1,Y1,Length:integer;Horizontal:boolean);
var i : integer;
begin
  with ACanvas do begin
    if Horizontal then begin
      for i:=X1 to X1+Length-1 do
        if odd(i) then
          Pixels[i, Y1]:=Brush.Color;
    end
    else begin
      for i:=Y1 to Y1+Length-1 do
        if odd(i) then
          Pixels[X1, i]:=Brush.Color;
    end;
  end;
end;

procedure DrawLine(ACanvas:TCanvas;X1,Y1,X2,Y2:integer);
begin
  with ACanvas do begin
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end; {DrawLine}

procedure DrawTextComp(ACanvas:TCanvas;AText:string;var ARect:TRect;AFormat:Word);
{$IFDEF SR_Delphi1}
var CText : PChar;
{$ENDIF}
begin
  {$IFDEF SR_Delphi1}
  CText:=StrAlloc(255);
  StrPCopy(CText, AText);
  DrawText(ACanvas.Handle, CText, StrLen(CText), ARect, AFormat);
  StrDispose(CText);
  {$ELSE}
  DrawText(ACanvas.Handle, PChar(AText), Length(AText), ARect, AFormat);
  {$ENDIF}
end;

procedure DrawTriangle(ACanvas:TCanvas;X,Y,Width,Height:integer;
                       AColor:TColor;ADestination:TTriangleDest);
var AWidth,
    AHeight,
    HDiff,
    VDiff   : integer;
begin
  with ACanvas do begin
    Brush.Color:=AColor;
    Pen.Color:=AColor;
    if (Height-7)>MaxTriangleHeight then
      AHeight:=MaxTriangleHeight
    else
      AHeight:=Height-7;
    AWidth:=AHeight*2;
    HDiff:=AWidth div 2;
    VDiff:=AHeight div 2;
    X:=X+(Width div 2)-HDiff;
    Y:=Y+(Height div 2)-VDiff-1;
    if ADestination=tdTop then
      Polygon([Point(X, Y+AHeight), Point(X+HDiff, Y), Point(X+AWidth, Y+AHeight)]);
    if ADestination=tdRight then
      Polygon([Point(X, Y), Point(X+AWidth, Y+VDiff), Point(X, Y+AHeight)]);
    if ADestination=tdBottom then
      Polygon([Point(X, Y), Point(X+HDiff, Y+AHeight), Point(X+AWidth, Y)]);
    if ADestination=tdLeft then
      Polygon([Point(X+AWidth, Y+AHeight), Point(X, Y+VDiff), Point(X+AWidth, Y)]);
  end;
end; {DrawTriangle}

{ TColors }

constructor TColors.Create(AOwner: TComponent);
begin
  inherited Create;

  if AOwner is TCustomControl then
    FOwner := TCustomControl(AOwner)
  else
    FOwner:=nil;
  FBackground:=clBtnFace;
  FNumBackground:=clBlack;
  FNumFrame:=clWindowFrame;
  FRuler:=clBtnFace;
  FSliderBorder:=clSilver;
  FSliderTop:=clBtnFace;
  FSliderMark:=clBlue;
  FTickMarks:=clBtnText;

  AssignBevelColors(FSliderBorder, FBorderHL, FBorderSh, FHLContrast, FShContrast);
end;

procedure TColors.Assign(Source: TPersistent);
begin
  Background:=TColors(Source).Background;
  NumBackground:=TColors(Source).NumBackground;
  NumFrame:=TColors(Source).NumFrame;
  Ruler:=TColors(Source).Ruler;
  SliderBorder:=TColors(Source).SliderBorder;
  SliderTop:=TColors(Source).SliderTop;
  SliderMark:=TColors(Source).SliderMark;
  TickMarks:=TColors(Source).TickMarks;
end;

procedure TColors.AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  {$IFDEF SR_Delphi1}
  HighlightColor:=ChangeBrightness(FaceColor, 100 div 10*HLContrast);
  ShadowColor:=ChangeBrightness(FaceColor, -100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FaceColor, HighlightColor, ShadowColor,
              (10-HLContrast)/10, (10-ShContrast)/10);
  {$ENDIF}
end; {AssignBevelColors}

procedure TColors.SetBackground(NewValue: TColor);
begin
  if NewValue<>FBackground then begin
    FBackground:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TColors.SetNumBackground(NewValue: TColor);
begin
  if NewValue<>FNumBackground then begin
    FNumBackground:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
    if Assigned(FOwner) and (FOwner is TSRValueEdit) and assigned(TSRValueEdit(FOwner).FNumericEdit) then
      TSRValueEdit(FOwner).FNumericEdit.SetColor(NewValue);
  end;
end;

procedure TColors.SetNumFrame(NewValue: TColor);
begin
  if NewValue<>FNumFrame then begin
    FNumFrame:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
    if Assigned(FOwner) and (FOwner is TSRValueEdit) and assigned(TSRValueEdit(FOwner).FNumericEdit) then
      TSRValueEdit(FOwner).FNumericEdit.SetFrameColor(NewValue);
  end;
end;

procedure TColors.SetRuler(NewValue: TColor);
begin
  if NewValue<>FRuler then begin
    FRuler:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TColors.SetSliderBorder(NewValue: TColor);
begin
  if NewValue<>FSliderBorder then begin
    FSliderBorder:=NewValue;
    AssignBevelColors(FSliderBorder, FBorderHL, FBorderSh, FHLContrast, FShContrast);
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TColors.SetSliderMark(NewValue: TColor);
begin
  if NewValue<>FSliderMark then begin
    FSliderMark:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TColors.SetSliderTop(NewValue: TColor);
begin
  if NewValue<>FSliderTop then begin
    FSliderTop:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

procedure TColors.SetTickMarks(NewValue: TColor);
begin
  if NewValue<>FTickMarks then begin
    FTickMarks:=NewValue;
    if Assigned(FOwner) then
      FOwner.Invalidate;
  end;
end;

{ TNumericEdit }

constructor TNumericEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Height:=41;
  Width:=60;

  FAutoSize:=true;
  FBtnHeight:=10;
  FDecSeparator:='.';
  FDecSeparatorPos:=0;
  FDownIdxTop:=-1;
  FDownIdxBtm:=-1;
  FEditPos:=0;
  FNumDigits:=4;
  FMaxValue:=9999;
  FMinValue:=-9999;

  FDrawBuffer:=TBitmap.Create;

  Font.Color:=clLime;
  Font.Style:=[fsBold];
end;

destructor TNumericEdit.Destroy;
begin
  if assigned(FDrawBuffer) then
    FreeAndNil(FDrawBuffer);

  inherited Destroy;
end;

procedure TNumericEdit.CalcControlHeight;
var DC: HDC;
begin
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font.Assign(Font);
  Height:=Canvas.TextHeight('0,2')+(2*FBtnHeight)+4;
  Canvas.Handle:=0;
  ReleaseDC(0, DC);
end;

procedure TNumericEdit.CalcDigitWidth;
begin
  FDigitWidth:=(Width-2) div (FNumDigits+1);
end;

procedure TNumericEdit.Change;
begin
  if assigned(FValueEdit) then
    FValueEdit.SetValue(FValue);
  Invalidate;
  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNumericEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  invalidate;
end;

procedure TNumericEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FAutoSize then
    CalcControlHeight;
end;

procedure TNumericEdit.DoSpinBtnClick(BtnIndex:integer;UpBtn:boolean);
begin
  if assigned(FOnSpinBtnClick) then
    FOnSpinBtnClick(Self, BtnIndex, UpBtn);
end;

procedure TNumericEdit.DrawDigit(ACanvas:TCanvas;ARect:TRect;AValue:byte;Editing:boolean);
var AText : string;
begin
  AText:=IntToStr(AValue);
  InflateRect(ARect, -1, -1);
  with ACanvas do begin
    Font.Assign(Self.Font);
{    if FHasFocus and Editing and
     assigned(FValueEdit) and not assigned(FValueEdit.FSliderEdit) then begin}
    if FHasFocus and Editing then begin
      Brush.Color:=clHighlight;
      Font.Color:=clHighlightText;
      FillRect(ARect);
    end;
    Brush.Style:=bsClear;
    DrawTextComp(ACanvas, AText, ARect, DT_SingleLine or DT_VCenter or DT_Center);
    Brush.Style:=bsSolid;
  end;
end; {DrawDigit}

function TNumericEdit.GetDigitValue(DigitIndex:byte):byte;
var Temp1,
    Temp2 : integer;
begin
  Temp1:=trunc(FValue*Power10(FDecSeparatorPos));
  Temp2:=Temp1 div Power10(FNumDigits-DigitIndex-1);
  Result:=abs(Temp2 mod 10);
end;

function TNumericEdit.GetSpinButtonIndex(X,Y:integer;var UpperRow:boolean):integer;
var NumRect : TRect;
    OnBtn   : boolean;
begin
  OnBtn:=false;
  NumRect:=ClientRect;
  if Y<(NumRect.Top+FBtnHeight) then begin
    UpperRow:=true;
    OnBtn:=true;
  end;
  if Y>(NumRect.Bottom-FBtnHeight) then begin
    UpperRow:=false;
    OnBtn:=true;
  end;
  if OnBtn then
    Result:=(X div FDigitWidth)
  else
    Result:=-1;
end;

function TNumericEdit.GetMaxValue:extended;
var FracPart  : extended;
    TruncPart : integer;
begin
  if FDecSeparatorPos=0 then
    FracPart:=0
  else
    FracPart:=1/Power10(FDecSeparatorPos);
  TruncPart:=Power10(FNumDigits);
  Result:=TruncPart-FracPart;
end;

procedure TNumericEdit.IncreaseDigitValue(DigitIndex:byte;Increment:integer);
var OldValue : byte;
    NewValue : integer;
begin
  OldValue:=GetDigitValue(DigitIndex);
  NewValue:=OldValue+Increment;
  if (NewValue>=0) and (NewValue<=9) then
    SetDigitValue(DigitIndex, NewValue);
end;

procedure TNumericEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (FEditPos>0) and (Key=VK_Left) then
    SetEditPos(FEditPos-1);
  if (FEditPos<(FNumDigits-1)) and (Key=VK_Right) then
    SetEditPos(FEditPos+1);
  if Key=VK_Up then
    SpinButtonDown(FEditPos+1, true);
  if Key=VK_Down then
    SpinButtonDown(FEditPos+1, false);
  if Key=VK_Back then begin
    if FEditPos>0 then
      SetEditPos(EditPos-1);
    SetDigitValue(EditPos, 0);
  end;
  if Key=VK_Delete then
    SetDigitValue(FEditPos, 0);
  if Key=VK_Home then
    SetEditPos(0);
  if Key=VK_End then
    SetEditPos(FNumDigits-1);

  inherited KeyDown(Key, Shift);
end;

procedure TNumericEdit.KeyPress(var Key: Char);
var AValue : byte;
begin
  if (Key='-') and (-abs(FValue)>=FMinValue) then
    SetValue(-abs(FValue));
  if (Key='+') and (abs(FValue)<=FMaxValue)then
    SetValue(abs(FValue));
  if Key in ['0'..'9'] then begin
    AValue:=ord(Key)-ord('0');
    SetDigitValue(FEditPos, AValue);
    if EditPos<(NumDigits-1) then
      SetEditPos(FEditPos+1);
  end;

  inherited KeyPress(Key);
end;

procedure TNumericEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_Up) then
    SpinButtonUp(FEditPos, true);
  if (Key=VK_Down) then
    SpinButtonUp(FEditPos, false);

  inherited KeyUp(Key, Shift);
end;

procedure TNumericEdit.Loaded;
var AValue : extended;
begin
  inherited Loaded;

  if FMaxValue=FMinValue then begin
    if (FMaxValue=0) and (FMinValue=0) then begin
      AValue:=GetMaxValue;
      SetMaxValue(AValue);
      SetMinValue(-AValue);
    end
    else
      SetMaxValue(FMinValue+1);
  end;
  if FAutoSize then
    CalcControlHeight;
end;

procedure TNumericEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var BtnIdx : integer;
    SpinUp : boolean;
begin
  if assigned(FValueEdit) and FValueEdit.Enabled then
    FValueEdit.SetFocus
  else
    SetFocus;
  if (Button=mbLeft) then begin
    BtnIdx:=(X div FDigitWidth);
    if (BtnIdx>0) and (BtnIdx<=FNumDigits) then
      SetEditPos(BtnIdx-1);
    if (Y<=FBtnHeight) or (Y>=(Height-FBtnHeight)) then begin
      { Maus ist im SpinButton-Bereich }
      BtnIdx:=GetSpinButtonIndex(X, Y, SpinUp);
      if BtnIdx>=0 then
        SpinButtonDown(BtnIdx, SpinUp);
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TNumericEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) then begin
    if FDownIdxTop>=0 then
      SpinButtonUp(FDownIdxTop, true);
    if FDownIdxBtm>=0 then
      SpinButtonUp(FDownIdxBtm, false);
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TNumericEdit.Paint;
var BRect,
    CRect  : TRect;
    AText  : string;
    i,AVal : integer;
begin
  CRect:=ClientRect;
  with FDrawBuffer.Canvas do begin
    { Rahmen }
    if Ctl3D then
      DrawBevel(FDrawBuffer.Canvas, CRect, false, false)
    else begin
      Brush.Color:=clBlack;
      FrameRect(CRect);
    end;
    { SpinButtons }
    Brush.Color:=clBtnFace;
    BRect:=Rect(CRect.Left+1, CRect.Top+1,
                CRect.Right-1, 1+FBtnHeight);
    FillRect(BRect);
    BRect:=Rect(CRect.Left+1, CRect.Bottom-FBtnHeight-1,
                CRect.Right-1, CRect.Bottom-1);
    FillRect(BRect);
    for i:=0 to FNumDigits do begin
      BRect.Left:=(i*FDigitWidth)+1;
      BRect.Top:=1;
      BRect.Right:=BRect.Left+FDigitWidth;
      BRect.Bottom:=BRect.Top+FBtnHeight;
      DrawBevel(FDrawBuffer.Canvas, BRect, (FDownIdxTop<>i) or (i<0), true);
      DrawTriangle(FDrawBuffer.Canvas, BRect.Left, BRect.Top,
                   BRect.Right-BRect.Left, BRect.Bottom-BRect.Top,
                   clBtnText, tdTop);
      BRect.Bottom:=CRect.Bottom-1;
      BRect.Top:=BRect.Bottom-FBtnHeight;
      DrawBevel(FDrawBuffer.Canvas, BRect, (FDownIdxBtm<>i) or (i<0), true);
      DrawTriangle(FDrawBuffer.Canvas, BRect.Left, BRect.Top,
                   BRect.Right-BRect.Left, BRect.Bottom-BRect.Top,
                   clBtnText, tdBottom);
    end;
    if BRect.Right<(CRect.Right-3) then begin
      BRect.Left:=((FNumDigits+1)*FDigitWidth)+1;
      BRect.Top:=1;
      BRect.Right:=CRect.Right-1;
      BRect.Bottom:=BRect.Top+FBtnHeight;
      DrawBevel(FDrawBuffer.Canvas, BRect, true, true);
      BRect.Bottom:=CRect.Bottom-1;
      BRect.Top:=BRect.Bottom-FBtnHeight;
      DrawBevel(FDrawBuffer.Canvas, BRect, true, true);
    end;
    { Hintergrund }
    Brush.Color:=FColor;
    Pen.Color:=FFrameColor;
    BRect.Left:=+1;
    BRect.Top:=FBtnHeight;
    BRect.Right:=CRect.Right-1;
    BRect.Bottom:=CRect.Bottom-FBtnHeight-1;
    Rectangle(BRect.Left, BRect.Top, BRect.Right, BRect.Bottom);
    { Digits }
    for i:=0 to FNumDigits do begin
      BRect.Left:=(i*FDigitWidth)+1;
      BRect.Top:=FBtnHeight;
      BRect.Right:=BRect.Left+FDigitWidth-1;
      BRect.Bottom:=CRect.Bottom-FBtnHeight-1;
      if i=0 then begin
        Font.Assign(Self.Font);
        if FValue<0 then
          DrawText(Handle, '-', 1, BRect,
                   DT_SingleLine or DT_VCenter or DT_Center);
        if FValue>0 then
          DrawText(Handle, '+', 1, BRect,
                   DT_SingleLine or DT_VCenter or DT_Center);
      end
      else begin
        AVal:=GetDigitValue(i-1);
        DrawDigit(FDrawBuffer.Canvas, BRect, AVal, EditPos=(i-1));
      end;
    end;
    { Separator }
    if FDecSeparatorPos>0 then begin
      AText:=FDecSeparator;
      Font.Assign(Self.Font);
      Brush.Style:=bsClear;
      TextOut(((FNumDigits-FDecSeparatorPos+1)*FDigitWidth)-(TextWidth(AText) div 2)+1,
              (CRect.Bottom-CRect.Top-TextHeight('0')) div 2, AText);
      Brush.Style:=bsSolid;
    end;

  end;
  Canvas.Draw(0, 0, FDrawBuffer);
end;

procedure TNumericEdit.SetAutoSize(newValue: boolean);
begin
  if newValue<>FAutoSize then begin
    FAutoSize:=newValue;
    if FAutoSize then begin
      CalcControlHeight;
      Invalidate;
    end;
  end;
end;

procedure TNumericEdit.SetBtnHeight(newValue: integer);
begin
  if newValue<>FBtnHeight then begin
    FBtnHeight:=newValue;
    if FAutoSize then
      CalcControlHeight;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetColor(newValue: TColor);
begin
  if newValue<>FColor then begin
    FColor:=newValue;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetDecSeparator(newValue: char);
begin
  if NewValue<>FDecSeparator then begin
    FDecSeparator:=NewValue;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetDecSeparatorPos(newValue: byte);
var NumMax : extended;
begin
  if NewValue<>FDecSeparatorPos then begin
    FDecSeparatorPos:=NewValue;
    NumMax:=GetMaxvalue;
    if FMaxValue>NumMax then
      FMaxValue:=NumMax;
    if abs(FMinValue)>NumMax then
      FMinValue:=-NumMax;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetDigitValue(DigitIndex:byte;Value:byte);
var NewVal     : integer;
    ResultVal  : extended;
    DigitVal,i : byte;
begin
  NewVal:=Value*Power10(FNumDigits-DigitIndex-1);
  for i:=0 to FNumDigits-1 do begin
    if i<>DigitIndex then begin
      DigitVal:=GetDigitValue(i);
      NewVal:=NewVal+DigitVal*Power10(FNumDigits-i-1);
    end;
  end;
  ResultVal:=NewVal/Power10(FDecSeparatorPos);
  if FValue<0 then
    SetValue(-ResultVal)
  else
    SetValue(ResultVal);
end;

procedure TNumericEdit.SetEditPos(newValue: byte);
begin
  if newValue<>FEditPos then begin
    FEditPos:=newValue;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetFrameColor(newValue: TColor);
begin
  if newValue<>FFrameColor then begin
    FFrameColor:=newValue;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetMaxValue(newValue: extended);
var NumMax : extended;
begin
  if NewValue<>FMaxValue then begin
    NumMax:=GetMaxValue;
    if NewValue>NumMax then
      NewValue:=NumMax;
    FMaxValue:=NewValue;
    if FValue>FMaxValue then
      SetValue(FMaxValue);
  end;
end;

procedure TNumericEdit.SetMinValue(newValue: extended);
var NumMax : extended;
begin
  if NewValue<>FMinValue then begin
    NumMax:=-GetMaxValue;
    if NewValue<NumMax then
      NewValue:=NumMax;
    FMinValue:=NewValue;
    if FValue<FMinValue then
      SetValue(FMinValue);
  end;
end;

procedure TNumericEdit.SetNumDigits(newValue: TNumDigits);
var NumMax : extended;
begin
  if newValue<>FNumDigits then begin
    FNumDigits:=newValue;
    CalcDigitWidth;
    NumMax:=GetMaxvalue;
    if FMaxValue>NumMax then
      FMaxValue:=NumMax;
    if abs(FMinValue)>NumMax then
      FMinValue:=-NumMax;
    Invalidate;
  end;
end;

procedure TNumericEdit.SetValue(newValue: extended);
begin
  if newValue>FMaxValue then
    newValue:=FMaxValue;
  if newValue<FMinValue then
    newValue:=FMinValue;
  if newValue<>FValue then begin
    FValue:=newValue;
    Change;
  end;
end;

procedure TNumericEdit.SpinButtonDown(ButtonIndex:integer;SpinUp:boolean);
begin
  if SpinUp then begin
    FDownIdxTop:=ButtonIndex;
    if ButtonIndex>0 then
      IncreaseDigitValue(ButtonIndex-1, 1)
    else
      SetValue(-FValue);
  end
  else begin
    FDownIdxBtm:=ButtonIndex;
    if ButtonIndex>0 then
      IncreaseDigitValue(ButtonIndex-1, -1)
    else
      SetValue(-FValue);
  end;
  Invalidate;
  if assigned(FOnSpinBtnClick) then
    DoSpinBtnClick(ButtonIndex, SpinUp);
end;

procedure TNumericEdit.SpinButtonUp(ButtonIndex:integer;SpinUp:boolean);
begin
  FDownIdxTop:=-1;
  FDownIdxBtm:=-1;
  Invalidate;
end;

procedure TNumericEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TNumericEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  { Kein Fokuswechsel durch Cursortasten }
  Message.Result:=Message.Result or DLGC_WANTARROWS;
end;

procedure TNumericEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  FHasFocus:=false;
  Invalidate;
end;

procedure TNumericEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  FHasFocus:=true;
  Invalidate;
end;

procedure TNumericEdit.WMSize(var Message: TWMSize);
begin
  inherited;

  FDrawBuffer.Width:=Width;
  FDrawBuffer.Height:=Height;
  CalcDigitWidth;
  Message.Result := 0;
end;


{ TSliderEdit }

constructor TSliderEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FColors := TColors.Create(Self);
  FDrawBuffer:=TBitmap.Create;

  Width:=DefaultRulerWidth;
  Height:=DefaultRulerLength;

  FAutoHide:=false;
  FBevelStyle:=bvRaised;
  FBevelWidth:=1;
  FBorderWidth:=2;
  FDisplayWidth:=30;
  FDownIndex:=-1;
  FLargeChange:=100;
  FMaxValue:=127;
  FMinValue:=-128;
  FMediumChange:=10;
  FSliderWidth:=DefaultThumbWidth;
  FSmallChange:=1;
  FRulerDirection:=rdVertical;
  FSpinBtnHeight:=10;
  FTickStyle:=tsAll;
  FValueEdit:=nil;

  Font.Color:=clLime;
  Font.Style:=[fsBold];
  CalcDisplayHeight;
end;

destructor TSliderEdit.Destroy;
begin
  if assigned(FColors) then
    FreeAndNil(FColors);
  if assigned(FDrawBuffer) then
    FreeAndNil(FDrawBuffer);

  inherited Destroy;
end;

procedure TSliderEdit.CalcDisplayHeight;
var DC: HDC;
begin
  if not (csLoading in ComponentState) and not (csReading in ComponentState) then begin
    DC:=GetDC(0);
    Canvas.Handle:=DC;
    Canvas.Font.Assign(Font);
    FDisplayHeight:=Canvas.TextHeight('0')+2;
    Canvas.Handle:=0;
    ReleaseDC(0, DC);
  end;
end;

procedure TSliderEdit.Change;
begin
  if assigned(FValueEdit) then
    FValueEdit.SetValue(FValue);
  Invalidate;
  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSliderEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  CalcDisplayHeight;
end;

procedure TSliderEdit.DoDrawFocusRect(ACanvas:TCanvas);
var CRect : TRect;
begin
  CRect:=ClientRect;
  InflateRect(CRect, -FBevelWidth-FBorderWidth, -FBevelWidth-FBorderWidth);
  if FRulerDirection=rdHorizontal then begin
    CRect.Left:=CRect.Left+(FDisplayWidth div 2)+2;
    CRect.Right:=CRect.Right-FDisplayWidth-2;
  end
  else begin
    CRect.Top:=CRect.Top+FDisplayHeight+FSpinBtnHeight+4;
    CRect.Bottom:=CRect.Bottom-FSpinBtnHeight-3;
  end;
  InflateRect(CRect, -2, -2);
  ACanvas.Brush.Color:=clBlack;
  ACanvas.Pen.Color:=clBlack;
  DrawDotLine(ACanvas, CRect.Left+1, CRect.Top, CRect.Right-CRect.Left-2, true);
  DrawDotLine(ACanvas, CRect.Right-1, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
  DrawDotLine(ACanvas, CRect.Left+1, CRect.Bottom-1, CRect.Right-CRect.Left-2, true);
  DrawDotLine(ACanvas, CRect.Left, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
end;

procedure TSliderEdit.DoSpinBtnClick(const UpBtn:boolean;Shift:TShiftState);
var AValue : integer;
begin
  if ssShift in Shift then
    AValue:=FMediumChange
  else begin
    if ssCtrl in Shift then
      AValue:=FLargeChange
    else
      AValue:=FSmallChange;
  end;
  if UpBtn then begin
    FDownIndex:=1;
    SetValue(FValue-AValue);
  end
  else begin
    FDownIndex:=0;
    SetValue(FValue+AValue);
  end;
end;

procedure TSliderEdit.DrawValueDisplay(ACanvas:TCanvas;ARect:TRect);
var SRect  : TRect;
    SWidth : integer;
    AText  : string;
begin
  with ACanvas do begin
    { Wertanzeige zeichnen }
    SRect:=Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top+FDisplayHeight);
    Brush.Color:=FColors.NumBackground;
    FillRect(SRect);
    AText:=IntToStr(FValue);
    Font.Assign(Self.Font);
    DrawTextComp(ACanvas, AText, SRect,
                 DT_SingleLine or DT_Center or DT_VCenter);
    DrawBevel(ACanvas, SRect, false, false);

    { SpinButtons zeichnen }
    SRect.Top:=ARect.Bottom-FSpinBtnHeight;
    SRect.Bottom:=ARect.Bottom;
    SWidth:=(ARect.Right-ARect.Left) div 2;
    SRect.Right:=SRect.Left+SWidth;
    Brush.Color:=clBtnFace;
    FillRect(SRect);
    DrawBevel(ACanvas, SRect, FDownIndex<>0, true);
    DrawTriangle(ACanvas, SRect.Left, SRect.Top,
                 SRect.Right-SRect.Left, SRect.Bottom-SRect.Top,
                 clBtnText, tdTop);
    SRect.Left:=ARect.Right-SWidth;
    SRect.Right:=ARect.Right;
    Brush.Color:=clBtnFace;
    FillRect(SRect);
    DrawBevel(ACanvas, SRect, FDownIndex<>1, true);
    DrawTriangle(ACanvas, SRect.Left, SRect.Top,
                 SRect.Right-SRect.Left, SRect.Bottom-SRect.Top,
                 clBtnText, tdBottom);
  end;
end;

procedure TSliderEdit.DrawResetButtons(ACanvas:TCanvas;ARect:TRect);
var SRect  : TRect;
    SWidth : integer;
begin
  with ACanvas do begin
    { Reset-Button zeichnen }
    SRect:=ARect;
    SWidth:=(ARect.Right-ARect.Left) div 2;
    if FRulerDirection=rdHorizontal then
      SRect.Bottom:=SRect.Top+ResetBtnHeight
    else
      SRect.Right:=SRect.Left+SWidth;
    Brush.Color:=clBtnFace;
    FillRect(SRect);
    DrawBevel(ACanvas, SRect, FDownIndex<>2, true);
    Font.Name:='Small Fonts';
    Font.Size:=6;
    Font.Style:=[];
    Font.Color:=clBtnText;
    Brush.Style:=bsClear;
    DrawText(Handle, '0', 1, SRect, DT_SingleLine or DT_Center or DT_VCenter);
    Brush.Style:=bsSolid;

    if assigned(FValueEdit) then begin
      { Close-Button zeichnen }
      SRect:=ARect;
      if FRulerDirection=rdHorizontal then
        SRect.Top:=ARect.Bottom-ResetBtnHeight
      else
        SRect.Left:=ARect.Right-SWidth;
      Brush.Color:=clBtnFace;
      FillRect(SRect);
      DrawBevel(ACanvas, SRect, FDownIndex<>3, true);
      Font.Color:=clRed;
      Brush.Style:=bsClear;
      DrawText(Handle, 'X', 1, SRect, DT_SingleLine or DT_Center or DT_VCenter);
      Brush.Style:=bsSolid;
    end;
  end;
end;

procedure TSliderEdit.DrawRuler(ACanvas:TCanvas;ARect:TRect;DrawFocus:boolean);
var SRect          : TRect;
    SWidth,SHeight,
    RulerPosition,
    HalfSlidWidth,
    HalfWidth,
    TickLength,i   : integer;
    TickSpace      : single;

  procedure DrawTick(X,Y,TickLength:integer);
  begin
    with ACanvas do begin
      if FRulerDirection=rdHorizontal then begin
        DrawLine(ACanvas, X, Y-4-TickLength, X, Y-4);
        DrawLine(ACanvas, X, Y+5, X, Y+5+TickLength);
      end
      else begin
        DrawLine(ACanvas, X-4-TickLength, Y, X-4, Y);
        DrawLine(ACanvas, X+5, Y, X+5+TickLength, Y);
      end;
    end;
  end; { DrawTick }

begin
  { Ruler, Ticks und Slider zeichnen }
  HalfSlidWidth:=FSliderWidth div 2;
  with ACanvas do begin
    { Ruler }
    Brush.Color:=FColors.Ruler;
    FillRect(ARect);
    DrawBevel(ACanvas, ARect, false, false);
    if FRulerDirection=rdHorizontal then begin
      HalfWidth:=(ARect.Bottom-ARect.Top) div 2;
      SWidth:=ARect.Bottom-ARect.Top-2;
      SRect.Left:=ARect.Left+HalfSlidWidth;
      SRect.Top:=ARect.Top+(SWidth div 2);
      SRect.Right:=ARect.Right-HalfSlidWidth;
      SRect.Bottom:=SRect.Top+3;
      SHeight:=SRect.Right-SRect.Left;
    end
    else begin
      HalfWidth:=(ARect.Right-ARect.Left) div 2;
      SWidth:=ARect.Right-ARect.Left-2;
      SRect.Left:=ARect.Left+(SWidth div 2);
      SRect.Top:=ARect.Top+HalfSlidWidth;
      SRect.Right:=SRect.Left+3;
      SRect.Bottom:=ARect.Bottom-HalfSlidWidth;
      SHeight:=SRect.Bottom-SRect.Top;
    end;
    DrawBevel(ACanvas, SRect, false, false);
    Pen.Color:=clBlack;
    InflateRect(SRect, -1, -1);
    Rectangle(SRect.Left, SRect.Top, SRect.Right, SRect.Bottom);

    { Ticks }
    if FTickStyle<>tsNone then begin
      { Long Ticks }
      Pen.Color:=FColors.TickMarks;
      Pen.Width:=2;
      TickLength:=HalfWidth-8;
      i:=GetSliderOffset(0);
      if FRulerDirection=rdHorizontal then begin
        DrawTick(SRect.Left, ARect.Top+HalfWidth, TickLength);
        if (FMinValue<>0) and (FMaxValue<>0) then
          DrawTick(SRect.Left+i, ARect.Top+HalfWidth, TickLength);
        DrawTick(SRect.Right, ARect.Top+HalfWidth, TickLength);
      end
      else begin
        DrawTick(ARect.Left+HalfWidth, SRect.Top, TickLength);
        if (FMinValue<>0) and (FMaxValue<>0) then
          DrawTick(ARect.Left+HalfWidth, SRect.Bottom-i, TickLength);
        DrawTick(ARect.Left+HalfWidth, SRect.Bottom, TickLength);
      end;
      Pen.Width:=1;
      if FTickStyle=tsAll then begin
        { Short Ticks }
        TickSpace:=SHeight/10;
        TickLength:=3;
        for i:=1 to 9 do begin
          if FRulerDirection=rdHorizontal then
            DrawTick(SRect.Left+round(i*TickSpace)-1, ARect.Top+HalfWidth, TickLength)
          else
            DrawTick(ARect.Left+HalfWidth, SRect.Top+round(i*TickSpace)-1, TickLength);
        end;
      end;
    end;

    if FFocusRect and Focused and DrawFocus then
      DoDrawFocusRect(ACanvas);

    { Slider }
    RulerPosition:=GetSliderOffset(FValue);
    if FRulerDirection=rdHorizontal then begin
      SRect.Left:=ARect.Left+RulerPosition+1;
      SRect.Top:=ARect.Top+1;
      SRect.Right:=SRect.Left+FSliderWidth;
      SRect.Bottom:=ARect.Bottom-1;
    end
    else begin
      SRect.Left:=ARect.Left+1;
      SRect.Top:=ARect.Bottom-RulerPosition-FSliderWidth-1;
      SRect.Right:=ARect.Right-1;
      SRect.Bottom:=SRect.Top+FSliderWidth;
    end;
    Brush.Color:=FColors.SliderTop;
    Pen.Color:=clBlack;
    Rectangle(SRect.Left, SRect.Top, SRect.Right, SRect.Bottom);
    InflateRect(SRect, -1, -1);
    DrawColorBevel(ACanvas, SRect, true, false, FColors.FBorderHL, FColors.FBorderSh);
    InflateRect(SRect, -1, -1);
    DrawColorBevel(ACanvas, SRect, true, false, FColors.FBorderHL, FColors.FBorderSh);
    Pen.Width:=2;
    Pen.Color:=FColors.SliderMark;
    if FRulerDirection=rdHorizontal then
      DrawLine(ACanvas, SRect.Left+(FSliderWidth div 2)-2, SRect.Top+1,
                        SRect.Left+(FSliderWidth div 2)-2, SRect.Bottom-1)
    else
      DrawLine(ACanvas, SRect.Left+1, SRect.Top+(FSliderWidth div 2)-2,
                        SRect.Right-1, SRect.Top+(FSliderWidth div 2)-2);
  end;
end;

function TSliderEdit.GetRulerLength:integer;
var Temp,
    CWidth : integer;
    CRect  : TRect;
begin
  CRect:=ClientRect;
  InflateRect(CRect, -FBevelWidth-FBorderWidth, -FBevelWidth-FBorderWidth);
  if FRulerDirection=rdHorizontal then begin
    CWidth:=(CRect.Right-CRect.Left);
    Temp:=CWidth-FDisplayWidth-(FDisplayWidth div 2)-4;
  end
  else begin
    CWidth:=(CRect.Bottom-CRect.Top);
    Temp:=CWidth-FDisplayHeight-(2*FSpinBtnHeight)-7;
  end;
  Result:=Temp-FSliderWidth-1;
end;

function TSliderEdit.GetSliderOffset(const AValue:integer):integer;
var Temp    : extended;
    ALength : integer;
begin
  if FMaxValue-FMinValue<>0 then begin
    ALength:=GetRulerLength;
    Temp:=ALength/(FMaxValue-FMinValue);
    Result:=round(Temp*(AValue-FMinValue));
  end
  else
    Result:=0;
end;

procedure TSliderEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_Left) or (Key=VK_Down) then
    DoSpinBtnClick(true, Shift);
  if (Key=VK_Right) or (Key=VK_Up) then
    DoSpinBtnClick(false, Shift);
  if Key=VK_Prior then begin
    Include(Shift, ssShift);
    DoSpinBtnClick(false, Shift);
  end;
  if Key=VK_Next then begin
    Include(Shift, ssShift);
    DoSpinBtnClick(true, Shift);
  end;
  if Key=VK_Home then
    SetValue(FMinValue);
  if Key=VK_End then
    SetValue(FMaxValue);
  if Key=ord('0') then
    SetValue(0);
  if ((Key=VK_Escape) or (Key=VK_Return)) and assigned(FValueEdit) then
    FValueEdit.DestroySliderEdit;

  inherited KeyDown(Key, Shift);
end;

procedure TSliderEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FDownIndex:=-1;
  Invalidate;

  inherited KeyUp(Key, Shift);
end;

procedure TSliderEdit.Loaded;
begin
  inherited Loaded;

  if FMaxValue=FMinValue then begin
    if (FMaxValue=0) and (FMinValue=0) then begin
      SetMaxValue(127);
      SetMinValue(-128);
    end
    else
      SetMaxValue(FMinValue+1);
  end;
end;

procedure TSliderEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CRect,
    BRect     : TRect;
    CWidth    : integer;
    MoveMouse : boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);

  MoveMouse:=true;
  FMouseDown:=true;
  SetFocus;
  CRect:=ClientRect;
  InflateRect(CRect, -FBevelWidth-FBorderWidth, -FBevelWidth-FBorderWidth);

  { SpinButton-Bereich ermitteln }
  if RulerDirection=rdHorizontal then begin
    CWidth:=FDisplayWidth;
    BRect:=Bounds(CRect.Right-FDisplayWidth, CRect.Bottom-FSpinBtnHeight, CWidth, FSpinBtnHeight);
  end
  else begin
    CWidth:=CRect.Right-CRect.Left;
    BRect:=Bounds(CRect.Left, CRect.Top+FDisplayHeight+2, CWidth, FSpinBtnHeight);
  end;
  if (X>BRect.Left) and (X<BRect.Right) and
   (Y>BRect.Top) and (Y<BRect.Bottom) then
    { Maus ist über den SpinButtons }
    DoSpinBtnClick(X>(BRect.Left+(CWidth div 2)), Shift);
  { Reset-Button-Bereich ermitteln }
  if RulerDirection=rdHorizontal then begin
    BRect:=Bounds(CRect.Left, CRect.Top, CWidth div 2, ResetBtnHeight);
    if (X>BRect.Left) and (X<BRect.Right) and
     (Y>BRect.Top) and (Y<BRect.Bottom) then begin
      { Maus ist über dem Reset-Button: FValue auf 0 setzen }
      FDownIndex:=2;
      SetValue(0);
    end;
    BRect:=Bounds(CRect.Left, CRect.Bottom-ResetBtnHeight, CWidth div 2, ResetBtnHeight);
    if (X>BRect.Left) and (X<BRect.Right) and
     (Y>BRect.Top) and (Y<BRect.Bottom) and assigned(FValueEdit) then begin
      { Maus ist über dem Close-Button: Control ausblenden }
      FValueEdit.DestroySliderEdit;
      MoveMouse:=false;
    end;
  end
  else begin
    BRect:=Bounds(CRect.Left, CRect.Bottom-ResetBtnHeight, CWidth, ResetBtnHeight);
    if (X>BRect.Left) and (X<BRect.Right) and
     (Y>BRect.Top) and (Y<BRect.Bottom) then begin
      { Maus ist über den Reset-Buttons }
      if X<(BRect.Left+(CWidth div 2)) then begin
        { Reset-Button: FValue auf 0 setzen }
        FDownIndex:=2;
        SetValue(0);
      end
      else begin
        { Close-Button: Control ausblenden }
        if assigned(FValueEdit) then begin
          FValueEdit.DestroySliderEdit;
          MoveMouse:=false;
        end;
      end;
    end;
  end;

  if MoveMouse and not FAutoHide and (ssLeft in Shift) then
    { Sliderposition setzen }
    MouseMove(Shift, X, Y);
end;

procedure TSliderEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var MousePos,
    RulerLen : integer;
    Temp     : extended;
begin
  if ssLeft in Shift then begin
    RulerLen:=GetRulerLength;
    if FRulerDirection=rdHorizontal then
      MousePos:=X-(FDisplayWidth div 2)-6-(FSliderWidth div 2)
    else
      MousePos:=Height-Y-FSpinBtnHeight-7-(FSliderWidth div 2);
    if (MousePos>=0) and (MousePos<=RulerLen) and (FMaxValue-FMinValue<>0) then begin
      Temp:=MousePos/RulerLen;
      SetValue(round(Temp*(FMaxValue-FMinValue))+FMinValue);
    end;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TSliderEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMouseDown:=false;
  if FAutoHide and assigned(FValueEdit) then
    FValueEdit.DestroySliderEdit;
  if not FAutoHide then begin
    FDownIndex:=-1;
    Invalidate;
  end;
end;

procedure TSliderEdit.Paint;
var CRect,
    ARect : TRect;
    i     : integer;
begin
  CRect:=ClientRect;
  with FDrawBuffer.Canvas do begin
    { Bevel }
    Pen.Width:=1;
    if FBevelStyle<>bvNone then begin
      {$IFDEF SR_Delphi5_Up}
      if FBevelStyle<>bvSpace then begin
      {$ENDIF}
        for i:=1 to FBevelWidth do begin
          if FBevelStyle=bvRaised then
            DrawBevel(FDrawBuffer.Canvas, CRect, true, false);
          if FBevelStyle=bvLowered then
            DrawBevel(FDrawBuffer.Canvas, CRect, false, false);
          InflateRect(CRect, -1, -1);
        end;
      {$IFDEF SR_Delphi5_Up}
      end;
      {$ENDIF}
    end;

    { Hintergrund }
    Brush.Color:=FColors.Background;
    FillRect(CRect);
    InflateRect(CRect, -FBorderWidth, -FBorderWidth);

    { Wertanzeige und SpinButtons }
    ARect:=CRect;
    if FRulerDirection=rdHorizontal then begin
      ARect.Left:=ARect.Right-FDisplayWidth;
      CRect.Right:=ARect.Left-2;
    end
    else begin
      ARect.Bottom:=FDisplayHeight+FSpinBtnHeight+4;
      CRect.Top:=ARect.Bottom+2;
    end;
    DrawValueDisplay(FDrawBuffer.Canvas, ARect);

    { Reset- und Close-Buttons }
    ARect:=CRect;
    if FRulerDirection=rdHorizontal then begin
      ARect.Right:=ARect.Left+ResetBtnWidth;
      CRect.Left:=ARect.Right+2;
    end
    else begin
      ARect.Top:=ARect.Bottom-ResetBtnHeight;
      CRect.Bottom:=ARect.Top-1;
    end;
    DrawResetButtons(FDrawBuffer.Canvas, ARect);

    { Slider }
    DrawRuler(FDrawBuffer.Canvas, CRect, not FMouseDown);
  end;

  Canvas.Draw(0, 0, FDrawBuffer);
end;

procedure TSliderEdit.SetBevelStyle(newValue: TPanelBevel);
begin
  if NewValue<>FBevelStyle then begin
    FBevelStyle:=NewValue;
    GetRulerLength;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetBevelWidth(newValue: integer);
begin
  if NewValue<>FBevelWidth then begin
    FBevelWidth:=NewValue;
    GetRulerLength;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetBorderWidth(newValue: integer);
begin
  if NewValue<>FBorderWidth then begin
    FBorderWidth:=NewValue;
    GetRulerLength;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetColors(newValue: TColors);
begin
  with FColors do begin
    Background:=newValue.Background;
    NumBackground:=newValue.NumBackground;
    NumFrame:=newValue.NumFrame;
    SliderBorder:=newValue.SliderBorder;
    SliderTop:=newValue.SliderTop;
    SliderMark:=newValue.SliderMark;
    TickMarks:=newValue.TickMarks;
  end;
  Invalidate;
end;

procedure TSliderEdit.SetDisplayWidth(newValue: integer);
begin
  if NewValue<>FDisplayWidth then begin
    FDisplayWidth:=NewValue;
    if FRulerDirection=rdHorizontal then begin
      GetRulerLength;
      Invalidate;
    end;
  end;
end;

procedure TSliderEdit.SetFocusRect(newValue: boolean);
begin
  if NewValue<>FFocusRect then begin
    FFocusRect:=NewValue;
    if Focused then
      Invalidate;
  end;
end;

procedure TSliderEdit.SetMaxValue(newValue: integer);
begin
  if NewValue<>FMaxValue then begin
    FMaxValue:=NewValue;
    if FValue>FMaxValue then
      SetValue(FMaxValue);
  end;
end;

procedure TSliderEdit.SetMinValue(newValue: integer);
begin
  if NewValue<>FMinValue then begin
    FMinValue:=NewValue;
    if FValue<FMinValue then
      SetValue(FMinValue);
  end;
end;

procedure TSliderEdit.SetSliderWidth(newValue: integer);
begin
  if newValue<>FSliderWidth then begin
    FSliderWidth:=newValue;
    GetRulerLength;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetRulerDirection(newValue: TRulerDirection);
var OldSize : integer;
begin
  if newValue<>FRulerDirection then begin
    FRulerDirection:=newValue;
    if not (csReading in ComponentState) then begin
      OldSize:=Width;
      Width:=Height;
      Height:=OldSize;
      Invalidate;
    end;
  end;
end;

procedure TSliderEdit.SetSpinBtnHeight(newValue: integer);
begin
  if NewValue<>FSpinBtnHeight then begin
    FSpinBtnHeight:=NewValue;
    GetRulerLength;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetTickStyle(newValue: TTickStyle);
begin
  if NewValue<>FTickStyle then begin
    FTickStyle:=NewValue;
    Invalidate;
  end;
end;

procedure TSliderEdit.SetValue(NewValue: integer);
begin
  if newValue<>FValue then begin
    if NewValue<FMinValue then
      NewValue:=FMinValue;
    if NewValue>FMaxValue then
      NewValue:=FMaxValue;
    FValue:=NewValue;
    Change;
  end;
end;

procedure TSliderEdit.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSliderEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  { Kein Fokuswechsel durch Cursortasten }
  Message.Result:=Message.Result or DLGC_WANTARROWS;
end;

procedure TSliderEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  if FHasFocus and assigned(FValueEdit) then
    FValueEdit.DestroySliderEdit
  else
    Invalidate;
  FHasFocus:=false;
end;

procedure TSliderEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  FHasFocus:=true;
  Invalidate;
end;

procedure TSliderEdit.WMSize(var Message: TWMSize);
begin
  inherited;

  FDrawBuffer.Width:=Width;
  FDrawBuffer.Height:=Height;
  GetRulerlength;
  Message.Result := 0;
end;


{ TSRValueEdit }

constructor TSRValueEdit.Create(AOwner: TComponent);
var i : byte;
begin
  inherited Create(AOwner);

  Randomize;
  Width:=60;
  Height:=100;

  FNumericEdit:=TNumericEdit.Create(Self);
  FNumericEdit.Parent:=Self;
  FNumericEdit.FValueEdit:=Self;
  FNumericEdit.Left:=FNumEditOffset;
  FNumericEdit.Top:=FNumEditOffset;
  FNumericEdit.Width:=Self.Width-(2*FNumEditOffset);

  Font.Color:=clLime;
  Font.Style:=[fsBold];

  FColors := TColors.Create(Self);

  FBevelStyle:=bvRaised;
  FBevelWidth:=1;
  FBorderWidth:=4;
  FCaptureMouse:=true;
  FDecSeparator:='.';
  FFocusRect:=false;
  if csDesigning in ComponentState then begin
    FNumDigits:=4;
    FDecSeparatorPos:=2;
    FMaxValue:=99.99;
    FMinValue:=-99.99;
  end;
  for i:=0 to 4 do
    FScrewPos[i]:=Random(4);
  FScrewSize:=8;
  FShowScrews:=false;
  FRulerDirection:=rdVertical;
  FSliderEditLength:=DefaultRulerLength;
  FSliderEditWidth:=DefaultRulerWidth;
  FSliderWidth:=DefaultThumbWidth;
  FSpinBtnHeight:=10;
  FSpinControlWidth:=25;
  FTickStyle:=tsAll;
  FValue:=0;
  FValueHistory:=true;
  CalcBorderWidth;

end;

destructor TSRValueEdit.Destroy;
var i : integer;
begin
  if assigned(FColors) then
    FreeAndNil(FColors);

  if assigned(FNumericEdit) then
    FreeAndNil(FNumericEdit);

  if assigned(FHistoryMenu) then begin
    for i:=FHistoryMenu.Items.Count-1 downto 0 do
      FHistoryMenu.Items[i].Free;
    FreeAndNil(FHistoryMenu);
  end;

  DestroySliderEdit;

  inherited Destroy;
end;

procedure TSRValueEdit.AddHistoryItem(ValueNr:byte);
var NewItem : TMenuItem;
begin
  if (trunc(FHistoryValue[ValueNr]*10000)<>trunc(FValue*10000)) and assigned(FHistoryMenu) then begin
    NewItem:=TMenuItem.Create(Self);
    with NewItem do begin
      Caption:=FloatToStrF(FHistoryValue[ValueNr], ffNumber, 15, FDecSeparatorPos);
      Tag:=ValueNr;
      OnClick:=HistoryItemClick;
    end;
    FHistoryMenu.Items.Insert(0, NewItem);
  end;
end;

procedure TSRValueEdit.AddValueToHistory(AValue:extended);
var i : byte;

  function ValueExists(AValue:extended):boolean;
  var i : byte;
  begin
    Result:=false;
    if FHistoryCount>0 then
      for i:=0 to FHistoryCount-1 do
        if (trunc(FHistoryValue[i]*10000)=trunc(AValue*10000)) then
          Result:=true;
  end; {ValueExists}

begin
  if not ValueExists(AValue) then begin
    if FHistoryCount<MaxHistoryItems then begin
      FHistoryValue[FHistoryCount]:=AValue;
      FHistoryCount:=FHistoryCount+1;
    end
    else begin
      for i:=0 to MaxHistoryItems-2 do
        FHistoryValue[i]:=FHistoryValue[i+1];
      FHistoryValue[MaxHistoryItems-1]:=AValue;
    end;
  end;
end;

procedure TSRValueEdit.CalcBorderWidth;
begin
  FOldOffset:=FNumEditOffset;
  if FShowScrews then
    FNumEditOffset:=ScrewSize
  else
    FNumEditOffset:=0;
  if FBevelStyle<>bvNone then
    FNumEditOffset:=FNumEditOffset+FBevelWidth;
  FNumEditOffset:=FNumEditOffset+FBorderWidth;
  if assigned(FNumericEdit) and (FOldOffset<>FNumEditOffset) then
    SetControlsPosAndWidth;
end;

procedure TSRValueEdit.Change;
begin
  FOldvalue:=FValue;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSRValueEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if assigned(FNumericEdit) then
    FNumericEdit.Font.Assign(Self.Font);
end;

procedure TSRValueEdit.DestroySliderEdit;
begin
  if assigned(FSliderEdit) then begin
    FreeAndNil(FSliderEdit);
    if assigned(FOnHideSlider) then
      FOnHideSlider(Self);
  end;
  if FCaptureMouse then
    ClipCursor(nil);
  if FValueHistory then
    AddValueToHistory(FValue);
  if assigned(FNumericEdit) then
    FNumericEdit.SetFocus;
end;

procedure TSRValueEdit.DoDrawFocusRect(IsFocused:boolean);
var CRect : TRect;
begin
  CRect:=ClientRect;
  InflateRect(CRect, -FNumEditOffset, -FNumEditOffset);
  if assigned(FNumericEdit) then
    CRect.Top:=FNumericEdit.Top+FNumericEdit.Height+4;
  if IsFocused then begin
    Canvas.Brush.Color:=clBlack;
    Canvas.Pen.Color:=clBlack;
    DrawDotLine(Canvas, CRect.Left+1, CRect.Top, CRect.Right-CRect.Left-2, true);
    DrawDotLine(Canvas, CRect.Right-1, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
    DrawDotLine(Canvas, CRect.Left+1, CRect.Bottom-1, CRect.Right-CRect.Left-2, true);
    DrawDotLine(Canvas, CRect.Left, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
  end
  else begin
    Canvas.Brush.Color:=FColors.Background;
    Canvas.FrameRect(CRect);
  end;
end;

procedure TSRValueEdit.DoSpinBtnClick(BtnIndex:integer;UpBtn:boolean);
begin
  if assigned(FOnSpinBtnClick) then
    FOnSpinBtnClick(Self, BtnIndex, UpBtn);
end;

procedure TSRValueEdit.DrawRadioControl(IsFocused:boolean);
var ARect,
    CRect   : TRect;
    Center  : TPoint;
    i,Angle : integer;
    Temp    : double;
begin
  CRect:=ClientRect;
  if assigned(FNumericEdit) then
    InflateRect(CRect, -FNumEditOffset, -FNumEditOffset);
  ARect:=GetSpinControlRect(CRect, true);
  with Canvas do begin
    { Border }
    if FocusRect and Focused then
      DoDrawFocusRect(true);
    Brush.Color:=clBlack;
    Pen.Color:=clBlack;
    Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    InflateRect(ARect, -2, -2);
    { Bevel }
    Pen.Width:=2;
    Pen.Color:=FColors.FBorderSh;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Left, ARect.Bottom, ARect.Right, ARect.Top);
    Pen.Color:=FColors.FBorderHL;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
    { Top }
    InflateRect(ARect, -1, -1);
    Pen.Width:=1;
    Pen.Color:=clBlack;
    Brush.Color:=FColors.SliderTop;
    Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    { Mark }
    Center.X:=ARect.Left+(ARect.Right-ARect.Left) div 2;
    Center.Y:=ARect.Top+(ARect.Bottom-ARect.Top) div 2;
    if FMaxValue-FMinValue<>0 then begin
      Temp:=240/(FMaxValue-FMinValue);
      Angle:=round(Temp*(FValue-FMinValue));
    end
    else
      Angle:=0;
    Pen.Color:=FColors.SliderMark;
    Pen.Width:=2;
    MoveTo(Center.X, Center.Y);
    LineTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+2, 120-Angle),
           YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+2, 120-Angle));
    { Ticks }
    if FTickStyle<>tsNone then begin
      Pen.Color:=FColors.TickMarks;
      Pen.Width:=2;
      for i:=0 to 2 do begin
        MoveTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+5, 120+(i*120)),
               YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+5, 120+(i*120)));
        LineTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+10, 120+(i*120)),
               YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+10, 120+(i*120)));
      end;
      if FTickStyle=tsAll then begin
        Pen.Width:=1;
        for i:=0 to 5 do begin
          MoveTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+5, 240+(i*24)),
                 YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+5, 240+(i*24)));
          LineTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+10, 240+(i*24)),
                 YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+10, 240+(i*24)));
          MoveTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+5, 120-(i*24)),
                 YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+5, 120-(i*24)));
          LineTo(XKoord(Center.X, ((ARect.Right-ARect.Left) div 2)+10, 120-(i*24)),
                 YKoord(Center.Y, ((ARect.Bottom-ARect.Top) div 2)+10, 120-(i*24)));
        end;
      end;
    end;
  end;
end;

procedure TSRValueEdit.DrawScrew(X,Y,Nr:integer);
var ScrewRegion : HRgn;
begin
  with Canvas do begin
    Pen.Width:=1;
    { Außenrand }
    Pen.Color:=clBtnShadow;
    Brush.Color:=clSilver;
    Ellipse(X, Y, X+FScrewSize, Y+FScrewSize);
    Arc(X,Y,X+FScrewSize,Y+FScrewSize,
        X+((FScrewSize div 4)*3), Y+(FScrewSize div 4),
        X+(FScrewSize div 4), Y+((FScrewSize div 4)*3));
    Pen.Color:=clBtnHighlight;
    Arc(X, Y, X+FScrewSize, Y+FScrewSize,
        X+(FScrewSize div 4), Y+((FScrewSize div 4)*3),
        X+((FScrewSize div 4)*3), Y+(FScrewSize div 4));
    Pen.Color:=clBlack;
  end;
  { Schlitz }
  ScrewRegion:=CreateEllipticRgn(X+1, Y+1, X+FScrewSize-1, Y+FScrewSize-1);
  try
    SelectClipRgn(Canvas.Handle, ScrewRegion);
    Canvas.Pen.Color:=clBtnShadow;
    case Nr of
      0 : DrawLine(Canvas, X, Y, X+FScrewSize, Y+FScrewSize);
      1 : DrawLine(Canvas, X, Y+FScrewSize-1, X+FScrewSize, Y-1);
      2 : DrawLine(Canvas, X, Y+(FScrewSize div 2)-1, X+FScrewSize, Y+(FScrewSize div 2)-1);
      3 : DrawLine(Canvas, X+(FScrewSize div 2)-1, Y, X+(FScrewSize div 2)-1, Y+FScrewSize);
    end;
    Canvas.Pen.Color:=clBtnHighlight;
    case Nr of
      0 : DrawLine(Canvas, X, Y+1, X+FScrewSize, Y+1+FScrewSize);
      1 : DrawLine(Canvas, X, Y+FScrewSize, X+FScrewSize, Y);
      2 : DrawLine(Canvas, X+1, Y+(FScrewSize div 2), X+1+FScrewSize, Y+(FScrewSize div 2));
      3 : DrawLine(Canvas, X+(FScrewSize div 2), Y, X+(FScrewSize div 2), Y+FScrewSize);
    end;
  finally
    DeleteObject(ScrewRegion);
    SelectClipRgn(Canvas.Handle, 0);
  end;
end;

function TSRValueEdit.GetSpinControlRect(CRect:TRect;IsClientRect:boolean):TRect;
var CWidth,
    CHeight : integer;
begin
  if IsClientRect and assigned(FNumericEdit) then begin
    InflateRect(CRect, -FNumEditOffset, -FNumEditOffset);
    CRect.Top:=FNumericEdit.Top+FNumericEdit.Height+4;
  end;
  CWidth:=CRect.Right-CRect.Left;
  CHeight:=CRect.Bottom-CRect.Top;
  with Result do begin
    Left:=CRect.Left+((CWidth-FSpinControlWidth) div 2);
    Top:=CRect.Top+((CHeight-FSpinControlWidth) div 2)+4;
    Right:=Left+FSpinControlWidth;
    Bottom:=Top+FSpinControlWidth;
  end;
end;

function TSRValueEdit.IsInsideSpinControl(X,Y: Integer):boolean;
var BtnEllipse : HRgn;
    ARect      : TRect;
begin
  { Ist die Maus über dem Drehknopf? }
  ARect:=GetSpinControlRect(ClientRect, true);
  BtnEllipse:=CreateEllipticRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  Result:=PtInRegion(BtnEllipse, X, Y);
  DeleteObject(BtnEllipse);
end;

procedure TSRValueEdit.HistoryItemClick(Sender: TObject);
var ValueNr : integer;
begin
  ValueNr:=(Sender as TMenuItem).Tag;
  if (ValueNr>=0) and (ValueNr<FHistoryCount) then
    SetValue(FHistoryValue[ValueNr]);
    
  if assigned(FOnHistoryClick) then
    FOnHistoryClick(Self);
end;

procedure TSRValueEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MenuPos : TPoint;
    i       : integer;
    OnSpin  : boolean;
begin
  OnSpin:=IsInsideSpinControl(X, Y);
  if Enabled and not assigned(FSliderEdit) and
   assigned(FNumericEdit) and not OnSpin then
    FNumericEdit.SetFocus;

  if Enabled and not assigned(FSliderEdit) and
   (Owner is TForm) and (Button=mbLeft) and OnSpin then
      ShowSliderEdit(not (ssShift in Shift))
  else
    inherited MouseDown(Button, Shift, X, Y);

  if FValueHistory and (Button=mbRight) and (PopupMenu=nil) then begin
    { Value-History als PopupMenu anzeigen }
    FHistoryMenu:=TPopupMenu.Create(Self);
    for i:=0 to FHistoryCount-1 do
      AddHistoryItem(i);
    MenuPos:=ClientToScreen(Point(X, Y));
    with FHistoryMenu do begin
      Alignment:=paRight;
      {$IFDEF SR_Delphi5_Up}
      AutoHotkeys:=maManual;
      {$ENDIF}
      Popup(MenuPos.X, MenuPos.Y);
    end;
  end;
end;

procedure TSRValueEdit.Paint;
var ARect,
    BRect : TRect;
    i     : integer;
begin
  inherited Paint;

  ARect:=ClientRect;
  { Rahmen }
  if FBevelStyle<>bvNone then begin
    {$IFDEF SR_Delphi5_Up}
    if FBevelStyle<>bvSpace then begin
    {$ENDIF}
      BRect:=ARect;
      for i:=1 to FBevelWidth do begin
        if FBevelStyle=bvRaised then
          DrawBevel(Canvas, BRect, true, false);
        if FBevelStyle=bvLowered then
          DrawBevel(Canvas, BRect, false, false);
        InflateRect(BRect, -1, -1);
      end;
    {$IFDEF SR_Delphi5_Up}
    end;
    {$ENDIF}
    InflateRect(ARect, -BevelWidth, -BevelWidth);
  end;

  { Hintergrund }
  with Canvas do begin
    Brush.Color:=FColors.Background;
    FillRect(ARect);
  end;

  { Schrauben }
  if FShowScrews then begin
    DrawScrew(ARect.Left+1, ARect.Top+1, 0);
    DrawScrew(ARect.Right-FScrewSize-1, ARect.Top+1, 1);
    DrawScrew(ARect.Left+1, ARect.Bottom-FScrewSize-1, 2);
    DrawScrew(ARect.Right-FScrewSize-1, ARect.Bottom-FScrewSize-1, 3);
  end;

  { RadioControl }
  DrawRadioControl(Focused);
  
  { NumericEdit }
  if assigned(FNumericEdit) then
    FNumericEdit.Paint;
end;

procedure TSRValueEdit.SetBevelStyle(newValue: TPanelBevel);
begin
  if NewValue<>FBevelStyle then begin
    FBevelStyle:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetBevelWidth(newValue: integer);
begin
  if NewValue<>FBevelWidth then begin
    FBevelWidth:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetBorderWidth(newValue: integer);
begin
  if NewValue<>FBorderWidth then begin
    FBorderWidth:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetColors(newValue: TColors);
begin
  with FColors do begin
    Background:=newValue.Background;
    NumBackground:=newValue.NumBackground;
    NumFrame:=newValue.NumFrame;
    SliderBorder:=newValue.SliderBorder;
    SliderTop:=newValue.SliderTop;
    SliderMark:=newValue.SliderMark;
    TickMarks:=newValue.TickMarks;
  end;
  if assigned(FNumericEdit) then begin
    FNumericEdit.SetColor(FColors.NumBackground);
    FNumericEdit.SetFrameColor(FColors.NumFrame);
  end;
  Invalidate;
end;

procedure TSRValueEdit.SetControlsPosAndWidth;
var AWidth : integer;
begin
  if assigned(FNumericEdit) then begin
    AWidth:=(Width-(2*FNumEditOffset)) div (FNumericEdit.FNumDigits+1);
    FNumericEdit.Width:=AWidth*(FNumericEdit.FNumDigits+1)+2;
    FNumericEdit.Left:=FNumEditOffset+((Width div 2)-FNumEditOffset-(FNumericEdit.Width div 2));
    FNumericEdit.Top:=FNumEditOffset;
  end;
end;

procedure TSRValueEdit.SetDecSeparator(newValue: char);
begin
  if NewValue<>FDecSeparator then begin
    FDecSeparator:=NewValue;
    if assigned(FNumericEdit) then
      FNumericEdit.DecSeparator:=NewValue;
  end;
end;

procedure TSRValueEdit.SetDecSeparatorPos(newValue: byte);
var NumMax : extended;
begin
  if NewValue<>FDecSeparatorPos then begin
    FDecSeparatorPos:=NewValue;
    if assigned(FNumericEdit) then begin
      FNumericEdit.SetDecSeparatorPos(NewValue);
      NumMax:=FNumericEdit.GetMaxValue;
      if FMaxValue>NumMax then
        FMaxValue:=NumMax;
      if abs(FMinValue)>NumMax then
        FMinValue:=-NumMax;
    end;
  end;
end;

procedure TSRValueEdit.SetFocusRect(newValue: boolean);
begin
  if NewValue<>FFocusRect then begin
    FFocusRect:=NewValue;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetMaxValue(newValue: extended);
var NumMax : extended;
begin
  if NewValue<>FMaxValue then begin
    if NewValue<=FMinValue then
      FMaxValue:=FMinValue+1
    else begin
      if assigned(FNumericEdit) then begin
        NumMax:=FNumericEdit.GetMaxValue;
        if abs(NewValue)>NumMax then
          FMaxValue:=NumMax
        else
          FMaxValue:=NewValue;
      end
      else
        FMaxValue:=NewValue;
    end;
    if FValue>FMaxValue then
      SetValue(FMaxValue);
    if assigned(FNumericEdit) then
      FNumericEdit.SetMaxValue(FMaxValue);
  end;
end;

procedure TSRValueEdit.SetMinValue(newValue: extended);
var NumMax : extended;
begin
  if NewValue<>FMinValue then begin
    if NewValue>=FMaxValue then
      FMinValue:=FMaxValue-1
    else begin
      if assigned(FNumericEdit) then begin
        NumMax:=FNumericEdit.GetMaxValue;
        if abs(NewValue)>NumMax then
          FMinValue:=-NumMax
        else
          FMinValue:=NewValue;
      end
      else
        FMinValue:=NewValue;
    end;
    if FValue<FMinValue then
      SetValue(FMinValue);
    if assigned(FNumericEdit) then
      FNumericEdit.SetMinValue(FMinValue);
  end;
end;

procedure TSRValueEdit.SetNumDigits(newValue: TNumDigits);
begin
  if NewValue<>FNumDigits then begin
    FNumDigits:=NewValue;
    if assigned(FNumericEdit) then begin
      FNumericEdit.NumDigits:=NewValue;
      SetControlsPosAndWidth;
      FNumericEdit.Invalidate;
    end;
  end;
end;

procedure TSRValueEdit.SetShowScrews(newValue: boolean);
begin
  if NewValue<>FShowScrews then begin
    FShowScrews:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetScrewSize(newValue: TScrewSize);
begin
  if NewValue<>FScrewSize then begin
    FScrewSize:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetRulerDirection(newValue: TRulerDirection);
begin
  if NewValue<>FRulerDirection then begin
    if (NewValue=rdHorizontal) and (FSliderEditWidth>Height) then
      SetSliderEditWidth(Height);
    if (NewValue=rdVertical) and (FSliderEditWidth>Width) then
      SetSliderEditWidth(Width);
    FRulerDirection:=NewValue;
  end;
end;

procedure TSRValueEdit.SetSliderEditWidth(newValue: integer);
begin
  if NewValue<>FSliderEditWidth then begin
    if (FRulerDirection=rdHorizontal) and (NewValue>Height) then
      NewValue:=Height;
    if (FRulerDirection=rdVertical) and (NewValue>Width) then
      NewValue:=Width;
    FSliderEditWidth:=NewValue;
  end;
end;

procedure TSRValueEdit.SetSpinBtnHeight(newValue: integer);
begin
  if NewValue<>FSpinBtnHeight then begin
    FSpinBtnHeight:=NewValue;
    if assigned(FNumericEdit) then
      FNumericEdit.BtnHeight:=NewValue;
    CalcBorderWidth;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetSpinControlWidth(newValue: integer);
begin
  if NewValue<>FSpinControlWidth then begin
    FSpinControlWidth:=NewValue;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetTickStyle(newValue: TTickStyle);
begin
  if NewValue<>FTickStyle then begin
    FTickStyle:=NewValue;
    Invalidate;
  end;
end;

procedure TSRValueEdit.SetValue(newValue: extended);
begin
  if NewValue<>FValue then begin
    if NewValue<FMinValue then
      NewValue:=FMinValue;
    if NewValue>FMaxValue then
      NewValue:=FMaxValue;
    FValue:=NewValue;
    DrawRadioControl(Focused);
    if assigned(FNumericEdit) then
      FNumericEdit.Value:=FValue;
    Change;
  end;
end;

procedure TSRValueEdit.ShowSliderEdit(AutoHide: boolean);
var SliderOffset,
    MouseOffset,
    SliderPos     : integer;
    PosOnForm,
    TopLeft,
    ACenter,
    CPoint,SPoint : TPoint;
    CrsrRect,
    ARect         : TRect;
    OwnerForm     : TForm;
begin
  { Positionen des SpinControls und des Mauszeigers ermitteln }
  if (Owner is TForm) then begin
    OwnerForm:=TForm(Owner);
    ARect:=GetSpinControlRect(ClientRect, true);
    GetCursorPos(CPoint);
    SPoint:=TForm(Owner).ScreenToClient(CPoint);
    ACenter.X:=ARect.Left+((ARect.Right-ARect.Left) div 2);
    ACenter.Y:=ARect.Top+((ARect.Bottom-ARect.Top) div 2);
    TopLeft:=ClientToScreen(Point(0, 0));
    PosOnForm:=OwnerForm.ScreenToClient(TopLeft);

    { SliderEdit erstellen }
    FSliderEdit:=TSliderEdit.Create(Self);
    if assigned(FOnShowSlider) then
      FOnShowSlider(Self);
    with FSliderEdit do begin
      FAutoHide:=AutoHide;
      Colors.Assign(Self.FColors);
      FocusRect:=Self.FFocusRect;
      TickStyle:=Self.FTickStyle;
      Font.Color:=Self.Font.Color;
      BevelWidth:=1;
      BevelStyle:=FBevelStyle;
      RulerDirection:=Self.FRulerDirection;
      MaxValue:=trunc(Self.FMaxValue);
      MinValue:=trunc(Self.FMinValue);
      Parent:=TForm(Self.Owner);
      SliderWidth:=Self.FSliderWidth;
      Value:=trunc(Self.FValue);
      ValueEdit:=Self;
      CalcDisplayHeight;
    end;

    if FRulerDirection=rdHorizontal then begin
      { Position und Größe festlegen }
      FSliderEdit.FDisplayWidth:=FSliderEdit.Canvas.TextWidth(IntToStr(FSliderEdit.MinValue))+4;
      FSliderEdit.Width:=FSliderEditLength;
      FSliderEdit.Height:=FSliderEditWidth;
      SliderPos:=PosOnForm.X+ACenter.X-(FSliderEditLength div 2);
      if (SliderPos+FSliderEditLength)>=OwnerForm.ClientWidth then
        SliderPos:=OwnerForm.ClientWidth-FSliderEdit.Width-2;
      if SliderPos<0 then
        SliderPos:=2;
      FSliderEdit.Left:=SliderPos;
      FSliderEdit.Top:=PosOnForm.Y+ACenter.Y-(FSliderEditWidth div 2);
      { Mausposition über dem Ruler berechnen }
      SliderOffset:=FSliderEdit.GetSliderOffset(FSliderEdit.FValue);
      MouseOffset:=SliderOffset+FSliderEdit.BevelWidth+FSliderEdit.BorderWidth+
                   (FSliderEdit.DisplayWidth div 2)+(FSliderEdit.SliderWidth div 2)+3;
      CPoint:=Point(FSliderEdit.Left+MouseOffset, SPoint.Y);
    end
    else begin
      { Position und Größe festlegen }
      FSliderEdit.Width:=FSliderEditWidth;
      FSliderEdit.Height:=FSliderEditLength;
      FSliderEdit.Left:=PosOnForm.X+ACenter.X-(FSliderEditWidth div 2);
      SliderPos:=PosOnForm.Y+ACenter.Y-(FSliderEditLength div 2);
      if (SliderPos+FSliderEdit.Height)>=OwnerForm.ClientHeight then
        SliderPos:=OwnerForm.ClientHeight-FSliderEdit.Height-2;
      if SliderPos<0 then
        SliderPos:=2;
      FSliderEdit.Top:=SliderPos;
      { Mausposition über dem Ruler berechnen }
      SliderOffset:=FSliderEdit.GetSliderOffset(FSliderEdit.Value);
      MouseOffset:=SliderOffset+FSliderEdit.BevelWidth+FSliderEdit.BorderWidth+
                   FSliderEdit.SpinBtnHeight+(FSliderEdit.SliderWidth div 2)+4;
      CPoint:=Point(SPoint.X, FSliderEdit.Top+FSliderEditLength-MouseOffset);
    end;

    { Anzeigen }
    FSliderEdit.FDrawBuffer.Width:=FSliderEdit.Width;
    FSliderEdit.FDrawBuffer.Height:=FSliderEdit.Height;

    { Mauscursor auf den Ruler setzen }
    CPoint:=OwnerForm.ClientToScreen(CPoint);
    SetCursorPos(CPoint.X, CPoint.Y);
    if AutoHide and FCaptureMouse then begin
      { Mausbereich auf den Slider einschränken }
      GetWindowRect(FSliderEdit.Handle, CrsrRect);
      InflateRect(CrsrRect, -FSliderEdit.BorderWidth-FSliderEdit.BevelWidth,
                            -FSliderEdit.BorderWidth-FSliderEdit.BevelWidth);
      ClipCursor(@CrsrRect);
    end;
    if AutoHide then
      Self.Perform(WM_LButtonUp, 0, 0)
    else
      FSliderEdit.SetFocus;
  end;
end;

procedure TSRValueEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  { Kein Fokuswechsel durch Cursortasten }
  Message.Result:=Message.Result or DLGC_WANTARROWS;
end;

procedure TSRValueEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;

  if FocusRect then
    DoDrawFocusRect(false);
  if FValueHistory then
    AddValueToHistory(FValue);
end;

procedure TSRValueEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;

  if FocusRect then
    DoDrawFocusRect(true);
  if assigned(FNumericEdit) then
    FNumericEdit.SetFocus;
end;

procedure TSRValueEdit.WMSize(var Message: TWMSize);
begin
  inherited;

  SetControlsPosAndWidth;
  Message.Result := 0;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRValueEdit, TNumericEdit, TSliderEdit]);
end;

end.
