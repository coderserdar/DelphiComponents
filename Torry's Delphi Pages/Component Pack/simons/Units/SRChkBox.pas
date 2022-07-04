unit SRChkBox;

{ TSRCheckBox (C)opyright 2005 Version 1.40
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRCheckBox ist eine Checkbox-Komponente mit Autosize-,
  Transparent- und WordWrap-Eigenschaften. Außerdem wird kein OnClick-Ereignis
  abgefeuert, wenn die Checked-Eigenschaft per Programmcode geändert wird.
  Die Komponente ist abgeleteitet von TGraphicControl.

  Die Komponente TEnhancedCheckBox entspricht der Komponente TSRCheckBox,
  ist aber von TCustomControl abgeleitet und kann deshalb auch den Eingabefokus
  bekommen. Dafür entfällt die "Transparent"-Eigenschaft.

  Die Komponenten sind Public Domain, das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, {$ENDIF}
  Classes, Graphics, Controls, SysUtils, Messages, StdCtrls;

type
  TCheckStyle = (csBitmap, csCheckBox, csDiamond, csPushButton, csRadioButton, csTrafficLight);
  TCheckboxLayout = (clBottom, clCenter, clTop);
  TNumGlyphs = 0..4;

  TSRCheckBox = class(TGraphicControl)
  private
    FAlignment         : TLeftRight;
    FAllowAllUnchecked,
    FAllowGrayed,
    FAutoSize          : boolean;
    FColor             : TColor;
    FChecked           : boolean;
    FCheckSize         : integer;
    FGlyph             : TBitmap;
    FGrouped,
    FHoverActive       : boolean;
    FHoverColor        : TColor;
    FHoverCursor       : TCursor;
    FHoverFontColor    : TColor;
    FLayout            : TCheckboxLayout;
    FMouseDown,
    FMouseOnControl    : boolean;
    FNumGlyphs         : TNumGlyphs;
    FOldCursor         : TCursor;
    FSpacing           : integer;
    FState             : TCheckBoxState;
    FStateChanged      : boolean;
    FStyle             : TCheckStyle;
    FTranspColor       : TColor;
    FTransparent,
    FUnderlineOnEnter,
    FWordWrap          : boolean;

    FOnChange,
    FOnClick,
    FOnDblClick        : TNotifyEvent;
    FOnMouseDown       : TMouseEvent;
    FOnMouseEnter,
    FOnMouseExit       : TNotifyEvent;
    FOnMouseMove       : TMouseMoveEvent;
    FOnMouseUp         : TMouseEvent;

    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message:TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;

  protected
    procedure AdjustBounds;
    procedure Change; dynamic;
    procedure DblClick; override;
    procedure DrawGlyph(ACanvas:TCanvas; ARect:TRect);
    function GetTextRect(ARect: TRect): TRect;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton;
    procedure PaintCaption;

    procedure SetAlignment(newValue: TLeftRight);
    procedure SetAutosize(newValue: boolean);
    procedure SetColor(newValue: TColor);
    procedure SetChecked(newValue: boolean);
    procedure SetCheckSize(newValue: integer);
    procedure SetGlyph(newValue: TBitmap);
    procedure SetGrouped(newValue: boolean);
    procedure SetHoverActive(newValue: boolean);
    procedure SetHoverColor(newValue: TColor);
    procedure SetHoverFontColor(newValue: TColor);
    procedure SetLayout(newValue: TCheckboxLayout);
    procedure SetNumGlyphs(newValue: TNumGlyphs);
    procedure SetSpacing(newValue: integer);
    procedure SetState(newValue: TCheckBoxState);
    procedure SetStyle(newValue: TCheckStyle);
    procedure SetTransparent(newValue: boolean);
    procedure SetUnderlineOnEnter(newValue: boolean);
    procedure SetWordWrap(newValue: boolean);

    procedure UncheckGroupCheckBoxes;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    {$ENDIF}
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property AllowAllUnchecked: boolean read FAllowAllUnchecked write FAllowAllUnchecked;
    property AllowGrayed: boolean read FAllowGrayed write FAllowGrayed;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    {$IFDEF SR_Delphi5_Up}
    property BiDiMode;
    {$ENDIF}
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property CheckSize: integer read FCheckSize write SetCheckSize;
    property Color: TColor read FColor write SetColor;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    {$IFDEF SR_Delphi4_Up}
    property DragKind;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Grouped: boolean read FGrouped write SetGrouped;
    property HoverActive: boolean read FHoverActive write SetHoverActive;
    property HoverColor: TColor read FHoverColor write SetHoverColor;
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor;
    property Layout: TCheckboxLayout read FLayout write SetLayout;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 0;
    {$IFDEF SR_Delphi5_Up}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: integer read FSpacing write SetSpacing;
    property State: TCheckBoxState read FState write SetState;
    property Style: TCheckStyle read FStyle write SetStyle;
    property Transparent: boolean read FTransparent write SetTransparent;
    property UnderlineOnEnter: boolean read FUnderlineOnEnter write SetUnderlineOnEnter;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF SR_Delphi5_Up}
    property OnContextPopup;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF SR_Delphi4_Up}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnStartDock;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

  TEnhancedCheckBox = class(TCustomControl)
  private
    FAlignment         : TLeftRight;
    FAllowAllUnchecked,
    FAllowGrayed,
    FAutoSize          : boolean;
    FColor             : TColor;
    FChecked           : boolean;
    FCheckSize         : integer;
    FGlyph             : TBitmap;
    FGrouped,
    FHoverActive       : boolean;
    FHoverColor        : TColor;
    FHoverCursor       : TCursor;
    FHoverFontColor    : TColor;
    FLayout            : TCheckboxLayout;
    FMouseDown,
    FMouseOnControl    : boolean;
    FNumGlyphs         : TNumGlyphs;
    FOldCursor         : TCursor;
    FSpacing           : integer;
    FState             : TCheckBoxState;
    FStateChanged      : boolean;
    FStyle             : TCheckStyle;
    FTranspColor       : TColor;
    FUnderlineOnEnter,
    FWordWrap          : boolean;

    FOnChange,
    FOnClick,
    FOnDblClick        : TNotifyEvent;
    FOnMouseDown       : TMouseEvent;
    FOnMouseEnter,
    FOnMouseExit       : TNotifyEvent;
    FOnMouseMove       : TMouseMoveEvent;
    FOnMouseUp         : TMouseEvent;

    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message:TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;

  protected
    procedure AdjustBounds;
    procedure Change; dynamic;
    procedure DblClick; override;
    procedure DoDrawFocusRect(IsFocused:boolean); dynamic;
    procedure DrawDotLine(X1,Y1,Length:integer;Horizontal:boolean);
    procedure DrawGlyph(ACanvas:TCanvas; ARect:TRect);
    function GetTextRect(ARect: TRect): TRect;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton;
    procedure PaintCaption;

    procedure SetAlignment(newValue: TLeftRight);
    procedure SetAutosize(newValue: boolean);
    procedure SetColor(newValue: TColor);
    procedure SetChecked(newValue: boolean);
    procedure SetCheckSize(newValue: integer);
    procedure SetGlyph(newValue: TBitmap);
    procedure SetGrouped(newValue: boolean);
    procedure SetHoverActive(newValue: boolean);
    procedure SetHoverColor(newValue: TColor);
    procedure SetHoverFontColor(newValue: TColor);
    procedure SetLayout(newValue: TCheckboxLayout);
    procedure SetNumGlyphs(newValue: TNumGlyphs);
    procedure SetSpacing(newValue: integer);
    procedure SetState(newValue: TCheckBoxState);
    procedure SetStyle(newValue: TCheckStyle);
    procedure SetUnderlineOnEnter(newValue: boolean);
    procedure SetWordWrap(newValue: boolean);

    procedure UncheckGroupCheckBoxes;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    {$ENDIF}
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property AllowAllUnchecked: boolean read FAllowAllUnchecked write FAllowAllUnchecked;
    property AllowGrayed: boolean read FAllowGrayed write FAllowGrayed;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    {$IFDEF SR_Delphi5_Up}
    property BiDiMode;
    {$ENDIF}
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property CheckSize: integer read FCheckSize write SetCheckSize;
    property Color: TColor read FColor write SetColor;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    {$IFDEF SR_Delphi4_Up}
    property DragKind;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Grouped: boolean read FGrouped write SetGrouped;
    property HoverActive: boolean read FHoverActive write SetHoverActive;
    property HoverColor: TColor read FHoverColor write SetHoverColor;
    property HoverCursor: TCursor read FHoverCursor write FHoverCursor;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor;
    property Layout: TCheckboxLayout read FLayout write SetLayout;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 0;
    {$IFDEF SR_Delphi5_Up}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: integer read FSpacing write SetSpacing;
    property State: TCheckBoxState read FState write SetState;
    property Style: TCheckStyle read FStyle write SetStyle;
    property TabOrder;
    property TabStop;
    property UnderlineOnEnter: boolean read FUnderlineOnEnter write SetUnderlineOnEnter;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {$IFDEF SR_Delphi5_Up}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF SR_Delphi4_Up}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnStartDock;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
{$R *.R32}
{$ELSE}
{$R *.D16}
{$R *.R16}
{$ENDIF}

uses Forms;

const
  crLinkPoint = TCursor(-40);

function IsAccellerator(VK: Word; const Str: string): Boolean;
var P : Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (Upcase(Str[P + 1])=Upcase(Char(VK)));
end; {IsAccellerator}

procedure DrawCheckBox(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState; IsEnabled:boolean);
{$IFDEF SR_Delphi2_Up}
var Flags     : integer;
{$ELSE}
var OldColor  : TColor;
    Diff,
    BrdrWidth : integer;
    i         : byte;

  procedure Draw3DFrame(ARect:TRect;TLColor,BRColor:TColor);
  begin
    with ACanvas do begin
      Pen.Width:=1;
      Pen.Color:=TLColor;
      MoveTo(ARect.Right-2, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-2);
      Pen.Color:=BRColor;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Left, ARect.Bottom-1);
    end;
  end; {Draw3DFrame}

  procedure DrawCheckMark(ARect:TRect);
  begin
    with ACanvas do begin
      Pen.Color:=clWindowText;
      MoveTo(ARect.Right-2, ARect.Top+1);
      LineTo(ARect.Right-Diff, ARect.Top+Diff-1);
      LineTo(ARect.Right-Diff-(Diff div 2), ARect.Top+(Diff div 2)-1);
    end;
  end; {DrawCheckMark}
{$ENDIF}

begin
  {$IFDEF SR_Delphi2_Up}
  if IsEnabled then
    Flags:=0
  else
    Flags:=DFCS_Inactive;
  case AState of
    cbUnchecked : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonCheck or Flags);
    cbChecked   : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonCheck or Flags or DFCS_Checked);
    cbGrayed    : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonCheck or DFCS_Inactive);
  end;
  {$ELSE}
  BrdrWidth:=((ARect.Right-ARect.Left) div 15)+1;
  with ACanvas do begin
    { 3D-Rahmen }
    for i:=1 to BrdrWidth do begin
      Draw3DFrame(ARect, clBtnShadow, clBtnHighlight);
      InflateRect(ARect, -1, -1);
    end;
    for i:=1 to BrdrWidth do begin
      Draw3DFrame(ARect, clBlack, clBtnFace);
      InflateRect(ARect, -1, -1);
    end;

    { Hintergrund }
    OldColor:=Brush.Color;
    if IsEnabled and (AState<>cbGrayed) then
      Brush.Color:=clWindow
    else
      Brush.Color:=clBtnFace;
    FillRect(ARect);

    { Check-Häkchen }
    Diff:=round((ARect.Right-ARect.Left)/3*2);
    if AState=cbChecked then begin
      for i:=1 to BrdrWidth+2 do begin
        DrawCheckMark(ARect);
        OffsetRect(ARect, 0, 1);
      end;
    end;

    Brush.Color:=OldColor;
  end;
  {$ENDIF}
end; {DrawCheckBox}

procedure DrawDiamond(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState; IsEnabled:boolean);
var Offset        : integer;
    OldBrushStyle : TBrushStyle;
begin
  Offset:=(ARect.Right-ARect.Left) div 2;
  with ACanvas do begin
    Pen.Width:=1;
    if AState=cbUnChecked then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Left+Offset, ARect.Top);
    LineTo(ARect.Left, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Bottom-1);
    if AState=cbUnChecked then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    LineTo(ARect.Right-1, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Top);
    if AState<>cbUnchecked then begin
      OldBrushStyle:=Brush.Style;
      Pen.Color:=Brush.Color;
      Brush.Style:=bsSolid;
      if AState=cbChecked then
        Brush.Color:=clBlack
      else
        Brush.Color:=clGray;
      Polygon([Point(ARect.Left+Offset, ARect.Top+1),
              Point(ARect.Right-2, ARect.Top+Offset),
              Point(ARect.Left+Offset, ARect.Bottom-2),
              Point(ARect.Left+1, ARect.Top+Offset)]);
      Brush.Color:=Pen.Color;
      Brush.Style:=OldBrushStyle;
    end;
  end;
end; {DrawDiamond}

procedure DrawPushButton(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState; IsEnabled:boolean);
{$IFDEF SR_Delphi1}
var OldColor : TColor;
    i        : byte;
{$ENDIF}

  procedure Draw3DFrame(ARect:TRect;TLColor,BRColor:TColor);
  begin
    with ACanvas do begin
      Pen.Width:=1;
      Pen.Color:=TLColor;
      MoveTo(ARect.Right-2, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-2);
      Pen.Color:=BRColor;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Left, ARect.Bottom-1);
    end;
  end; {Draw3DFrame}

begin
  {$IFDEF SR_Delphi2_Up}
  case AState of
    cbUnchecked : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonPush);
    cbChecked   : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Pushed);
    cbGrayed    : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Flat);
  end;
  {$ELSE}
  with ACanvas do begin
    { 3D-Rahmen }
    if AState=cbChecked then begin
      Draw3DFrame(ARect, clBlack, clBtnHighlight);
      InflateRect(ARect, -1, -1);
      Draw3DFrame(ARect, clBtnShadow, clBtnFace);
      InflateRect(ARect, -1, -1);
    end
    else begin
      if AState=cbUnChecked then begin
        Draw3DFrame(ARect, clBtnHighlight, clBlack);
        InflateRect(ARect, -1, -1);
        Draw3DFrame(ARect, clBtnFace, clBtnShadow);
        InflateRect(ARect, -1, -1);
      end
      else begin
        Draw3DFrame(ARect, clBtnShadow, clBtnShadow);
        InflateRect(ARect, -1, -1);
      end;
    end;

    { Hintergrund }
    OldColor:=Brush.Color;
    Brush.Color:=clBtnFace;
    FillRect(ARect);

    Brush.Color:=OldColor;
  end;
  {$ENDIF}
end; {DrawPushButton}

procedure DrawRadioButton(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState; IsEnabled:boolean);
{$IFDEF SR_Delphi1}
var OldColor  : TColor;
    BrdrWidth,
    ChckWidth : integer;
    i         : byte;
{$ENDIF}

  procedure Draw3DBorder(ARect:TRect;TLColor,BRColor:TColor);
  begin
    with ACanvas do begin
      Pen.Width:=1;
      Pen.Color:=TLColor;
      Brush.Color:=Pen.Color;
      Pie(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1,
          ARect.Right-1, ARect.Top, ARect.Left, ARect.Bottom-1);
      Pen.Color:=BRColor;
      Brush.Color:=Pen.Color;
      Pie(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1,
          ARect.Left, ARect.Bottom-1, ARect.Right-1, ARect.Top);
    end;
  end; {Draw3DBorder}

begin
  {$IFDEF SR_Delphi2_Up}
  case AState of
    cbUnchecked : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonRadio);
    cbChecked   : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked);
    cbGrayed    : DrawFrameControl(ACanvas.Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked or DFCS_Inactive);
  end;
  {$ELSE}
  with ACanvas do begin
    BrdrWidth:=((ARect.Right-ARect.Left) div 15)+1;
    { 3D-Rahmen }
    Draw3DBorder(ARect, clBtnShadow, clBtnHighlight);
    InflateRect(ARect, -BrdrWidth, -BrdrWidth);
    Draw3DBorder(ARect, clBlack, clBtnFace);
    InflateRect(ARect, -BrdrWidth+1, -BrdrWidth+1);

    { Hintergrund }
    OldColor:=Brush.Color;
    if IsEnabled and (AState<>cbGrayed) then
      Brush.Color:=clWindow
    else
      Brush.Color:=clBtnFace;
    Ellipse(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1);
    ChckWidth:=(ARect.Right-ARect.Left) div 5;
    InflateRect(ARect, -ChckWidth-1, -ChckWidth-1);

    { Check-Punkt }
    if AState=cbChecked then begin
      Brush.Color:=clWindowText;
      Pen.Color:=clWindowText;
      Ellipse(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1);
    end;

    Brush.Color:=OldColor;
  end;
  {$ENDIF}
end; {DrawRadioButton}

procedure DrawTrafficLight(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState; IsEnabled:boolean);
const LightColors : array[TCheckBoxState] of TColor =
  (clLime, clRed, clYellow);
var OldColor      : TColor;
    OldBrushStyle : TBrushStyle;
begin
  with ACanvas do begin
    Pen.Color:=clBlack;
    Pen.Width:=1;
    OldColor:=Brush.Color;
    OldBrushStyle:=Brush.Style;
    Brush.Color:=LightColors[AState];
    Brush.Style:=bsSolid;
    InflateRect(ARect, -1, -1);
    Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    InflateRect(ARect, 1, 1);
    Pen.Color:=clBtnShadow;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
    Pen.Color:=clBtnHighlight;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Left, ARect.Bottom, ARect.Right, ARect.Top);
    Brush.Color:=OldColor;
    Brush.Style:=OldBrushStyle;
  end;
end; {DrawTrafficLight}

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
end; {DrawTextComp}

{ Komponente TSRCheckBox }

constructor TSRCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph:=TBitmap.Create;

  Screen.Cursors[crLinkPoint]:=LoadCursor(HInstance, 'CRSRPOINTTO');
  {Vorgabewerte setzen}
  FAlignment:=taLeftJustify;
  FAllowAllUnchecked:=false;
  FAllowGrayed:=false;
  FAutosize:=false;
  FColor:=clBtnFace;
  FChecked:=false;
  FCheckSize:=13;
  FGrouped:=false;
  FHoverActive:=false;
  FHoverColor:=FColor;
  FHoverCursor:=crLinkPoint;
  FHoverFontColor:=Font.Color;
  FLayout:=clCenter;
  FOldCursor:=Cursor;
  FSpacing:=4;
  FState:=cbUnchecked;
  FTransparent:=false;
  FTranspColor:=FColor;
  FUnderlineOnEnter:=true;
  FWordWrap:=false;
  Width:=90;
  Height:=15;

  FMouseDown:=False;
  AdjustBounds;
end;

destructor  TSRCheckBox.Destroy;
begin
  if assigned(FGlyph) then
    FGlyph.Free;
  inherited Destroy;
end;

procedure TSRCheckBox.AdjustBounds;
var ARect   : TRect;
    AHeight : integer;
begin
  if FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    ARect.Right:=ARect.Right+FCheckSize+FSpacing;
    InflateRect(ARect, 1, 1);
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and
     (FGlyph.Height>(ARect.Bottom-ARect.Top)) then
      AHeight:=FGlyph.Height+1
    else
      AHeight:=ARect.Bottom-ARect.Top;
    SetBounds(Left, Top, ARect.Right, AHeight);
  end;
end;

procedure TSRCheckBox.Change;
begin
  if FChecked and FGrouped then
    UncheckGroupCheckBoxes;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSRCheckBox.CMDialogChar(var Message: TCMDialogChar);
var AState : integer;
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      AState:=ord(FState);
      if (FState<>cbChecked) or not FGrouped or (FGrouped and FAllowAllUnchecked) then
        inc(AState);
      if (FAllowGrayed and (AState=3)) or (not FAllowGrayed and (AState=2)) then
        AState:=0;
      SetState(TCheckBoxState(AState));
      if Enabled and Assigned(FOnClick) then
        FOnClick(Self);
      Result:=1;
    end
    else
      inherited;
  end;
end;

procedure TSRCheckBox.CMEnabledChanged(var Message:TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSRCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TSRCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseOnControl and FHoverActive and Enabled then begin
    FMouseOnControl:=true;
    if FHoverCursor<>Cursor then begin
      FOldCursor:=Cursor;
      Cursor:=FHoverCursor;
    end;
    Invalidate;
  end;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TSRCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseOnControl and FHoverActive and Enabled then begin
    FMouseOnControl:=false;
    if FOldCursor<>Cursor then
      Cursor:=FOldCursor;
    Invalidate;
  end;
  if assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TSRCheckBox.CMTextChanged(var msg: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TSRCheckBox.DblClick;
begin
  if Enabled then
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
end;

procedure TSRCheckBox.DrawGlyph(ACanvas:TCanvas; ARect:TRect);
var ATop,
    AWidth   : integer;
    SrcRect,
    DestRect : TRect;
begin
  if Assigned(FGlyph) and (FNumGlyphs>0) then begin
    { Reihenfolge der Bilder in FGlyph:
      Unchecked, Checked, Disabled, Grayed }
    AWidth:=FGlyph.Width div FNumGlyphs;
    { Quellrechteck ermitteln }
    if Enabled or (FNumGlyphs < 3) then begin
      if FState=cbUnchecked then begin
        { Glyph für Zustand "unchecked" bestimmen }
        SrcRect.Left:=0;
        SrcRect.Right:=AWidth;
      end;
      if (FState=cbChecked) and (FNumGlyphs > 1) then begin
        { Glyph für Zustand "checked" bestimmen }
        SrcRect.Left:=AWidth;
        SrcRect.Right:=SrcRect.Left shl 1;
      end;
      if (FState=cbGrayed) and (FNumGlyphs > 3)  then begin
        { Glyph für Zustand "grayed" bestimmen }
        SrcRect.Left:=AWidth * 3;
        SrcRect.Right:=FGlyph.Width;
      end;
    end
    else begin
      { Glyph für Zustand "disabled" bestimmen }
      SrcRect.Left:=AWidth * 2;
      SrcRect.Right:=AWidth * 3;
    end;
    SrcRect.Top:=0;
    SrcRect.Bottom:=FGlyph.Height;
    { Zielrechteck ermitteln }
    ATop:=ARect.Top+(((ARect.Bottom-ARect.Top)-FGlyph.Height) div 2);
    DestRect:=Rect(ARect.Left, ATop, ARect.Left+AWidth, ATop+FGlyph.Height);
    { Bitmap transparent zeichnen }
    ACanvas.BrushCopy(DestRect, FGlyph, SrcRect, FTranspColor);
  end;
end;

function TSRCheckBox.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result:=ARect;
  AText:=Caption;
  if (AText='') or ((AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  try
    Canvas.Handle:=DC;
    Canvas.Font:=Font;
    DrawTextComp(Canvas, AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  finally
    Canvas.Handle:=OldHandle;
    ReleaseDC(0, DC);
  end;
end;

procedure TSRCheckBox.Loaded;
begin
  inherited Loaded;

  if Assigned(FGlyph) then
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
end;

procedure TSRCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    if Button=mbLeft then begin
      FMouseDown:=true;
      if FState=cbUnChecked then begin
        SetState(cbChecked);
        FStateChanged:=true;
        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
    end;
    if Assigned(FOnMouseDown) then
      FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TSRCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled and FMouseDown then begin
    { State-Eigenschaft berechnen }
    if not FStateChanged then begin
      if FState=cbChecked then begin
        if not FGrouped or (FGrouped and FAllowAllUnchecked) then begin
          if FAllowGrayed then
            SetState(cbGrayed)
          else
            SetState(cbUnChecked);
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end
      else begin
        if FState=cbGrayed then begin
          SetState(cbUnChecked);
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end;
    end;
    FStateChanged:=false;

    { OnClick-Ereignis abfeuern }
    if Assigned(FOnMouseUp) then
      FOnMouseUp(Self, Button, Shift, X, Y);
  end;
  FMouseDown:=false;
end;

procedure TSRCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TSRCheckBox.Paint;
var ARect : TRect;
begin
  { Hintergrund zeichnen }
  with Canvas do begin
    if FTransparent and (not FMouseOnControl or not FHoverActive) then
      Brush.Style:=bsClear
    else begin
      Brush.Style:=bsSolid;
      if FHoverActive and FMouseOnControl then
        Brush.Color:=FHoverColor
      else
        Brush.Color:=FColor;
      ARect:=GetClientRect;
      FillRect(ARect);
    end;
  end;

  { Den Rest zeichnen }
  PaintButton;
  PaintCaption;
end;

procedure TSRCheckBox.PaintButton;
var ARect : TRect;
begin
  { Ausgaberechteck für Checkbox ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Right:=ARect.Left+FCheckSize
  else
    ARect.Left:=ARect.Right-FCheckSize;
  if FLayout=clCenter then
    ARect.Top:=(ARect.Bottom-ARect.Top-FCheckSize) div 2;
  if FLayout=clBottom then
    ARect.Top:=ARect.Bottom-FCheckSize;
  ARect.Bottom:=ARect.Top+FCheckSize;

  { Checkbox zeichnen }
  case FStyle of
    csCheckBox     : DrawCheckBox(Canvas, ARect, FState, Enabled);
    csBitmap       : DrawGlyph(Canvas, ARect);
    csDiamond      : DrawDiamond(Canvas, ARect, FState, Enabled);
    csPushButton   : DrawPushButton(Canvas, ARect, FState, Enabled);
    csRadioButton  : DrawRadioButton(Canvas, ARect, FState, Enabled);
    csTrafficLight : DrawTrafficLight(Canvas, ARect, FState, Enabled);
  end;
end;

procedure TSRCheckBox.PaintCaption;
const
  Alignments: array[TAlignment] of Word =
   (DT_Left, DT_Right, DT_Center);
  WordWraps: array[Boolean] of Word =
   (0, DT_WordBreak);
  Lines: array[Boolean] of Word =
   (DT_SingleLine, 0);
var ARect     : TRect;
    DrawStyle : Integer;
begin
  { Ausgaberechteck für Caption ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Left:=ARect.Left+FCheckSize+FSpacing
  else
    ARect.Right:=ARect.Right-FCheckSize-FSpacing;

  { Caption zeichnen }
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style:=bsClear;
  if FHoverActive and FMouseOnControl then begin
    Canvas.Font.Color:=FHoverFontColor;
    if FUnderlineOnEnter then
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderLine];
  end;
  if not Enabled then begin
    Canvas.Font.Color:=clBtnHighlight;
    OffsetRect(ARect, 1, 1);
  end;
  DrawStyle:=DT_ExpandTabs or DT_VCenter or Lines[FWordWrap] or WordWraps[FWordWrap] or Alignments[FAlignment];
  DrawTextComp(Canvas, Caption, ARect, DrawStyle);
  if not Enabled then begin
    Canvas.Font.Color:=clInactiveCaption;
    OffsetRect(ARect, -1, -1);
    DrawTextComp(Canvas, Caption, ARect, DrawStyle);
  end;
  Canvas.Brush.Style:=bsSolid;
end;

procedure TSRCheckBox.SetAlignment(newValue: TLeftRight);
begin
  if FAlignment<>newValue then begin
    FAlignment:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetAutosize(newValue: boolean);
begin
  if FAutosize<>newValue then begin
    FAutosize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetColor(newValue: TColor);
begin
  if FColor<>newValue then begin
    FColor:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetChecked(newValue: boolean);
begin
  if FChecked<>newValue then begin
    FChecked:=newValue;
    if FChecked then
      SetState(cbChecked)
    else
      SetState(cbUnChecked);
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetCheckSize(newValue: integer);
begin
  if FCheckSize<>newValue then begin
    FCheckSize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetGlyph(newValue: TBitmap);
begin
  FGlyph.Assign(newValue);
  if Assigned(newValue) then begin
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
    if (csDesigning in ComponentState) then begin
      { Glyph 0: Unchecked, 1: Checked, 2: Disabled, 3: Grayed
        Muß die Ausmaße (Height * NumGlyphs) = Width  haben }
      if (newValue.Width mod newValue.Height = 0) then
        FNumGlyphs:=newValue.Width div newValue.Height
      else
        FNumGlyphs:=1;
    end;
    if (FStyle=csBitmap) and (FAutoSize) then
      SetCheckSize(FGlyph.Width div FNumGlyphs);
  end
  else
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
  Invalidate;
end;

procedure TSRCheckBox.SetGrouped(newValue: boolean);
begin
  if FGrouped<>newValue then begin
    FGrouped:=newValue;
    if FGrouped and FChecked then
      UncheckGroupCheckBoxes;
  end;
end;

procedure TSRCheckBox.SetHoverActive(newValue: boolean);
begin
  if FHoverActive<>newValue then begin
    FHoverActive:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRCheckBox.SetHoverColor(newValue: TColor);
begin
  if FHoverColor<>newValue then begin
    FHoverColor:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRCheckBox.SetHoverFontColor(newValue: TColor);
begin
  if FHoverFontColor<>newValue then begin
    FHoverFontColor:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRCheckBox.SetLayout(newValue: TCheckboxLayout);
begin
  if FLayout<>newValue then begin
    FLayout:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetNumGlyphs(newValue: TNumGlyphs);
begin
  if FNumGlyphs<>newValue then begin
    FNumGlyphs:=newValue;
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and FAutoSize then
      SetCheckSize(FGlyph.Width div FNumGlyphs);
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetSpacing(newValue: integer);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetState(newValue: TCheckBoxState);
begin
  if FState<>newValue then begin
    FState:=newValue;
    if FState=cbChecked then
      FChecked:=true;
    if FState=cbUnChecked then
      FChecked:=false;
    Change;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetStyle(newValue: TCheckStyle);
begin
  if FStyle<>newValue then begin
    FStyle:=newValue;
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and FAutoSize then 
      SetCheckSize(FGlyph.Width div FNumGlyphs);
    if (FStyle=csRadioButton) and not FGrouped then
      FGrouped:=true;
    if (FStyle=csCheckbox) and FGrouped then
      FGrouped:=false;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetTransparent(newValue: boolean);
begin
  if FTransparent<>newValue then begin
    FTransparent:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetUnderlineOnEnter(newValue: boolean);
begin
  if FUnderlineOnEnter<>newValue then begin
    FUnderlineOnEnter:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRCheckBox.SetWordWrap(newValue: boolean);
begin
  if FWordWrap<>newValue then begin
    FWordWrap:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.UncheckGroupCheckBoxes;
var i : integer;
begin
  for i:=0 to Self.Parent.ControlCount-1 do
    if Self.Parent.Controls[i] is TSRCheckBox then
      if (TSRCheckBox(Self.Parent.Controls[i])<>Self)
       and TSRCheckBox(Self.Parent.Controls[i]).Grouped
       and TSRCheckBox(Self.Parent.Controls[i]).Checked then
         TSRCheckBox(Self.Parent.Controls[i]).Checked:=false;
end;

{ Komponente TEnhancedCheckBox }

constructor TEnhancedCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph:=TBitmap.Create;

  Screen.Cursors[crLinkPoint]:=LoadCursor(HInstance, 'CRSRPOINTTO');
  {Vorgabewerte setzen}
  FAlignment:=taLeftJustify;
  FAllowAllUnchecked:=false;
  FAllowGrayed:=false;
  FAutosize:=false;
  FColor:=clBtnFace;
  FChecked:=false;
  FCheckSize:=13;
  FGrouped:=false;
  FHoverActive:=false;
  FHoverColor:=FColor;
  FHoverCursor:=crLinkPoint;
  FHoverFontColor:=Font.Color;
  FLayout:=clCenter;
  FSpacing:=4;
  FState:=cbUnchecked;
  FTranspColor:=FColor;
  FUnderlineOnEnter:=true;
  FWordWrap:=false;
  Width:=90;
  Height:=15;

  FMouseDown:=False;
  AdjustBounds;
end;

destructor TEnhancedCheckBox.Destroy;
begin
  if assigned(FGlyph) then
    FGlyph.Free;
  inherited Destroy;
end;

procedure TEnhancedCheckBox.AdjustBounds;
var ARect   : TRect;
    AHeight : integer;
begin
  if FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    ARect.Right:=ARect.Right+FCheckSize+FSpacing;
    InflateRect(ARect, 1, 1);
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and
     (FGlyph.Height>(ARect.Bottom-ARect.Top)) then
      AHeight:=FGlyph.Height+1
    else
      AHeight:=ARect.Bottom-ARect.Top;
    SetBounds(Left, Top, ARect.Right, AHeight);
  end;
end;

procedure TEnhancedCheckBox.Change;
begin
  if FChecked and FGrouped then
    UncheckGroupCheckBoxes;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEnhancedCheckBox.CMDialogChar(var Message: TCMDialogChar);
var AState : integer;
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      AState:=ord(FState);
      inc(AState);
      if (FAllowGrayed and (AState=3)) or (not FAllowGrayed and (AState=2)) then
        AState:=0;
      SetState(TCheckBoxState(AState));
      if Enabled and Assigned(FOnClick) then
        FOnClick(Self);
      Result:=1;
    end
    else
      inherited;
  end;
end;

procedure TEnhancedCheckBox.CMEnabledChanged(var Message:TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TEnhancedCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TEnhancedCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseOnControl and FHoverActive and Enabled then begin
    FMouseOnControl:=true;
    if FHoverCursor<>Cursor then begin
      FOldCursor:=Cursor;
      Cursor:=FHoverCursor;
    end;
    Invalidate;
  end;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TEnhancedCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseOnControl and FHoverActive and Enabled then begin
    FMouseOnControl:=false;
    if FOldCursor<>Cursor then
      Cursor:=FOldCursor;
    Invalidate;
  end;
  if assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TEnhancedCheckBox.CMTextChanged(var msg: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TEnhancedCheckBox.DblClick;
begin
  if Enabled then
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
end;

procedure TEnhancedCheckBox.DoDrawFocusRect(IsFocused:boolean);
var CRect : TRect;
begin
  CRect:=ClientRect;
  if FAlignment=taLeftJustify then
    CRect.Left:=CRect.Left+FCheckSize+FSpacing-1
  else
    CRect.Right:=CRect.Right-FCheckSize-FSpacing+1;
  with Canvas do begin
    if IsFocused then begin
      Brush.Color:=clBlack;
      DrawDotLine(CRect.Left+1, CRect.Top, CRect.Right-CRect.Left-2, true);
      DrawDotLine(CRect.Right-1, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
      DrawDotLine(CRect.Left+1, CRect.Bottom-1, CRect.Right-CRect.Left-2, true);
      DrawDotLine(CRect.Left, CRect.Top+1, CRect.Bottom-CRect.Top-2, false);
    end
    else begin
      Brush.Color:=Color;
      FrameRect(CRect);
    end;
  end;
end;

procedure TEnhancedCheckBox.DrawDotLine(X1,Y1,Length:integer;Horizontal:boolean);
var i : integer;
begin
  with Canvas do begin
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

procedure TEnhancedCheckBox.DrawGlyph(ACanvas:TCanvas; ARect:TRect);
var ATop,
    AWidth   : integer;
    SrcRect,
    DestRect : TRect;
begin
  if Assigned(FGlyph) and (FNumGlyphs>0) then begin
    { Reihenfolge der Bilder in FGlyph:
      Unchecked, Checked, Disabled, Grayed }
    AWidth:=FGlyph.Width div FNumGlyphs;
    { Quellrechteck ermitteln }
    if Enabled or (FNumGlyphs < 3) then begin
      if FState=cbUnchecked then begin
        { Glyph für Zustand "unchecked" bestimmen }
        SrcRect.Left:=0;
        SrcRect.Right:=AWidth;
      end;
      if (FState=cbChecked) and (FNumGlyphs > 1) then begin
        { Glyph für Zustand "checked" bestimmen }
        SrcRect.Left:=AWidth;
        SrcRect.Right:=SrcRect.Left shl 1;
      end;
      if (FState=cbGrayed) and (FNumGlyphs > 3)  then begin
        { Glyph für Zustand "grayed" bestimmen }
        SrcRect.Left:=AWidth * 3;
        SrcRect.Right:=FGlyph.Width;
      end;
    end
    else begin
      { Glyph für Zustand "disabled" bestimmen }
      SrcRect.Left:=AWidth * 2;
      SrcRect.Right:=AWidth * 3;
    end;
    SrcRect.Top:=0;
    SrcRect.Bottom:=FGlyph.Height;
    { Zielrechteck ermitteln }
    ATop:=ARect.Top+(((ARect.Bottom-ARect.Top)-FGlyph.Height) div 2);
    DestRect:=Rect(ARect.Left, ATop, ARect.Left+AWidth, ATop+FGlyph.Height);
    { Bitmap transparent zeichnen }
    ACanvas.BrushCopy(DestRect, FGlyph, SrcRect, FTranspColor);
  end;
end;

function TEnhancedCheckBox.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result:=ARect;
  AText:=Caption;
  if (AText='') or ((AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  try
    Canvas.Handle:=DC;
    Canvas.Font:=Font;
    DrawTextComp(Canvas, AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  finally
    Canvas.Handle:=OldHandle;
    ReleaseDC(0, DC);
  end;
end;

procedure TEnhancedCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Enabled then begin
    if Key=VK_Space then begin
      if FState=cbUnChecked then begin
        SetState(cbChecked);
        FStateChanged:=true;
        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TEnhancedCheckBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Enabled then begin
    if Key=VK_Space then begin
      { State-Eigenschaft berechnen }
      if not FStateChanged then begin
        if FState=cbChecked then begin
          if not FGrouped or (FGrouped and FAllowAllUnchecked) then begin
            if FAllowGrayed then
              SetState(cbGrayed)
            else
              SetState(cbUnChecked);
            if Assigned(FOnClick) then
              FOnClick(Self);
          end;
        end
        else begin
          if FState=cbGrayed then begin
            SetState(cbUnChecked);
            if Assigned(FOnClick) then
              FOnClick(Self);
          end;
        end;
      end;
      FStateChanged:=false;
    end;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TEnhancedCheckBox.Loaded;
begin
  inherited Loaded;

  if Assigned(FGlyph) then
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
end;

procedure TEnhancedCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    if not Focused then
      SetFocus;
    if Button=mbLeft then begin
      FMouseDown:=true;
      if FState=cbUnChecked then begin
        SetState(cbChecked);
        FStateChanged:=true;
        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
    end;
    if Assigned(FOnMouseDown) then
      FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TEnhancedCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled and FMouseDown then begin
    { State-Eigenschaft berechnen }
    if not FStateChanged then begin
      if FState=cbChecked then begin
        if not FGrouped or (FGrouped and FAllowAllUnchecked) then begin
          if FAllowGrayed then
            SetState(cbGrayed)
          else
            SetState(cbUnChecked);
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end
      else begin
        if FState=cbGrayed then begin
          SetState(cbUnChecked);
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end;
    end;
    FStateChanged:=false;

    { OnClick-Ereignis abfeuern }
    if Assigned(FOnMouseUp) then
      FOnMouseUp(Self, Button, Shift, X, Y);
  end;
  FMouseDown:=false;
end;

procedure TEnhancedCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TEnhancedCheckBox.Paint;
var ARect : TRect;
begin
  { Hintergrund zeichnen }
  with Canvas do begin
    Brush.Style:=bsSolid;
    if FHoverActive and FMouseOnControl then
      Brush.Color:=FHoverColor
    else
      Brush.Color:=FColor;
    ARect:=GetClientRect;
    FillRect(ARect);
  end;

  { Den Rest zeichnen }
  PaintButton;
  PaintCaption;
  DoDrawFocusRect(Focused);
end;

procedure TEnhancedCheckBox.PaintButton;
var ARect : TRect;
begin
  { Ausgaberechteck für Checkbox ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Right:=ARect.Left+FCheckSize
  else
    ARect.Left:=ARect.Right-FCheckSize;
  if FLayout=clCenter then
    ARect.Top:=(ARect.Bottom-ARect.Top-FCheckSize) div 2;
  if FLayout=clBottom then
    ARect.Top:=ARect.Bottom-FCheckSize;
  ARect.Bottom:=ARect.Top+FCheckSize;

  { Checkbox zeichnen }
  case FStyle of
    csCheckBox     : DrawCheckBox(Canvas, ARect, FState, Enabled);
    csBitmap       : DrawGlyph(Canvas, ARect);
    csDiamond      : DrawDiamond(Canvas, ARect, FState, Enabled);
    csPushButton   : DrawPushButton(Canvas, ARect, FState, Enabled);
    csRadioButton  : DrawRadioButton(Canvas, ARect, FState, Enabled);
    csTrafficLight : DrawTrafficLight(Canvas, ARect, FState, Enabled);
  end;
end;

procedure TEnhancedCheckBox.PaintCaption;
const
  Alignments: array[TAlignment] of Word =
   (DT_Left, DT_Right, DT_Center);
  WordWraps: array[Boolean] of Word =
   (0, DT_WordBreak);
  Lines: array[Boolean] of Word =
   (DT_SingleLine, 0);
var ARect     : TRect;
    DrawStyle : Integer;
begin
  { Ausgaberechteck für Caption ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Left:=ARect.Left+FCheckSize+FSpacing
  else
    ARect.Right:=ARect.Right-FCheckSize-FSpacing;

  { Caption zeichnen }
  Canvas.Font.Assign(Font);
  Canvas.Brush.Style:=bsClear;
  if FHoverActive and FMouseOnControl then begin
    Canvas.Font.Color:=FHoverFontColor;
    if FUnderlineOnEnter then
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderLine];
  end;
  if not Enabled then begin
    Canvas.Font.Color:=clBtnHighlight;
    OffsetRect(ARect, 1, 1);
  end;
  DrawStyle:=DT_ExpandTabs or DT_VCenter or Lines[FWordWrap] or WordWraps[FWordWrap] or Alignments[FAlignment];
  DrawTextComp(Canvas, Caption, ARect, DrawStyle);
  if not Enabled then begin
    Canvas.Font.Color:=clInactiveCaption;
    OffsetRect(ARect, -1, -1);
    DrawTextComp(Canvas, Caption, ARect, DrawStyle);
  end;
  Canvas.Brush.Style:=bsSolid;
end;

procedure TEnhancedCheckBox.SetAlignment(newValue: TLeftRight);
begin
  if FAlignment<>newValue then begin
    FAlignment:=newValue;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetAutosize(newValue: boolean);
begin
  if FAutosize<>newValue then begin
    FAutosize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetColor(newValue: TColor);
begin
  if FColor<>newValue then begin
    FColor:=newValue;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetChecked(newValue: boolean);
begin
  if FChecked<>newValue then begin
    FChecked:=newValue;
    if FChecked then
      SetState(cbChecked)
    else
      SetState(cbUnChecked);
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetCheckSize(newValue: integer);
begin
  if FCheckSize<>newValue then begin
    FCheckSize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetGlyph(newValue: TBitmap);
begin
  FGlyph.Assign(newValue);
  if Assigned(newValue) then begin
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
    if (csDesigning in ComponentState) then begin
      { Glyph 1: Normal, 2: Disabled, 3: Down;
        Muß die Ausmaße (Height * NumGlyphs) = Width  haben }
      if (newValue.Width mod newValue.Height = 0) then
        FNumGlyphs:=newValue.Width div newValue.Height
      else
        FNumGlyphs:=1;
    end;
    if (FStyle=csBitmap) and FAutoSize then 
      SetCheckSize(FGlyph.Width div FNumGlyphs);
  end
  else
    FTranspColor:=FGlyph.Canvas.Pixels[0, FGlyph.Height-1];
  Invalidate;
end;

procedure TEnhancedCheckBox.SetGrouped(newValue: boolean);
begin
  if FGrouped<>newValue then begin
    FGrouped:=newValue;
    if FGrouped and FChecked then
      UncheckGroupCheckBoxes;
  end;
end;

procedure TEnhancedCheckBox.SetHoverActive(newValue: boolean);
begin
  if FHoverActive<>newValue then begin
    FHoverActive:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetHoverColor(newValue: TColor);
begin
  if FHoverColor<>newValue then begin
    FHoverColor:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetHoverFontColor(newValue: TColor);
begin
  if FHoverFontColor<>newValue then begin
    FHoverFontColor:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetLayout(newValue: TCheckboxLayout);
begin
  if FLayout<>newValue then begin
    FLayout:=newValue;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetNumGlyphs(newValue: TNumGlyphs);
begin
  if FNumGlyphs<>newValue then begin
    FNumGlyphs:= newValue;
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and FAutoSize then 
      SetCheckSize(FGlyph.Width div FNumGlyphs);
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetSpacing(newValue: integer);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetState(newValue: TCheckBoxState);
begin
  if FState<>newValue then begin
    FState:=newValue;
    if FState=cbChecked then
      FChecked:=true;
    if FState=cbUnChecked then
      FChecked:=false;
    Change;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetStyle(newValue: TCheckStyle);
begin
  if FStyle<>newValue then begin
    FStyle:=newValue;
    if (FStyle=csBitmap) and (FNumGlyphs>0) and assigned(FGlyph) and FAutoSize then
      SetCheckSize(FGlyph.Width div FNumGlyphs);
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetUnderlineOnEnter(newValue: boolean);
begin
  if FUnderlineOnEnter<>newValue then begin
    FUnderlineOnEnter:=newValue;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TEnhancedCheckBox.SetWordWrap(newValue: boolean);
begin
  if FWordWrap<>newValue then begin
    FWordWrap:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TEnhancedCheckBox.UncheckGroupCheckBoxes;
var i : integer;
begin
  for i:=0 to Self.Parent.ControlCount-1 do
    if Self.Parent.Controls[i] is TEnhancedCheckBox then
      if (TEnhancedCheckBox(Self.Parent.Controls[i])<>Self)
       and TEnhancedCheckBox(Self.Parent.Controls[i]).Grouped
       and TEnhancedCheckBox(Self.Parent.Controls[i]).Checked then
         TEnhancedCheckBox(Self.Parent.Controls[i]).Checked:=false;
end;

procedure TEnhancedCheckBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  DoDrawFocusRect(false);
end;

procedure TEnhancedCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  DoDrawFocusRect(true);
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRCheckBox, TEnhancedCheckBox]);
end;

end.
