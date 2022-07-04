unit SRLabel;

{ TSRLabel (C)opyright 2005 Version 2.00
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponente ist eine Label-Komponente mit Schatteneffekt und
  Internet-Link-Funktion. Sie ist abgeleitet von TGraphicControl und
  sie ist Public Domain, das Urheberrecht liegt aber beim Autor.

  Vielen Dank an Robert Rossmair für die rrColors-Unit! }

interface

{$I SRDefine.inc}

uses {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, SysUtils, {$ENDIF}
     Messages, Classes, Controls, Graphics, ExtCtrls, StdCtrls, Forms;

type
  THighLightPos = (hpTopLeft, hpTopRight);
  TLabelBevel = (bvFrame, bvLowered, bvNone, bvRaised, bvSingle);
  TLabelStyle = (ls3DText, lsCustom, lsLink, lsPath, lsWindow);
  TLinkType = (ltEMail, ltNews, ltNone, ltWWW);
  {$IFDEF SR_Delphi2_Up}
  TTextDirection = (tdRight, tdLeft, tdDown, tdUp);
  {$ENDIF}
  {$IFNDEF SR_Delphi3_Up}
  TTextLayout = (tlTop, tlCenter, tlBottom);
  {$ENDIF}

  TSRLabel = class(TGraphicControl)
  private
    FAlignment         : TAlignment;
    FAutoSize          : boolean;
    FBevelStyle        : TLabelBevel;
    FBevelWidth,
    FBorderWidth       : word;
    FColor             : TColor;
    {$IFDEF SR_Delphi2_Up}
    FDirection         : TTextDirection;
    {$ENDIF}
    FFocusControl      : TWinControl;
    FHighlightColor    : TColor;
    FHighlightOffset   : word;
    FHighlightPos      : THighlightPos;
    FHoverColor,
    FHoverFontColor    : TColor;
    FLayout            : TTextLayout;
    FLinkActive        : boolean;
    FLinkedAdress      : string;
    FLinkType          : TLinkType;
    FMouseOnControl    : boolean;
    FOldCursor         : TCursor;
    FShadowColor       : TColor;
    FShadowOffset      : word;
    FShortenFilenames  : boolean;
    FShowAccelChar,
    FShowHighlight,
    FShowShadow        : boolean;
    FStyle             : TLabelStyle;
    FStyleChange,
    FTrueType,
    FUnderlineOnEnter  : boolean;
    FWordWrap          : boolean;

    FOnClick,
    FOnExecuteLink,
    FOnMouseEnter,
    FOnMouseExit       : TNotifyEvent;

    procedure AdjustBounds;
    procedure DrawTextComp(AText:string; var ARect:TRect;const AFormat:Word{$IFDEF SR_Delphi2_Up};
                           const ADirection:TTextDirection{$ENDIF});
    procedure GetTextAndTextRect(var AText:string;var ARect:TRect);
    function GetTextRect(ARect: TRect): TRect;
    function GetTransparent: Boolean;
    procedure PaintBevel(ARect: TRect);
    procedure PaintText(ARect: TRect; AFlags: Word);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevelStyle(Value: TLabelBevel);
    procedure SetBevelWidth(Value: word);
    procedure SetBorderWidth(Value: word);
    procedure SetColor(Value: TColor);
    {$IFDEF SR_Delphi2_Up}
    procedure SetDirection(Value: TTextDirection);
    procedure SetFocusControl(Value: TWinControl);
    {$ENDIF}
    procedure SetHighlightColor(Value: TColor);
    procedure SetHighlightOffset(Value: word);
    procedure SetHighlightPos(Value: THighlightPos);
    procedure SetHoverColor(Value: TColor);
    procedure SetHoverFontColor(Value: TColor);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowOffset(Value: word);
    procedure SetShortenFilenames(Value: Boolean);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetShowHighlight(Value: Boolean);
    procedure SetShowShadow(Value: Boolean);
    procedure SetStyle(Value: TLabelStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetUnderlineOnEnter(Value: boolean);
    procedure SetWordWrap(Value: Boolean);

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

  protected
    procedure Click; override;
    procedure ExecuteLink; dynamic;
    function GetLabelText: string; virtual;
    function IsFontTrueType:boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;

  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BevelStyle: TLabelBevel read FBevelStyle write SetBevelStyle;
    property BevelWidth: word read FBevelWidth write SetBevelWidth;
    property BorderWidth: word read FBorderWidth write SetBorderWidth;
    property Caption;
    property Color: TColor read FColor write SetColor;
    {$IFDEF SR_Delphi4_Up}
    property Constraints;
    {$ENDIF}
    property Cursor;
    {$IFDEF SR_Delphi2_Up}
    property Direction: TTextDirection read FDirection write SetDirection;
    {$ENDIF}
    {$IFDEF SR_Delphi4_Up}
    property DragKind;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi2_Up}
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    {$ELSE}
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    {$ENDIF}
    property Font;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clBtnHighlight;
    property HighlightOffset: word read FHighlightOffset write SetHighlightOffset default 1;
    property HighlightPos: THighlightPos read FHighlightPos write SetHighlightPos default hpTopLeft;
    property HoverColor: TColor read FHoverColor write SetHoverColor;
    property HoverFontColor: TColor read FHoverFontColor write SetHoverFontColor;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LinkActive: boolean read FLinkActive write FLinkActive;
    property LinkedAdress: string read FLinkedAdress write FLinkedAdress;
    property LinkType: TLinkType read FLinkType write FLinkType;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property ShadowOffset: word read FShadowOffset write SetShadowOffset default 1;
    property ShortenFilenames: Boolean read FShortenFilenames write SetShortenFilenames;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowHighlight: Boolean read FShowHighlight write SetShowHighlight;
    property ShowHint;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow;
    property Style: TLabelStyle read FStyle write SetStyle;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property UnderlineOnEnter: Boolean read FUnderlineOnEnter write SetUnderlineOnEnter default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {$IFDEF SR_Delphi5_Up}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    {$IFDEF SR_Delphi4_Up}
    property OnEndDock;
    {$ENDIF}
    property OnExecuteLink: TNotifyEvent read FOnExecuteLink write FOnExecuteLink;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
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
uses ShellAPI, SysUtils, FileCtrl, rrColors;
{$ELSE}
{$R *.D16}
{$R *.R16}
uses ShellAPI, FileCtrl;
{$ENDIF}

const
  HLFactor    = 0.65;
  HLContrast  = 5;
  ShContrast  = 4;
  crLinkPoint = TCursor(-30);

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  {Rot}
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
  {Grün}
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  {Blau}
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
end;
{$ENDIF}

procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  {$IFDEF SR_Delphi1}
  HighlightColor:=ChangeBrightness(FaceColor,100 div 10*HLContrast);
  ShadowColor:=ChangeBrightness(FaceColor,-100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FaceColor,HighlightColor,ShadowColor,(10-HLContrast)/10,(10-ShContrast)/10);
  {$ENDIF}
end; {AssignBevelColors}

{$IFDEF SR_Delphi2_Up}
procedure DrawRotatedText(ACanvas:TCanvas;const X,Y:integer;const Angle:word;const AText:string);
var
  lf       : TLogFont;
  OldFont,
  NewFont  : hFont;
  PpIVert  : integer;
begin
  PpIVert:=GetDeviceCaps(ACanvas.Handle, LOGPIXELSY);
  ACanvas.Font.PixelsPerInch:=PpIVert;
  GetObject(ACanvas.Font.Handle, SizeOf(lf), @lf);
  lf.lfEscapement := Angle*10;
  lf.lfOrientation := Angle*10;
  NewFont := CreateFontIndirect(lf);
  OldFont := SelectObject(ACanvas.Handle, NewFont);
  Windows.TextOut(ACanvas.Handle, X, Y, PChar(AText), length(AText));
  SelectObject(ACanvas.Handle, OldFont);
  DeleteObject(NewFont);
end; {DrawRotatedText}
{$ENDIF}

constructor TSRLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Screen.Cursors[crLinkPoint]:=LoadCursor(HInstance, 'CRSRLINKPOINT');
  {$IFDEF SR_Delphi2_Up}
  ControlStyle:=ControlStyle+[csOpaque, csReplicatable];
  FAutoSize:=True;
  {$ELSE}
  ControlStyle:=ControlStyle+[csOpaque];
  FAutoSize:=False;
  {$ENDIF}
  Width:=65;
  Height:=17;

  FBevelStyle:=bvNone;
  FBevelWidth:=1;
  FBorderWidth:=0;
  FColor:=clBtnFace;
  {$IFDEF SR_Delphi2_Up}
  FDirection:=tdRight;
  {$ENDIF}
  FHighlightColor:=clBtnHighlight;
  FHighlightOffset:=1;
  FHighlightPos:=hpTopLeft;
  FHoverColor:=FColor;
  FHoverFontColor:=Font.Color;
  FLayout:=tlTop;
  FLinkType:=ltWWW;
  FLinkedAdress:='http://www.picsoft.de';
  FLinkActive:=false;
  FOldCursor:=Cursor;
  FShadowColor:=clBtnShadow;
  FShadowOffset:=1;
  FShortenFilenames:=false;
  FShowAccelChar:=True;
  FShowHighlight:=False;
  FShowShadow:=False;
  FStyle:=lsCustom;
  FUnderlineOnEnter:=true;
end;

procedure TSRLabel.AdjustBounds;
var Buf   : Integer;
    ARect,
    BRect : TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    Buf:=0;
    if FShowHighlight then
      Buf:=Buf+FHighlightOffset;
    if FShowShadow then
      Buf:=Buf+FShadowOffset;
    if FBevelStyle<>bvNone then
      Buf:=FBevelWidth*2;
    if FBevelStyle=bvSingle then
      Buf:=Buf+1;
    if FBorderWidth>0 then
      Buf:=Buf+(FBorderWidth*2);
    BRect:=Bounds(Left, Top, ARect.Right+Buf, ARect.Bottom+Buf);
    {$IFDEF SR_Delphi2_Up}
    if (FDirection=tdUp) or (FDirection=tdDown) then begin
      Buf:=BRect.Right-BRect.Left;
      BRect.Right:=BRect.Left+(BRect.Bottom-BRect.Top);
      BRect.Bottom:=BRect.Top+Buf;
    end;
    {$ENDIF}
    SetBounds(BRect.Left, BRect.Top, BRect.Right-BRect.Left, BRect.Bottom-BRect.Top);
  end;
end;

procedure TSRLabel.Click;
var URL   : string;
{$IFDEF SR_Delphi1}
    CText : PChar;
{$ENDIF}
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
  if Enabled and FLinkActive and (FLinkType<>ltNone) and (FLinkedAdress<>'') then begin
    case FLinkType of
      ltEMail : URL:='mailto:'+FLinkedAdress;
      ltNews  : URL:='news:'+FLinkedAdress;
      else URL:=FLinkedAdress;
    end;
    ExecuteLink;
    {$IFDEF SR_Delphi1}
    CText:=StrAlloc(255);
    StrPCopy(CText, URL);
    ShellExecute(0, 'open', CText, nil, nil, SW_ShowNormal);
    StrDispose(CText);
    {$ELSE}
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_ShowNormal);
    {$ENDIF}
  end;
end;

procedure TSRLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
   IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then begin
        SetFocus;
        Message.Result:=1;
      end;
end;

procedure TSRLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  {$IFDEF SR_Delphi2_Up}
  FTrueType:=IsFontTrueType;
{  if not (csLoading in ComponentState) and not FTrueType and (FDirection<>tdRight) then
    SetDirection(tdRight);}
  {$ENDIF}
  AdjustBounds;
end;

procedure TSRLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseOnControl and FLinkActive and Enabled then begin
    FMouseOnControl:=true;
    FOldCursor:=Cursor;
    Cursor:=crLinkPoint;
    Invalidate;
  end;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TSRLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseOnControl and FLinkActive and Enabled then begin
    FMouseOnControl:=false;
    Cursor:=FOldCursor;
    Invalidate;
  end;
  if assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TSRLabel.CMTextChanged(var Message: TMessage);
begin
  AdjustBounds;
  Invalidate;
end;

procedure TSRLabel.DrawTextComp(AText:string; var ARect:TRect;const AFormat:Word{$IFDEF SR_Delphi2_Up};
                                const ADirection:TTextDirection{$ENDIF});
{$IFDEF SR_Delphi1}
var CText : PChar;
{$ENDIF}
begin
  {$IFDEF SR_Delphi1}
  CText:=StrAlloc(255);
  StrPCopy(CText, AText);
  DrawText(Canvas.Handle, CText, StrLen(CText), ARect, AFormat);
  StrDispose(CText);
  {$ELSE}
  case ADirection of
    tdLeft :
      DrawRotatedText(Canvas, ARect.Left+Canvas.TextWidth(AText),
                      ARect.Top+Canvas.TextHeight(AText), 180, AText);
    tdDown :
      DrawRotatedText(Canvas, ARect.Left+Canvas.TextHeight(AText),
                      ARect.Top, 270, AText);
    tdUp :
      DrawRotatedText(Canvas, ARect.Left,
                      ARect.Top+Canvas.TextWidth(AText), 90, AText);
    else
      DrawText(Canvas.Handle, PChar(AText), Length(AText), ARect, AFormat);
  end;
  {$ENDIF}
end;

procedure TSRLabel.ExecuteLink;
begin
  if Assigned(FOnExecuteLink) then
    FOnExecuteLink(Self);
end;

function TSRLabel.GetLabelText: string;
begin
  if FShortenFilenames then begin
    Result:=MinimizeName(Caption, Canvas, ClientWidth-10);
  end
  else
    Result:=Caption;
end;

procedure TSRLabel.GetTextAndTextRect(var AText:string;var ARect:TRect);
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var DC        : HDC;
    OutText   : string;
    OldHandle : THandle;
begin
  AText:=GetLabelText;
  {$IFDEF SR_Delphi2_Up}
  if (FDirection=tdLeft) or (FDirection=tdRight) then begin
  {$ENDIF}
    OutText:=AText;
    if (OutText='') or (FShowAccelChar and (OutText[1]='&') and (OutText[2]=#0)) then
      OutText:=OutText+' ';
    DC:=GetDC(0);
    OldHandle:=Canvas.Handle;
    Canvas.Handle:=DC;
    Canvas.Font:=Font;
    DrawTextComp(OutText, ARect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]{$IFDEF SR_Delphi2_Up},
                 tdRight{$ENDIF});
    Canvas.Handle:=OldHandle;
    ReleaseDC(0, DC);
  {$IFDEF SR_Delphi2_Up}
  end;
  {$ENDIF}
end;

function TSRLabel.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result.Left:=ARect.Left;
  Result.Top:=ARect.Top;
  AText:=GetLabelText;
  if (AText='') or (FShowAccelChar and (AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  if FWordWrap then
    Result.Right:=Result.Left+(ARect.Right-ARect.Left);
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font:=Font;
  DrawTextComp(AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]{$IFDEF SR_Delphi2_Up}, tdRight{$ENDIF});
  Canvas.Handle:=OldHandle;
  ReleaseDC(0, DC);
end;

function TSRLabel.GetTransparent: Boolean;
begin
  Result:=not (csOpaque in ControlStyle);
end;

function TSRLabel.IsFontTrueType:boolean;
var
  Metrics : TTextMetric;
  lf      : TLogFont;
  oldFont,
  newFont : HFont;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := ANSI_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Font.Name);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(Canvas.Handle, newFont);
  GetTextMetrics(Canvas.Handle, Metrics);
  Result := (Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  SelectObject(Canvas.Handle, oldFont);
  DeleteObject(newFont);
end;

procedure TSRLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TSRLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FFocusControl) then
    FFocusControl:=nil;
end;

procedure TSRLabel.PaintBevel(ARect: TRect);
var ShColor,
    HLColor : TColor;
    i       : integer;
begin
  AssignBevelColors(Color, HLColor, ShColor, HLContrast, ShContrast);
  with Canvas do begin
    if FBevelStyle=bvFrame then
      Pen.Color:=clWindowFrame
    else begin
      if FBevelStyle=bvRaised then
        Pen.Color:=HLColor
      else
        Pen.Color:=ShColor;
    end;
    for i:=1 to FBevelWidth do begin
      MoveTo(ARect.Right-i, ARect.Top+i-1);
      LineTo(ARect.Left+i-1, ARect.Top+i-1);
      LineTo(ARect.Left+i-1, ARect.Bottom-i);
    end;
    if FBevelStyle=bvSingle then begin
      Pen.Color:=clBlack;
      MoveTo(ARect.Right-FBevelWidth-1, ARect.Top+FBevelWidth);
      LineTo(ARect.Left+FBevelWidth, ARect.Top+FBevelWidth);
      LineTo(ARect.Left+FBevelWidth, ARect.Bottom-FBevelWidth-1);
    end;
    if FBevelStyle=bvFrame then
      Pen.Color:=clWindowFrame
    else begin
      if FBevelStyle=bvRaised then
        Pen.Color:=ShColor
      else
        Pen.Color:=HLColor;
    end;
    for i:=1 to FBevelWidth do begin
      MoveTo(ARect.Left+i-1, ARect.Bottom-i);
      LineTo(ARect.Right-i, ARect.Bottom-i);
      LineTo(ARect.Right-i, ARect.Top+i-1);
    end;
    if FBevelStyle=bvSingle then begin
      Pen.Color:=clSilver;
      MoveTo(ARect.Left+FBevelWidth, ARect.Bottom-FBevelWidth-1);
      LineTo(ARect.Right-FBevelWidth-1, ARect.Bottom-FBevelWidth-1);
      LineTo(ARect.Right-FBevelWidth-1, ARect.Top+FBevelWidth);
    end;
  end;
end;

procedure TSRLabel.PaintText(ARect: TRect; AFlags: Word);
var AText      : string;
    TextHeight,
    TextWidth  : integer;
    TextRect,
    OutRect    : TRect;
begin
  if not FShowAccelChar then
    AFlags:=AFlags or DT_NOPREFIX;
  Canvas.Font:=Font;
  TextRect:=ARect;
  GetTextAndTextRect(AText, TextRect);

  { horizontale und vertikale Offsets berechnen }
  OutRect:=TextRect;
  {$IFDEF SR_Delphi2_Up}
  if (FDirection=tdLeft) or (FDirection=tdRight) then begin
  {$ENDIF}
    { horizontale Text-Ausrichtung }
    TextHeight:=TextRect.Bottom-TextRect.Top;
    TextWidth:=TextRect.Right-TextRect.Left;
    if FBevelStyle<>bvNone then begin
      if FLayout=tlCenter then
        OffsetRect(OutRect, FBevelWidth, 0)
      else
        OffsetRect(OutRect, FBevelWidth, FBevelWidth);
    end;
    if FBevelStyle=bvSingle then begin
      if FLayout=tlCenter then
        OffsetRect(OutRect, 1, 0)
      else
        OffsetRect(OutRect, 1, 1);
    end;
    {$IFDEF SR_Delphi2_Up}
    { Alignment }
    if ((FAlignment=taLeftJustify) and (FDirection=tdRight)) or
     ((FAlignment=taRightJustify) and (FDirection=tdLeft)) then
      OffsetRect(OutRect, FBorderWidth, 0);
    if ((FAlignment=taLeftJustify) and (FDirection=tdLeft)) or
     ((FAlignment=taRightJustify) and (FDirection=tdRight)) then
      OffsetRect(OutRect, Width-TextWidth-2-FBorderWidth, 0);
    if FAlignment=taCenter then
      OffsetRect(OutRect, (Width-TextWidth) div 2, 0);
    { Layout }
    if ((FLayout=tlTop) and (FDirection=tdRight)) or
     ((FLayout=tlBottom) and (FDirection=tdLeft)) then
      OffsetRect(OutRect, 0, FBorderWidth);
    if ((FLayout=tlTop) and (FDirection=tdLeft)) or
     ((FLayout=tlBottom) and (FDirection=tdRight)) then
      OffsetRect(OutRect, 0, Height-TextHeight-FBorderWidth);
    if FLayout=tlCenter then
      OffsetRect(OutRect, 0, ((Height-TextHeight) div 2));
    {$ELSE}
    { Alignment }
    if FAlignment=taLeftJustify then
      OffsetRect(OutRect, FBorderWidth, 0);
    if FAlignment=taRightJustify then
      OffsetRect(OutRect, Width-TextWidth-2-FBorderWidth, 0);
    if FAlignment=taCenter then
      OffsetRect(OutRect, (Width-TextWidth) div 2, 0);
    { Layout }
    if FLayout=tlTop then
      OffsetRect(OutRect, 0, FBorderWidth);
    if FLayout=tlBottom then
      OffsetRect(OutRect, 0, Height-TextHeight-FBorderWidth);
    if FLayout=tlCenter then
      OffsetRect(OutRect, 0, ((Height-TextHeight) div 2));
    {$ENDIF}
  {$IFDEF SR_Delphi2_Up}
  end
  else begin
    { vertikale Text-Ausrichtung }
    TextHeight:=Canvas.TextHeight(AText);
    TextWidth:=Canvas.TextWidth(AText);
    if FBevelStyle<>bvNone then begin
      if FLayout=tlCenter then
        OffsetRect(OutRect, 0, FBevelWidth)
      else
        OffsetRect(OutRect, FBevelWidth, FBevelWidth);
    end;
    if FBevelStyle=bvSingle then begin
      if FLayout=tlCenter then
        OffsetRect(OutRect, 0, 1)
      else
        OffsetRect(OutRect, 1, 1);
    end;
    { Alignment }
    if ((FAlignment=taLeftJustify) and (FDirection=tdDown)) or
     ((FAlignment=taRightJustify) and (FDirection=tdUp)) then
      OffsetRect(OutRect, 0, FBorderWidth);
    if ((FAlignment=taLeftJustify) and (FDirection=tdUp)) or
     ((FAlignment=taRightJustify) and (FDirection=tdDown)) then
      OffsetRect(OutRect, 0, Height-TextWidth-2-FBorderWidth);
    if FAlignment=taCenter then
      OffsetRect(OutRect, 0, (Height-TextWidth) div 2);
    { Layout }
    if ((FLayout=tlTop) and (FDirection=tdDown)) or
     ((FLayout=tlBottom) and (FDirection=tdUp)) then
      OffsetRect(OutRect, Width-TextHeight-2-FBorderWidth, 0);
    if ((FLayout=tlTop) and (FDirection=tdUp)) or
     ((FLayout=tlBottom) and (FDirection=tdDown)) then
      OffsetRect(OutRect, FBorderWidth, 0);
    if FLayout=tlCenter then
      OffsetRect(OutRect, ((Width-TextHeight) div 2), 0);
  end;
  {$ENDIF}
  if FShowHighlight then begin
    if FHighlightPos=hpTopLeft then
      OffsetRect(OutRect, FHighlightOffset, 0)
    else
      if Alignment=taRightJustify then
        OffsetRect(OutRect, -FHighlightOffset, 0);
    if FLayout=tlTop then
      OffsetRect(OutRect, 0, FHighlightOffset);
  end;
  if FShowShadow then begin
    if FHighlightPos=hpTopRight then
      OffsetRect(OutRect, FShadowOffset, 0)
    else
      if Alignment=taRightJustify then
        OffsetRect(OutRect, -FShadowOffset, 0);
    if FLayout=tlBottom then
      OffsetRect(OutRect, 0, -FShadowOffset);
  end;

  if not Enabled then begin
    OffsetRect(OutRect, 1, 1);
    Canvas.Font.Color:=clBtnHighlight;
    DrawTextComp(AText, OutRect, AFlags{$IFDEF SR_Delphi2_Up}, FDirection{$ENDIF});
    OffsetRect(OutRect, -1, -1);
    Canvas.Font.Color:=clBtnShadow;
    DrawTextComp(AText, OutRect, AFlags{$IFDEF SR_Delphi2_Up}, FDirection{$ENDIF});
  end
  else begin

    { Highlight-Text ausgeben }
    if FShowHighlight then begin
      Canvas.Font.Color:=FHighlightColor;
      TextRect:=OutRect;
      { horizontaler Offset }
      if FHighlightPos=hpTopLeft then
        OffsetRect(TextRect, -FHighlightOffset, -FHighlightOffset)
      else
        OffsetRect(TextRect, FHighlightOffset, -FHighlightOffset);
      DrawTextComp(AText, TextRect, AFlags{$IFDEF SR_Delphi2_Up}, FDirection{$ENDIF});
    end;

    { Shadow-Text ausgeben }
    if FShowShadow then begin
      Canvas.Font.Color:=FShadowColor;
      TextRect:=OutRect;
      { horizontaler Offset }
      if FHighlightPos=hpTopLeft then
        OffsetRect(TextRect, FShadowOffset, FShadowOffset)
      else
        OffsetRect(TextRect, -FShadowOffset, FShadowOffset);
      DrawTextComp(AText, TextRect, AFlags{$IFDEF SR_Delphi2_Up}, FDirection{$ENDIF});
    end;

    { Haupttext ausgeben }
    Canvas.Font.Color:=Font.Color;
    if FLinkActive and FMouseOnControl and Enabled then begin
      if FHoverColor<>FColor then
        Canvas.Font.Color:=FHoverColor;
      if FHoverFontColor<>Font.Color then
        Canvas.Font.Color:=FHoverFontColor;
      if FUnderlineOnEnter then
        Canvas.Font.Style:=Font.Style+[fsUnderline];
    end;
    DrawTextComp(AText, OutRect, AFlags{$IFDEF SR_Delphi2_Up}, FDirection{$ENDIF});

  end;
end;

procedure TSRLabel.Paint;
const
  Alignments: array[TAlignment] of Word =
   (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word =
   (0, DT_WORDBREAK);
var DrawStyle : Integer;
begin
  with Canvas do begin

    { Hintergrund }
    if not Transparent or (FMouseOnControl and (FHoverColor<>FColor)) then begin
      if FMouseOnControl and (FHoverColor<>FColor) then
        Brush.Color:=FHoverColor
      else
        Brush.Color:=FColor;
      Brush.Style:=bsSolid;
      FillRect(ClientRect);
    end;

    { Text }
    Brush.Style:=bsClear;
    DrawStyle:=DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    PaintText(ClientRect, DrawStyle);

    { Rahmen }
    if FBevelStyle<>bvNone then
      PaintBevel(ClientRect);
  end;
end;

procedure TSRLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment<>Value then begin
    FAlignment:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize<>Value then begin
    FAutoSize:=Value;
    AdjustBounds;
  end;
end;

procedure TSRLabel.SetBevelStyle(Value: TLabelBevel);
begin
  if not FStyleChange and (FBevelStyle<>Value) then begin
    FBevelStyle:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetBevelWidth(Value: word);
begin
  if not FStyleChange and (FBevelWidth<>Value) then begin
    FBevelWidth:=Value;
    AdjustBounds;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetBorderWidth(Value: word);
begin
  if FBorderWidth<>Value then begin
    FBorderWidth:=Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRLabel.SetColor(Value: TColor);
begin
  if not FStyleChange and (FColor<>Value) then begin
    FColor:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

{$IFDEF SR_Delphi2_Up}
procedure TSRLabel.SetDirection(Value: TTextDirection);
begin
  if FDirection<>Value then begin
    if (csLoading in ComponentState) or FTrueType or (Value=tdRight) then begin
      if not (Value<>tdRight) and FWordWrap then
        SetWordWrap(false);
      FDirection:=Value;
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TSRLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl:=Value;
  if Value<>nil then
    Value.FreeNotification(Self);
end;
{$ENDIF}

procedure TSRLabel.SetHighlightColor(Value: TColor);
begin
  if not FStyleChange and (FHighlightColor<>Value) then begin
    FHighlightColor:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom
    else
      Invalidate;
  end;
end;

procedure TSRLabel.SetHighlightOffset(Value: word);
begin
  if not FStyleChange and (FHighlightOffset<>Value) then begin
    FHighlightOffset:=Value;
    if FShowHighlight then begin
      AdjustBounds;
      Invalidate;
    end;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
  end;
end;

procedure TSRLabel.SetHoverColor(Value: TColor);
begin
  if not FStyleChange and (FHoverColor<>Value) then begin
    FHoverColor:=Value;
    if FMouseOnControl then
      Invalidate;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
  end;
end;

procedure TSRLabel.SetHoverFontColor(Value: TColor);
begin
  if not FStyleChange and (FHoverFontColor<>Value) then begin
    FHoverFontColor:=Value;
    if FMouseOnControl then
      Invalidate;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
  end;
end;

procedure TSRLabel.SetHighlightPos(Value: THighlightPos);
begin
  if not FStyleChange and (FHighlightPos<>Value) then begin
    FHighlightPos:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetLayout(Value: TTextLayout);
begin
  if not FStyleChange and (FLayout<>Value) then begin
    FLayout:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShadowColor(Value: TColor);
begin
  if not FStyleChange and (FShadowColor<>Value) then begin
    FShadowColor:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShadowOffset(Value: word);
begin
  if not FStyleChange and (FShadowOffset<>Value) then begin
    FShadowOffset:=Value;
    if FShowShadow then begin
      AdjustBounds;
      Invalidate;
    end;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
  end;
end;

procedure TSRLabel.SetShortenFilenames(Value: Boolean);
begin
  if not FStyleChange and (FShortenFilenames<>Value) then begin
    FShortenFilenames:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowAccelChar(Value: Boolean);
begin
  if not FStyleChange and (FShowAccelChar<>Value) then begin
    FShowAccelChar:=Value;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowHighlight(Value: Boolean);
begin
  if not FStyleChange and (FShowHighlight<>Value) then begin
    FShowHighlight:=Value;
    AdjustBounds;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowShadow(Value: Boolean);
begin
  if not FStyleChange and (FShowShadow<>Value) then begin
    FShowShadow:=Value;
    AdjustBounds;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetStyle(Value: TLabelStyle);
begin
  if FStyle<>Value then begin
    FStyle:=Value;

    FStyleChange:=true;
    case FStyle of
      ls3DText : begin
        FBevelStyle:=bvNone;
        FBevelWidth:=1;
        FColor:=clBtnFace;
        FHighlightColor:=clBtnHighlight;
        FHighlightOffset:=1;
        FHighlightPos:=hpTopLeft;
        FLinkActive:=false;
        FShadowColor:=clBtnShadow;
        FShadowOffset:=1;
        FShowHighlight:=true;
        FShowShadow:=true;
      end;
      lsLink : begin
        FBevelStyle:=bvNone;
        FBevelWidth:=1;
        Font.Color:=clNavy;
        FHoverColor:=FColor;
        FHoverFontColor:=clBlue;
        FLinkActive:=true;
        FLinkType:=ltWWW;
        FShowAccelChar:=false;
        FShowHighlight:=false;
        FShowShadow:=false;
        FUnderlineOnEnter:=true;
      end;
      lsPath : begin
        FLinkActive:=false;
        FShortenFilenames:=true;
        FShowAccelChar:=false;
        FShowHighlight:=false;
        FShowShadow:=false;
      end;
      lsWindow : begin
        FBevelStyle:=bvSingle;
        FBevelWidth:=1;
        FBorderWidth:=2;
        FColor:=clWindow;
        Font.Color:=clWindowText;
        FLayout:=tlCenter;
        FLinkActive:=false;
        FShowHighlight:=false;
        FShowShadow:=false;
      end;
    end;
    FStyleChange:=false;

    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRLabel.SetTransparent(Value: Boolean);
begin
  if not FStyleChange and (Transparent<>Value) then begin
    if Value then
      ControlStyle:=ControlStyle-[csOpaque]
    else
      ControlStyle:=ControlStyle+[csOpaque];
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
    Invalidate;
  end;
end;

procedure TSRLabel.SetUnderlineOnEnter(Value: boolean);
begin
  if not FStyleChange and (FUnderlineOnEnter<>Value) then begin
    FUnderlineOnEnter:=Value;
    if FMouseOnControl then
      Invalidate;
    if FStyle<>lsCustom then
      FStyle:=lsCustom;
  end;
end;

procedure TSRLabel.SetWordWrap(Value: Boolean);
begin
  {$IFDEF SR_Delphi2_Up}
  if (FDirection=tdRight) and (FWordWrap<>Value) then begin
  {$ELSE}
  if FWordWrap<>Value then begin
  {$ENDIF}
    FWordWrap:=Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRLabel]);
end;

end.
