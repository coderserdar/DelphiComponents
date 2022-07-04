unit SRColBtn;

{ TSRColorButton (C)opyright 2005 Version 1.34
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponente ist eine TSpeedButton-ähnliche Button-Komponente,
  die eine Color-Eigenschaft für farbige Buttons bietet. Außerdem
  kann ein Farbverlauf auf die Button-Oberfläche gezeichnet werden
  und es gibt eine per Timer gesteuerte automatische Click-Wiederholung.

  Die Komponente ist abgeleitet von TGraphicControl und sie ist Public
  Domain, das Urheberrecht liegt aber beim Autor.

  Vielen Dank an Markus Pinl für die Fehlerkorrektur und die Beisteuerung
  der BorderColor-Eigenschaft und an Robert Rossmair für die rrColors-Unit!

  Dank auch an Matthias Gilbricht für die Fehlerkorrektur in der
  SetGlyph-Methode }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Classes,
  Graphics, Controls, ExtCtrls, SysUtils, {$IFNDEF SR_Delphi3_Up} Menus,{$ENDIF}
  Messages;


const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type
  TBorderStyle = (bsFlat, bsNormal, bsSingle);
  TButtonLayout = (blGlyphBottom, blGlyphLeft, blGlyphRight, blGlyphTop);
  TContrast = 0..9;
  TGradDirection = (gdDownRight, gdUpLeft);
  TGradientStyle = (gsNone, gsHorizontal, gsPyramid, gsVertical);
  TNumGlyphs = 0..4;


  TSRColorButton = class(TGraphicControl)
  private
    FAllowAllUp,
    FAllowTimer:        boolean;
    FBC:                array[0..255] of longint;
    FBevelWidth:        integer;
    FBorderColor:       TColor;
    FBorderStyle:       TBorderStyle;
    FButtonPressed,
    FChangeDirection:   boolean;
    FColor,
    FColorHighlight,
    FColorShadow:       TColor;
    FContrastHighlight,
    FContrastShadow:    TContrast;
    FGradientDirection: TGradDirection;
    FGrouped:           boolean;
    FGroupIndex:        integer;
    FDown:              boolean;
    FGlyph:             TBitmap;
    FGradientStyle:     TGradientStyle;
    FLayout:            TButtonLayout;
    FMargin:            integer;
    FNumGlyphs:         TNumGlyphs;
    FRepeatTimer:       TTimer;
    FSpacing:           integer;
    FTimerDelay,
    FTimerInterval:     word;
    FTopMargin:         integer;

    FMouseDown:         boolean;
    FOnClick,
    FOnStateChange:     TNotifyEvent;

    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message:TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;

  protected
    procedure DrawBorder(const StateChanged:boolean);
    procedure DrawGradient;
    procedure LoadColors;
    procedure Paint;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetAllowAllUp(newValue: boolean);
    procedure SetBevelWidth(newValue: integer);
    procedure SetBorderColor(newColor: TColor);
    procedure SetBorderStyle(newValue: TBorderStyle);
    procedure SetChangeDirection(newValue: boolean);
    procedure SetColor(newColor: TColor);
    procedure SetContrastHighlight(newValue: TContrast);
    procedure SetContrastShadow(newValue: TContrast);
    procedure SetDown(newValue: boolean);
    procedure SetGlyph(newGlyph: TBitmap);
    procedure SetGradientDirection(newValue: TGradDirection);
    procedure SetGradientStyle(newValue: TGradientStyle);
    procedure SetLayout(newValue: TButtonLayout);
    procedure SetMargin(newValue: integer);
    procedure SetNumGlyphs(newNumGlyphs: TNumGlyphs);
    procedure SetSpacing(newValue: integer);
    procedure SetTopMargin(newValue: integer);
    procedure TimerExpired(Sender: TObject);
    procedure UncheckGroupButtons(AIndex: integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    {$ENDIF}
    property AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp;
    property AllowTimer: boolean read FAllowTimer write FAllowTimer;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelWidth: integer read FBevelWidth write SetBevelWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Caption;
    property ChangeDirection: boolean read FChangeDirection write FChangeDirection;
    property Color: TColor read FColor write SetColor;
    property ContrastHighlight: TContrast read FContrastHighlight write SetContrastHighlight;
    property ContrastShadow: TContrast read FContrastShadow write SetContrastShadow;
    property Down: boolean read FDown write SetDown;
    property Enabled;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GradientDirection: TGradDirection read FGradientDirection write SetGradientDirection;
    property GradientStyle: TGradientStyle read FGradientStyle write SetGradientStyle;
    property Grouped: boolean read FGrouped write FGrouped;
    property GroupIndex: integer read FGroupIndex write FGroupIndex;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Margin: integer read FMargin write SetMargin;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 0;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: integer read FSpacing write SetSpacing;
    property TimerDelay: word read FTimerDelay write FTimerDelay;
    property TimerInterval: word read FTimerInterval write FTimerInterval;
    property TopMargin: integer read FTopMargin write SetTopMargin;
    property Visible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
uses rrColors;
{$ELSE}
{$R *.D16}
{$ENDIF}

const
  DefaultWidth  = 75;
  DefaultHeight = 25;

function IsAccellerator(VK: Word; const AText: string): Boolean;
var P     : Integer;
    AKey,
    Accel : char;
begin
  P:=Pos('&', AText);
  AKey:=Upcase(Char(VK));
  if P<Length(AText) then begin
    Accel:=Upcase(AText[P+1]);
    Result:=(P>0) and (P<Length(AText)) and (AKey=Accel);
  end
  else
    Result:=false;
end;

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
end;


{ Komponente SRColorButton }
constructor TSRColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph:=TBitmap.Create;

  LoadColors;
  { Vorgabewerte setzen }
  FAllowAllUp:=false;
  FBevelWidth:=1;
  FBorderStyle:=bsNormal;
  FColor:=clBtnFace;
  FContrastHighlight:=5;
  FContrastShadow:=6;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FContrastHighlight,FContrastShadow);
  FGradientDirection:=gdDownRight;
  FDown:=false;
  FGradientStyle:=gsNone;
  FGrouped:=false;
  FGroupIndex:=0;
  FLayout:=blGlyphLeft;
  FMargin:=1;
  FNumGlyphs:=0;
  FSpacing:=1;
  FTopMargin:=0;
  TimerDelay:=InitRepeatPause;
  TimerInterval:=RepeatPause;
  Height:=25;
  Width:=75;

  FMouseDown:=False;
end;

destructor TSRColorButton.Destroy;
begin
  if assigned(FGlyph) then
    FGlyph.Free;
  if assigned(FRepeatTimer) then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TSRColorButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      if FGrouped and (not FDown or FAllowAllUp) then begin
        FButtonPressed:=FDown;
        SetDown(not FDown);
        FMouseDown:=True;
        if FButtonPressed<>FDown then begin
          if FGradientStyle<>gsNone then
            Invalidate
          else
            DrawBorder(FButtonPressed<>FDown);
        end;
      end;
      if Enabled and Assigned(FOnClick) then
        FOnClick(Self);
      Result:=1;
    end
    else
      inherited;
  end;
end;

procedure TSRColorButton.CMEnabledChanged(var Message:TMessage);
begin
  inherited;
  if not Grouped then
    SetDown(false);
  Invalidate;
end;

procedure TSRColorButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSRColorButton.CMTextChanged(var msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TSRColorButton.DrawBorder(const StateChanged:boolean);
var i      : integer;
    Dest,
    Source : TRect;
begin
  Dest:=GetClientRect;
  with Canvas do begin
    Brush.Style:=bsSolid;
    Pen.Style:=psSolid;
    Pen.Width:=1;
    if FBorderStyle=bsSingle then begin
      Brush.Color:=FBorderColor;
      FrameRect(Dest);
      InflateRect(Dest,-1,-1);
    end;
    if FBorderStyle=bsNormal then begin
      if FDown then
        Pen.Color:=FBorderColor
      else
        Pen.Color:=FColorHighlight;
      MoveTo(Dest.Left, Dest.Bottom-1);
      LineTo(Dest.Left, Dest.Top);
      LineTo(Dest.Right-1, Dest.Top);
      if FDown then
        Pen.Color:=FColorHighlight
      else
        Pen.Color:=FBorderColor;
      MoveTo(Dest.Left, Dest.Bottom-1);
      LineTo(Dest.Right-1, Dest.Bottom-1);
      LineTo(Dest.Right-1, Dest.Top);
      InflateRect(Dest,-1,-1);
    end;

    { links + oben }
    if FDown then
      Pen.Color:=FColorShadow
    else
      Pen.Color:=FColorHighlight;
    for i:=0 to FBevelWidth-1 do begin
      MoveTo(Dest.Right-i-1, Dest.Top+i);
      LineTo(Dest.Left+i, Dest.Top+i);
      LineTo(Dest.Left+i, Dest.Bottom-i-1);
    end;

    { rechts + unten }
    if FDown then
      Pen.Color:=FColorHighlight
    else
      Pen.Color:=FColorShadow;
    for i:=0 to FBevelWidth-1 do begin
      MoveTo(Dest.Right-i-1, Dest.Top+i);
      LineTo(Dest.Right-i-1, Dest.Bottom-i-1);
      LineTo(Dest.Left+i, Dest.Bottom-i-1);
    end;

    if StateChanged then begin
      if FDown then begin
        { Source muß links oben beginnen, rechts+unten 1 Pixel Rand }
        Source.Left:=Dest.Left+FBevelWidth;
        Source.Top:=Dest.Top+FBevelWidth;
        Source.Right:=Dest.Right-FBevelWidth-1;
        Source.Bottom:=Dest.Bottom-FBevelWidth-1;
        i:=1;
      end
      else begin
        { Source muß rechts unten anliegen, links+oben 1 Pixel Rand }
        Source.Left:=Dest.Left+FBevelWidth+1;
        Source.Top:=Dest.Top+FBevelWidth+1;
        Source.Right:=Dest.Right-FBevelWidth;
        Source.Bottom:=Dest.Bottom-FBevelWidth;
        i:=-1;
      end;
      { Entsprechende Verschiebung }
      Dest.Right:=Source.Right+i;
      Dest.Left:=Source.Left+i;
      Dest.Top:=Source.Top+i;
      Dest.Bottom:=Source.Bottom+i;
      Self.Canvas.CopyRect(Dest, Self.Canvas, Source);
    end;
  end;
end;

procedure TSRColorButton.DrawGradient;
var
  OutRect,
  TempRect   : TRect;
  TempStepV  : Single;
  TempStepH  : Single;
  ColorCode,
  TempLeft,
  TempTop,
  OutWidth,
  OutHeight,
  TempHeight,
  TempWidth,
  ECount,i   : integer;
  FlipDir    : boolean;
begin
  OutRect:=GetClientRect;
  OutWidth:=OutRect.Right-OutRect.Left;
  OutHeight:=OutRect.Bottom-OutRect.Top;
  FlipDir:=FChangeDirection and FDown;
  if (FGradientStyle=gsHorizontal) or (FGradientStyle=gsVertical) then begin
    if FGradientStyle=gsVertical then begin
      TempStepH:=1;
      TempStepV:=OutHeight/255;
      TempHeight:=Trunc(TempStepV+1);
      TempWidth:=1;
    end
    else begin
      TempStepH:=OutWidth/255;
      TempStepV:=1;
      TempHeight:=1;
      TempWidth:=Trunc(TempStepH+1);
    end;
    with Canvas do begin
      TempTop:=OutRect.Top;
      TempLeft:=OutRect.Left;
      TempRect:=OutRect;
      { Geradlinigen Verlauf zeichnen }
      Brush.Style:=bsSolid;
      for ColorCode:=0 to 255 do begin
        if FlipDir then
          Brush.Color:=FBC[255-ColorCode]
        else
          Brush.Color:=FBC[ColorCode];
        if FGradientStyle=gsVertical then begin
          TempRect.Top:=TempTop;
          TempRect.Bottom:=TempTop+TempHeight;
        end
        else begin
          TempRect.Left:=TempLeft;
          TempRect.Right:=TempLeft+TempWidth;
        end;
        FillRect(TempRect);
        if FGradientStyle=gsVertical then
          TempTop:=Trunc(TempStepV*ColorCode)
        else
          TempLeft:=Trunc(TempStepH*ColorCode);
      end;
    end;
  end;
  if FGradientStyle=gsPyramid then begin
    with Canvas do begin
      TempLeft:=OutWidth div 2;
      TempTop:=OutHeight div 2;
      Pen.Width:=1;
      ECount:=OutWidth+OutHeight;
      TempStepH:=255/ECount;
      i:=0;
      while i<=OutWidth do begin
        ColorCode:=trunc(TempStepH*i);
        if FlipDir then
          Pen.Color:=FBC[255-ColorCode]
        else
          Pen.Color:=FBC[ColorCode];
        MoveTo(i, 0);
        LineTo(TempLeft,TempTop);
        ColorCode:=trunc(TempStepH*(i+OutHeight));
        if FlipDir then
          Pen.Color:=FBC[255-ColorCode]
        else
          Pen.Color:=FBC[ColorCode];
        LineTo(i, OutHeight-1);
        inc(i);
      end;
      i:=0;
      while i<=OutHeight do begin
        ColorCode:=trunc(TempStepH*(i+OutWidth));
        if FlipDir then
          Pen.Color:=FBC[255-ColorCode]
        else
          Pen.Color:=FBC[ColorCode];
        MoveTo(OutWidth-1, i);
        LineTo(TempLeft,TempTop);
        ColorCode:=trunc(TempStepH*i);
        if FlipDir then
          Pen.Color:=FBC[255-ColorCode]
        else
          Pen.Color:=FBC[ColorCode];
        LineTo(0, i);
        inc(i);
      end;
    end;
  end;
end;

procedure TSRColorButton.LoadColors;
var StartColor,
    EndColor       : TColor;
    AContrast,
    ContrastFactor : double;
    i,Start        : byte;

  procedure CalcGradientColor(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;Contrast:double);
  begin
    {$IFDEF SR_Delphi1}
    HighlightColor:=ChangeBrightness(FaceColor, 60-round(60*Contrast));
    ShadowColor:=ChangeBrightness(FaceColor, -80+round(80*Contrast));
    {$ELSE}
    Get3DColors(FaceColor, HighlightColor, ShadowColor, Contrast, Contrast);
    {$ENDIF}
  end; { CalcGradientColor }

begin
  ContrastFactor:=1/128;
  for i:=0 to 127 do begin
    AContrast:=i*ContrastFactor;
    CalcGradientColor(FColor, StartColor, EndColor, AContrast);
    if FGradientDirection=gdDownRight then
      FBC[i]:=StartColor
    else
      FBC[i]:=EndColor;
  end;
  if FGradientDirection=gdDownRight then
    Start:=127
  else
    Start:=128;
  for i:=Start to 255 do begin
    AContrast:=(255-i)*ContrastFactor;
    CalcGradientColor(FColor, StartColor, EndColor, AContrast);
    if FGradientDirection=gdDownRight then
      FBC[i]:=EndColor
    else
      FBC[i]:=StartColor;
  end;
end;

procedure TSRColorButton.Loaded;
begin
  LoadColors;

  inherited Loaded;
end;

procedure TSRColorButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  
  if Enabled and (Button=mbLeft) then begin
    FButtonPressed:=FDown;
    if not FDown then
      SetDown(true);
    FMouseDown:=True;
    if FButtonPressed<>FDown then begin
      if FGradientStyle<>gsNone then
        Invalidate
      else
        DrawBorder(FButtonPressed<>FDown);
    end;
    if FAllowTimer then begin
      if FRepeatTimer=nil then
        FRepeatTimer:=TTimer.Create(Self);
      FRepeatTimer.OnTimer:=TimerExpired;
      FRepeatTimer.Interval:=FTimerDelay;
      FRepeatTimer.Enabled:=True;
    end;
  end;
end;

procedure TSRColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var WasDown : boolean;
begin
  if Enabled and (Button=mbLeft) then begin
    WasDown:=FDown;
    if FDown and (not FGrouped or (FGrouped and FButtonPressed and FAllowAllUp)) then
      SetDown(false);
    FMouseDown:=False;
    if FDown<>WasDown then begin
      if FGradientStyle<>gsNone then
        Invalidate
      else
        DrawBorder(WasDown<>FDown);
    end;
    if FRepeatTimer<>nil then
      FRepeatTimer.Enabled:=False;
    if Assigned(FOnClick) then
       FOnClick(Self);
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSRColorButton.Paint;
var
  Dest,Source,
  CRect,TextR : TRect;
  outWidth,
  outHeight,
  TextLeft,
  TextTop     : integer;
  DoDrawGlyph : boolean;
  outText     : array [0..79] of char;
begin
  CRect:=GetClientRect;
  InflateRect(CRect, -FBevelWidth, -FBevelWidth);
  if FBorderStyle<>bsFlat then
    InflateRect(CRect, -1, -1);
  Canvas.Font.Assign(Self.Font);

  with Canvas do begin
    Brush.Style:=bsSolid;
    Brush.Color:=FColor;
    FillRect(CRect);
  end;
  if FGradientStyle<>gsNone then begin
    DrawGradient;
    Canvas.Brush.Style:=bsClear;
  end;

  {$IFDEF SR_Delphi3_Up}
  DoDrawGlyph:=true;
  {$ELSE}
  DoDrawGlyph:=FGradientStyle=gsNone;
  {$ENDIF}

  with Canvas do begin
    { Glyph anzeigen }
    outWidth:=  0;
    outHeight:= 0;
    if Assigned(FGlyph) and DoDrawGlyph then begin
      with Source do begin
        { Source-Rechteck ermitteln }
        Left:=0;
        Right:=FGlyph.Width;
        Top:=0;
        Bottom:=FGlyph.Height;
        if FNumGlyphs>0 then
          Right:=Right div FNumGlyphs;
      end;
      if FNumGlyphs > 0 then begin
        if(not Enabled and (FNumGlyphs > 1)) then begin
          { disabled button }
          Source.Left:=  FGlyph.width div FNumGlyphs;
          Source.Right:= Source.Left shl 1;
        end;
        { Größe des Destination-Rechtecks }
        outWidth:=  Source.Right-Source.Left;
        outHeight:= Source.Bottom-Source.Top;
        { Glyph-Position ermitteln }
        Dest.Left:=  ((CRect.Right-outWidth) shr 1);
        Dest.Right:= Dest.Left+outWidth;
        if (Caption<>'') and (FLayout=blGlyphLeft) then begin
          Dest.Left:=  ((CRect.Right-(outWidth+FSpacing+TextWidth(Caption))) shr 1)-FMargin;
          Dest.Right:= Dest.Left+outWidth;
        end;
        if (Caption<>'') and (FLayout=blGlyphRight) then begin
          Dest.Left:=  ((CRect.Right+(outWidth+FSpacing+TextWidth(Caption))) shr 1)-outWidth+FMargin;
          Dest.Right:= Dest.Left+outWidth;
        end;
        if (Caption='') or (FLayout=blGlyphLeft) or (FLayout=blGlyphRight) then begin
          Dest.Top:=   ((CRect.Top+CRect.Bottom-outHeight) shr 1);
          Dest.Bottom:=Dest.Top+outHeight;
        end;
        if (Caption<>'') and (FLayout=blGlyphTop) then begin
          Dest.Top:=  ((CRect.Top+CRect.Bottom-(outHeight+FSpacing+TextHeight(Caption))) shr 1)-FMargin;
          Dest.Bottom:= Dest.Top+outHeight;
        end;
        if (Caption<>'') and (FLayout=blGlyphBottom) then begin
          Dest.Top:=  ((CRect.Top+CRect.Bottom-(outHeight+FSpacing+TextHeight(Caption))) shr 1)-outHeight+FMargin;
          Dest.Bottom:= Dest.Top+outHeight;
        end;
        if FGradientStyle=gsNone then begin
          Pen.Style := psSolid;
          Pen.Color := Color;
        end
        else
          Pen.Style := psClear;
        if FDown then begin
          { Glyph um 1 Pixel nach rechts unten verschieben }
          Inc(Dest.Left);
          Inc(Dest.Right);
          Inc(Dest.Top);
          Inc(Dest.Bottom);
          { verbleibende Up-Reste löschen }
          MoveTo(Dest.Left-1, Dest.Bottom);
          LineTo(Dest.Left-1, Dest.Top-1);
          LineTo(Dest.Right, Dest.Top-1);
        end
        else begin
          { verbleibende Down-Reste löschen }
          MoveTo(Dest.Right, Dest.Top);
          LineTo(Dest.Right, Dest.Bottom);
          LineTo(Dest.Left, Dest.Bottom);
        end;
        if (FDown and (FNumGlyphs > 2)) then begin
          { Glyph für gedrückten Zustand bestimmen }
          Source.Left:= FGlyph.width div FNumGlyphs * 2;
          Source.Right:=FGlyph.width div FNumGlyphs * 3;
        end;
        if FGradientStyle=gsNone then begin
          Brush.Style:= bsSolid;
          Brush.Color:= Color;
        end
        else
          Brush.Style:=bsClear;

        { Glyph zeichnen }
        BrushCopy(Dest, FGlyph, Source, FGlyph.Canvas.Pixels[0,FGlyph.Height-1]);
      end;
    end;

    { Caption zeichnen }
    if Caption<>'' then begin
      { Position ermitteln }
      TextLeft:=(CRect.Right-TextWidth(Caption)) div 2;
      if Assigned(FGlyph) and DoDrawGlyph and (FNumGlyphs > 0) and (FLayout=blGlyphRight) then
        TextLeft:=Dest.Left-TextWidth(Caption)-FSpacing;
      if Assigned(FGlyph) and DoDrawGlyph and (FNumGlyphs > 0) and (FLayout=blGlyphLeft) then
        TextLeft:=Dest.Left+outWidth+FSpacing;
      if FChangeDirection and FDown then
        TextTop:=((CRect.Top+CRect.Bottom-TextHeight(Caption)) div 2)-FTopMargin
      else
        TextTop:=((CRect.Top+CRect.Bottom-TextHeight(Caption)) div 2)+FTopMargin;
      if Assigned(FGlyph) and DoDrawGlyph and (FNumGlyphs > 0) and (FLayout=blGlyphTop) then
        TextTop:=Dest.Top+outHeight+FSpacing;
      if Assigned(FGlyph) and DoDrawGlyph and (FNumGlyphs > 0) and (FLayout=blGlyphBottom) then
        TextTop:=Dest.Top-TextHeight(Caption)-FSpacing;
      if FDown then
        inc(TextTop);
      { Text ausgeben }
      if FGradientStyle=gsNone then begin
        Brush.Style:= bsSolid;
        Brush.Color:= Color;
      end
      else
        Brush.Style:=bsClear;
      if FDown then
        { verbleibende Up-Reste löschen }
        FillRect(Rect( TextLeft, TextTop, TextLeft+TextWidth(Caption), TextTop+TextHeight(Caption)))
      else
        { verbleibende Down-Reste löschen }
        FillRect(Rect( TextLeft+1, TextTop+1, TextLeft+1+TextWidth(Caption), TextTop+1+TextHeight(Caption)));
      TextR:=Rect( TextLeft, TextTop, TextLeft+TextWidth(Caption), TextTop+TextHeight(Caption));
      StrPCopy(outText, Caption);
      if not Enabled then
        Font.Color:=clGrayText;
      DrawText(Handle, outText, length(Caption), TextR, DT_Center or DT_VCenter or DT_SingleLine);
    end;
  end;
  DrawBorder(false);
end;

procedure TSRColorButton.SetAllowAllUp(newValue: boolean);
begin
  if FAllowAllUp<>NewValue then begin
    FAllowAllUp:=NewValue;
    if not FAllowAllUp and FGrouped and not FDown then begin
      { prüfen, ob ein anderer Button der Gruppe gedrückt ist }
    end;
  end;
end;

procedure TSRColorButton.SetBevelWidth(NewValue: integer);
begin
  if (FBevelWidth<>NewValue) and (NewValue>=0) and (NewValue<(Height div 2)) and (NewValue<(Width div 2)) then begin
    FBevelWidth:=NewValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetBorderColor(newColor: TColor);
begin
  if newColor<>FBorderColor then begin
    FBorderColor:=newColor;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetBorderStyle(newValue: TBorderStyle);
begin
  if FBorderStyle<>newValue then begin
    FBorderStyle:=newValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetChangeDirection(newValue: boolean);
begin
  if FChangeDirection<>newValue then begin
    FChangeDirection:=newValue;
    if (FGradientStyle<>gsNone) and FDown then
      Invalidate;
  end;
end;

procedure TSRColorButton.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FContrastHighlight,FContrastShadow);
    if FGradientStyle<>gsNone then
      LoadColors;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetContrastHighlight(newValue: TContrast);
begin
  if (FContrastHighlight<>NewValue) and (NewValue>=0) and (NewValue<10) then begin
    FContrastHighlight:=NewValue;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FContrastHighlight,FContrastShadow);
    if FGradientStyle<>gsNone then
      LoadColors;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetContrastShadow(newValue: TContrast);
begin
  if (FContrastShadow<>NewValue) and (NewValue>=0) and (NewValue<10) then begin
    FContrastShadow:=NewValue;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FContrastHighlight,FContrastShadow);
    if FGradientStyle<>gsNone then
      LoadColors;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetDown(newValue: boolean);
begin
  if FDown<>newValue then begin
    FDown:=newValue;
    if FGradientStyle<>gsNone then
      Invalidate
    else
      DrawBorder(true);
    if FGrouped and FDown then
      UncheckGroupButtons(FGroupIndex);
    if assigned(FOnStateChange) then
      FOnStateChange(self);
  end;
end;

procedure TSRColorButton.SetGlyph(newGlyph: TBitmap);
begin
  if Assigned(FGlyph) then begin
    FGlyph.Assign(newGlyph);
    if Assigned(newGlyph) then begin
      if (csDesigning in ComponentState) then begin
        { Glyph 1: Normal, 2: Disabled, 3: Down;
          Muß die Ausmaße (Height * NumGlyphs) = Width  haben }
        if (newGlyph.width mod newGlyph.height = 0) then
          FNumGlyphs:= newGlyph.width div newGlyph.height
        else
          FNumGlyphs:= 1;
      end;
    end;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetGradientDirection(newValue: TGradDirection);
begin
  if FGradientDirection<>newValue then begin
    FGradientDirection:=newValue;
    if FGradientStyle<>gsNone then
      LoadColors;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetGradientStyle(newValue: TGradientStyle);
begin
  if FGradientStyle<>newValue then begin
    FGradientStyle:=newValue;
    if FGradientStyle<>gsNone then
      LoadColors;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetLayout(newValue: TButtonLayout);
begin
  if FLayout<>newValue then begin
    FLayout:=newValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetMargin(newValue: integer);
begin
  if FMargin<>newValue then begin
    FMargin:=newValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetNumGlyphs(newNumGlyphs: TNumGlyphs);
begin
  if FNumGlyphs<>newNumGlyphs then begin
    FNumGlyphs:= newNumGlyphs;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetSpacing(newValue: integer);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.SetTopMargin(newValue: integer);
begin
  if FTopMargin<>newValue then begin
    FTopMargin:=newValue;
    Invalidate;
  end;
end;

procedure TSRColorButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval:=FTimerInterval;
  if FDown and Enabled and MouseCapture then begin
    try
      Click;
    except
      FRepeatTimer.Enabled:=False;
      raise;
    end;
  end;
end;

procedure TSRColorButton.UncheckGroupButtons(AIndex: integer);
var i : integer;
begin
  for i:=0 to Self.Parent.ControlCount-1 do
    if Self.Parent.Controls[i] is TSRColorButton then
      if (TSRColorButton(Self.Parent.Controls[i])<>Self)
       and TSRColorButton(Self.Parent.Controls[i]).Grouped
       and (TSRColorButton(Self.Parent.Controls[i]).GroupIndex=AIndex)
       and TSRColorButton(Self.Parent.Controls[i]).Down then
         TSRColorButton(Self.Parent.Controls[i]).Down:=false;
end;

procedure TSRColorButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:=1;
end;

procedure Register;
begin
  RegisterComponents('Simon',[TSRColorButton]);
end;

end.
