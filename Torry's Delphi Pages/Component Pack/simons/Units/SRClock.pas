unit SRClock;

{ TSRClock (C)opyright 2005 Version 3.00
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die SRClock-Komponente stellt eine analoge oder digitale Uhr in verschiedenen
  Designs dar. Die Uhr kann auch als Stopuhr verwendet werden und verfügt über
  eine Alarmfunktion.

  Diese Komponente ist Public Domain, das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

{$R-}

uses {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, {$ENDIF}
     Classes, Controls, Messages, Forms, Graphics, StdCtrls, ExtCtrls, Grids, SysUtils;

type
  TBevelWidth = 1..10;
  TBorderWidth = 0..20;
  TClockBevel = (cbNone, cbLowered, cbRaised, cbSingle);
  TClockStyle = (csClassic, csDigital, csMovingPoints, csPieSlice);
  TClockKind = (ckRealTime, ckStopWatch);
  TClockOption = (coDrawDigitShapes, coFadingColor, coShowAlarm, coShowDate,
                  coShowKind, coShowSeconds, coShowTicks);
  TClockOptions = set of TClockOption;
  TContrast = 0..9;
  TMarkingStyle = (msNone, msQuarters, msAll);
  TSeparator = (dsApostrophe, dsComma, dsDoublePoint, dsHyphen, dsNone, dsPoint, dsSemicolon);
  TTime = TDateTime;

  TSRClock = class;

  TClockColors = class(TPersistent)
  private
    FBackground,
    FBorder,
    FDate,
    FHands,
    FNumbers     : TColor;
    FOwner       : TSRClock;

    procedure SetBackground(Value: TColor);
    procedure SetBorder(Value: TColor);
    procedure SetDate(Value: TColor);
    procedure SetHands(Value: TColor);
    procedure SetNumbers(Value: TColor);

  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source : TPersistent); override;

  published
    property Background: TColor read FBackground write SetBackground;
    property Border: TColor read FBorder write SetBorder;
    property Date: TColor read FDate write SetDate;
    property Hands: TColor read FHands write SetHands;
    property Numbers: TColor read FNumbers write SetNumbers;
  end;

  TSRClock = class(TGraphicControl)
  private
    FAlarmActive          : boolean;
    FAlarmTime            : TDateTime;
    FAutoUpdate           : boolean;
    FBevelStyle           : TClockBevel;
    FBevelWidth           : TBevelWidth;
    FBorderWidth          : TBorderWidth;
    FBuffer               : TBitmap;
    FColors               : TClockColors;
    FDate                 : integer;
    FDigit                : array [0..9] of TBitmap;
    FDigitShapeColor      : TColor;
    FDigitHeight,
    FDisplayHeight,
    FDisplayWidth,
    FHour,FMinute,FSecond : word;
    FImgAlarm,
    FImgKind              : TBitmap;
    FKind                 : TClockKind;
    FLEDContrast          : TContrast;
    FLineWidth            : byte;
    FMarkingStyle         : TMarkingStyle;
    {$IFNDEF SR_Delphi5_Up}
    FOldHeight,
    FOldWidth             : integer;
    {$ENDIF}
    FOptions              : TClockOptions;
    FRunning              : boolean;
    FSegmentOffColor      : TColor;
    FSegCl                : array [0..9, 1..7] of TColor;
    FSeparator            : TSeparator;
    FSummertime           : boolean;
    FStyle                : TClockStyle;
    FTextHeight           : integer;
    FTickColor            : TColor;
    FTime                 : TTime;
    FTimeOffset           : double;
    FTimer                : TTimer;
    FTransColorAlarm,
    FTransColorKind       : TColor;
    FUpdateInterval       : word;

    FOnAlarm,
    FOnMouseEnter,
    FOnMouseExit,
    FOnTimer              : TNotifyEvent;


    procedure SetAlarmActive(Value: boolean);
    procedure SetAutoUpdate(Value: boolean);
    procedure SetBevelStyle(Value: TClockBevel);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetColors(Value: TClockColors);
    procedure SetKind(Value: TClockKind);
    procedure SetLEDContrast(Value : TContrast);
    procedure SetLineWidth (Value: byte);
    procedure SetMarkingStyle(Value: TMarkingStyle);
    procedure SetOptions(Value: TClockOptions);
    procedure SetSeparator(Value: TSeparator);
    procedure SetStyle(Value: TClockStyle);
    procedure SetTime(Value: TTime);
    procedure SetUpdateInterval(Value: word);

    procedure AdjustBounds;
    procedure AssignColors (seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);
    procedure CreateDigitBitmaps(AWidth, AHeight: integer);

  protected
    procedure ApplyNewColors;
    procedure AutoUpdateClock(Sender: TObject);
    procedure Loaded; override;
    procedure Paint; override;
    {$IFDEF SR_Delphi5_Up}
    procedure Resize; override;
    {$ENDIF}

    procedure CMEnabledChanged(var Message: TWmNoParams); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMVisibleChanged(var Message: TWmNoParams); message CM_VISIBLECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;

  public
    property Hour: word read FHour;
    property Minute: word read FMinute;
    property Second: word read FSecond;
    property Summertime: boolean read FSummertime;
    property Time: TTime read FTime write SetTime;

    procedure Reset;
    procedure Start;
    procedure Stop;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property AlarmActive: boolean read FAlarmActive write SetAlarmActive;
    property AlarmTime: TDateTime read FAlarmTime write FAlarmTime;
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate;
    property BevelStyle: TClockBevel read FBevelStyle write SetBevelStyle;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth;
    property Colors: TClockColors read FColors write SetColors;
    property DigitLineWidth: byte read FLineWidth write SetLineWidth;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Kind: TClockKind read FKind write SetKind;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property MarkingStyle: TMarkingStyle read FMarkingStyle write SetMarkingStyle;
    property Options: TClockOptions read FOptions write SetOptions;
    property ParentShowHint;
    property Separator: TSeparator read FSeparator write SetSeparator;
    property ShowHint;
    property Style: TClockStyle read FStyle write SetStyle;
    property UpdateInterval: word read FUpdateInterval write SetUpdateInterval;
    property Visible;

    property OnAlarm: TNotifyEvent read FOnAlarm write FOnAlarm;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnStartDrag;
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2}
{$R *.dcr}
{$ENDIF}
{$R *.res}

uses SRUtils, rrColors;

const
  FShapeContrast = 3;

function XCoord(XMittel,XRadius,Grad:word):word;
begin
  Result:=round(XMittel-(sin(Grad*Pi/180)*XRadius));
end; { XCoord }

function YCoord(YMittel,YRadius,Grad:word):word;
begin
  Result:=round(YMittel-(cos(Grad*Pi/180)*YRadius));
end; { YCoord }

function GetIntermediateColor(Color1,Color2:TColor;AContrast:integer):TColor;
var YR,YG,YB,SR,
    SG,SB,DR,DG,DB,
    StartClr,EndClr : integer;
begin
  StartClr:=ColorToRGB(Color1);
  YR:=GetRValue(StartClr);
  YG:=GetGValue(StartClr);
  YB:=GetBValue(StartClr);
  SR:=YR;
  SG:=YG;
  SB:=YB;
  EndClr:=ColorToRGB(Color2);
  DR:=GetRValue(EndClr)-SR;
  DG:=GetGValue(EndClr)-SG;
  DB:=GetBValue(EndClr)-SB;
  YR:=SR + round(DR / 9 * AContrast);
  YG:=SG + round(DG / 9 * AContrast);
  YB:=SB + round(DB / 9 * AContrast);
  Result:=RGB(YR, YG, YB);
end; { GetIntermediateColor }

{ TColors }

constructor TClockColors.Create(AOwner: TComponent);
begin
  inherited Create;

  if AOwner is TSRClock then
    FOwner:=TSRClock(AOwner)
  else
    FOwner:=nil;
  FBackground:=$00333333;
  FBorder:=clBtnFace;
  FDate:=clWhite;
  FHands:=clNavy;
  FNumbers:=clBlue;
end;

procedure TClockColors.Assign(Source: TPersistent);
begin
  Background:=TClockColors(Source).Background;
  Border:=TClockColors(Source).Border;
  Date:=TClockColors(Source).Date;
  Hands:=TClockColors(Source).Hands;
  Numbers:=TClockColors(Source).Numbers;
end;

procedure TClockColors.SetBackground(Value: TColor);
begin
  if Value<>FBackground then begin
    FBackground:=Value;
    if Assigned(FOwner) then
      FOwner.ApplyNewColors;
  end;
end;

procedure TClockColors.SetBorder(Value: TColor);
begin
  if Value<>FBorder then begin
    FBorder:=Value;
    if Assigned(FOwner) then
      FOwner.ApplyNewColors;
  end;
end;

procedure TClockColors.SetDate(Value: TColor);
begin
  if Value<>FDate then begin
    FDate:=Value;
    if Assigned(FOwner) then
      FOwner.ApplyNewColors;
  end;
end;

procedure TClockColors.SetHands(Value: TColor);
begin
  if Value<>FHands then begin
    FHands:=Value;
    if Assigned(FOwner) then
      FOwner.ApplyNewColors;
  end;
end;

procedure TClockColors.SetNumbers(Value: TColor);
begin
  if Value<>FNumbers then begin
    FNumbers:=Value;
    if Assigned(FOwner) then
      FOwner.ApplyNewColors;
  end;
end;

{ Komponente TSRClock }

constructor TSRClock.Create(AOwner: TComponent);
var ADateTime : TDateTime;
begin
  inherited Create(AOwner);

  FBuffer:=TBitmap.Create;
  FColors:=TClockColors.Create(Self);
  FImgAlarm:=TBitmap.Create;
  FImgAlarm.Handle:=LoadBitmap(HInstance, 'BELL');
  FTransColorALarm:=FImgAlarm.Canvas.Pixels[0, 0];
  FImgKind:=TBitmap.Create;
  FImgKind.Handle:=LoadBitmap(HInstance, 'STOPWATCH');
  FTransColorKind:=FImgKind.Canvas.Pixels[0, 0];

  FUpdateInterval:=1000;
  FTimer:=TTimer.Create(self);
  FTimer.Enabled:=false;
  FTimer.Interval:=FUpdateInterval;
  FTimer.OnTimer:=AutoUpdateClock;

  ADateTime:=Now;
  FDate:=trunc(ADateTime);
  SetTime(ADateTime);

  FAutoUpdate:=false;
  FBevelStyle:=cbNone;
  FBevelWidth:=1;
  FBorderWidth:=1;
  FLEDContrast:=6;
  FSegmentOffColor:=GetIntermediateColor(FColors.Numbers, FColors.Background, FLEDContrast);
  FDigitShapeColor:=GetIntermediateColor(FColors.Background, clBlack, FShapeContrast);
  if GetLuminance(FColors.Background)>0.3 then
    FTickColor:=GetIntermediateColor(FColors.Background, clBlack, 5)
  else
    FTickColor:=GetIntermediateColor(FColors.Background, clWhite, 5);
  FOptions:=[coDrawDigitShapes, coShowAlarm, coShowKind, coShowSeconds, coShowTicks];
  FLineWidth:=3;
  FRunning:=false;
  FMarkingStyle:=msQuarters;
  FSummertime:=IsSummertime(Now);
  FStyle:=csClassic;
  FSeparator:=dsDoublePoint;

  SetBounds(0, 0, 80, 80);
  AdjustBounds;
end;

destructor TSRClock.Destroy;
begin
  if assigned(FBuffer) then
    FBuffer.Free;
  if assigned(FColors) then
    FColors.Free;
  if assigned(FImgAlarm) then
    FImgAlarm.Free;
  if assigned(FImgKind) then
    FImgKind.Free;
  if assigned(FTimer) then
    FTimer.Free;

  inherited Destroy;
end;

procedure TSRClock.AdjustBounds;
begin
  if FStyle=csDigital then begin
    if (coShowKind in FOptions) or (coShowAlarm in FOptions) and assigned(FImgAlarm) then
      FDisplayWidth:=Self.Width-FImgAlarm.Width-FBorderWidth
    else
      FDisplayWidth:=Self.Width;
    if CoShowDate in FOptions then
      FDisplayHeight:=Self.Height-FTextHeight
    else
      FDisplayHeight:=Self.Height;
    FDisplayWidth:=FDisplayWidth-(2*FBorderWidth);
    if not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
  end
  else begin
    FDisplayWidth:=Self.Width;
    FDisplayHeight:=Self.Height;
  end;
  FBuffer.Width:=Self.Width;
  FBuffer.Height:=Self.Height;
  if not (csLoading in ComponentState) then
    Invalidate;
end;

procedure TSRClock.AssignColors(seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);
begin
  if s1 then
    FSegCl[seg, 1]:=FColors.Numbers
  else
    FSegCl[seg, 1]:=FSegmentOffColor;
  if s2 then
    FSegCl[seg, 2]:=FColors.Numbers
  else
    FSegCl[seg, 2]:=FSegmentOffColor;
  if s3 then
    FSegCl[seg, 3]:=FColors.Numbers
  else
    FSegCl[seg, 3]:=FSegmentOffColor;
  if s4 then
    FSegCl[seg, 4]:=FColors.Numbers
  else
    FSegCl[seg, 4]:=FSegmentOffColor;
  if s5 then
    FSegCl[seg, 5]:=FColors.Numbers
  else
    FSegCl[seg, 5]:=FSegmentOffColor;
  if s6 then
    FSegCl[seg, 6]:=FColors.Numbers
  else
    FSegCl[seg, 6]:=FSegmentOffColor;
  if s7 then
    FSegCl[seg, 7]:=FColors.Numbers
  else
    FSegCl[seg, 7]:=FSegmentOffColor;
end;

procedure TSRClock.ApplyNewColors;
begin
  FSegmentOffColor:=GetIntermediateColor(FColors.Numbers, FColors.Background, FLEDContrast);
  FDigitShapeColor:=GetIntermediateColor(FColors.Background, clBlack, FShapeContrast);
  if GetLuminance(FColors.Background)>0.3 then
    FTickColor:=GetIntermediateColor(FColors.Background, clBlack, 5)
  else
    FTickColor:=GetIntermediateColor(FColors.Background, clWhite, 5);
  if (FStyle=csDigital) and not (csLoading in ComponentState) then
    CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
  Invalidate;
end;

procedure TSRClock.AutoUpdateClock(Sender: TObject);
var ADateTime : TDateTime;
begin
  if ((Kind=ckRealTime) and FAutoUpdate) or ((Kind=ckStopWatch) and FRunning) then begin
    ADateTime:=Now;
    if Kind=ckStopWatch then
      SetTime(ADateTime-FTimeOffset)
    else
      SetTime(ADateTime);
    if Assigned(FOnTimer) then
      FOnTimer(Self);
    if FDate<>trunc(ADateTime) then
      FDate:=trunc(ADateTime);
    if FAlarmActive and assigned(FOnAlarm) and
     (frac(FTime)>=frac(FAlarmTime)) and (trunc(FAlarmTime)=trunc(ADateTime)) then
      FOnAlarm(self);
  end;
end;

procedure TSRClock.CMEnabledChanged(var Message: TWmNoParams);
begin
  inherited;
  FTimer.Enabled:=Self.Enabled;
  Invalidate;
end;

procedure TSRClock.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) and assigned(FBuffer) then begin
    FTextHeight:=FBuffer.Canvas.TextHeight('Mp');
    if (coShowDate in FOptions) and (FStyle=csDigital) then
      AdjustBounds;
  end;
end;

procedure TSRClock.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TSRClock.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TSRClock.CMVisibleChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

procedure TSRClock.CreateDigitBitmaps(AWidth, AHeight: integer);
var
  TL,TR,TBL,TBR,
  ML,MTL,MTR,MR,
  MBL,MBR,BL,BTL,
  BTR,BR         : TPoint;
  c,LineW,DigitW : integer;
begin
  if assigned(FBuffer) then
    FTextHeight:=FBuffer.Canvas.TextHeight('Mp');
  if FBevelStyle<>cbNone then begin
    AWidth:=AWidth-(2*FBevelWidth);
    AHeight:=AHeight-(2*FBevelWidth);
  end;
  if FBorderWidth>0 then begin
    AWidth:=AWidth-(2*FBorderWidth);
    AHeight:=AHeight-(2*FBorderWidth);
  end;
  if (AWidth>4) and (AHeight>4) then begin
    LineW:=FLineWidth+2;
    DigitW:=round((AWidth-12)/8);
    FDigitHeight:=AHeight-4;
    { Polygonpunkte zuweisen }
    TL.x:=0;
    TL.y:=0;
    TR.x:=DigitW-1;
    TR.y:=0;
    TBL.x:=LineW - 1;
    TBL.y:=LineW -1;
    TBR.x:=DigitW - LineW;
    TBR.y:=TBL.y;
    ML.x:=0;
    ML.y:=FDigitHeight div 2;
    MTL.x:=TBL.x;
    MTL.y:=ML.y - (LineW div 2);
    MTR.x:=TBR.x;
    MTR.y:=MTL.y;
    MR.x:=TR.x;
    MR.y:=ML.y;
    MBL.x:=TBL.x;
    MBL.y:=ML.y + (LineW div 2);
    MBR.x:=MTR.x; MBR.y:=MBL.y;
    BL.x:=0;
    BL.y:=FDigitHeight - 1;
    BR.x:=TR.x;
    BR.y:=BL.y;
    BTL.x:=TBL.x;
    BTL.y:=FDigitHeight - LineW;
    BTR.x:=TBR.x;
    BTR.y:=BTL.y;

    { Segmentfarben zuweisen }
    AssignColors(0, true, true, true, false, true, true, true);
    AssignColors(1, false, false, true, false, false, true, false);
    AssignColors(2, true, false, true, true, true, false, true);
    AssignColors(3, true, false, true, true, false, true, true);
    AssignColors(4, false, true, true, true, false, true, false);
    AssignColors(5, true, true, false, true, false, true, true);
    AssignColors(6, false, true, false, true, true, true, true);
    AssignColors(7, true, false, true, false, false, true, false);
    AssignColors(8, true, true, true, true, true, true, true);
    AssignColors(9, true, true, true, true, false, true, true);

    { Bitmap erstellen }
    for c:=0 to 9 do begin
      FDigit[c].Free;
      FDigit[c]:=TBitmap.Create;
      FDigit[c].Width:=DigitW;
      FDigit[c].Height:=FDigitHeight;
      with FDigit[c].Canvas do begin
        if (coDrawDigitShapes in FOptions) then
          Pen.Color:=FDigitShapeColor
        else
          Pen.Color:=FColors.BackGround;
        Brush.Color:=FColors.BackGround;
        Brush.Style:=bsSolid;
        Pen.Width:=1;
        Rectangle(TL.x, TL.y, BR.x+1, BR.y+1);
        { Segment 1 }
        Brush.Color:=FSegCl[c, 1];
        Polygon([TL, TR, TBR, TBL]);
        { Segment 2 }
        Brush.Color:=FSegCl[c, 2];
        Polygon([TL, TBL, MTL, ML]);
        { Segment 3 }
        Brush.Color:=FSegCl[c, 3];
        Polygon([TR, MR, MTR, TBR]);
        { Segment 4 }
        Brush.Color:=FSegCl[c, 4];
        Polygon([ML, MTL, MTR, MR, MBR, MBL]);
        { Segment 5 }
        Brush.Color:=FSegCl[c, 5];
        Polygon([ML, MBL, BTL, BL]);
        { Segment 6 }
        Brush.Color:=FSegCl[c, 6];
        Polygon([MR, BR, BTR, MBR]);
        { Segment 7 }
        Brush.Color:=FSegCl[c, 7];
        Polygon([BL, BTL, BTR, BR]);
      end;
    end;
  end;
end;

procedure TSRClock.Loaded;
begin
  inherited Loaded;

  if assigned(FBuffer) then begin
    FTextHeight:=FBuffer.Canvas.TextHeight('Mp');
    AdjustBounds;
  end;
end;

procedure TSRClock.Paint;
var SrcRect,
    DispRect,
    CltRect     : TRect;
    Center,
    ElCenter    : TPoint;
    CharNum,i   : byte;
    XRadius,
    YRadius,
    ElXRadius,
    ElYRadius,
    Grad        : word;
    HorzOffs,
    VertOffs,
    ImgLeft,
    ImgTop,
    SepLeft,
    SepTop,
    AWidth,
    AHeight,
    CharWidth,
    CharHeight,
    CharLeft,
    CharTop,c,
    SepPosition : integer;
    outText     : string;
    ElXAbstand,
    ElYAbstand  : double;

  procedure AlTextOut(const X,Y:integer;Text:string;const HAlign,VAlign:TAlignment);
  var LeftOut,TopOut : integer;
  begin
    with FBuffer.Canvas do begin
      LeftOut:=X;
      if HAlign=taRightJustify then
        LeftOut:=X-TextWidth(Text);
      if HAlign=taCenter then
        LeftOut:=X-(TextWidth(Text) div 2);
      TopOut:=Y;
      if VAlign=taRightJustify then
        TopOut:=Y-FTextHeight;
      if VAlign=taCenter then
        TopOut:=Y-(FTextHeight div 2);
      TextOut(LeftOut, TopOut, Text);
    end;
  end; { AlTextOut }

  procedure DrawBevel(var ARect:TRect);
  var i,Diff : integer;
  begin
    with FBuffer.Canvas do begin
      Pen.Style:=psSolid;
      if FBevelStyle=cbLowered then
        Pen.Color:=clBtnShadow
      else begin
        if FBevelStyle=cbRaised then
          Pen.Color:=clBtnHighlight
        else
          Pen.Color:=clWindowFrame;
      end;
      if FStyle=csDigital then begin
        Diff:=0;
        Pen.Width:=1;
        for i:=1 to FBevelWidth do begin
          MoveTo(ARect.Right-i, ARect.Top+i-1);
          LineTo(ARect.Left+i-1, ARect.Top+i-1);
          LineTo(ARect.Left+i-1, ARect.Bottom-i);
        end;
      end
      else begin
        Diff:=FBevelWidth div 2;
        InflateRect(ARect, -Diff, -Diff);
        Pen.Width:=FBevelWidth;
        Arc(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1,
            ARect.Right-1, ARect.Top, ARect.Left, ARect.Bottom-1);
      end;
      if FBevelStyle=cbLowered then
        Pen.Color:=clBtnHighlight
      else begin
        if FBevelStyle=cbRaised then
          Pen.Color:=clBtnShadow
        else
          Pen.Color:=clWindowFrame
      end;
      if FStyle=csDigital then begin
        for i:=1 to FBevelWidth do begin
          MoveTo(ARect.Right-i, ARect.Top+i-1);
          LineTo(ARect.Right-i, ARect.Bottom-i);
          LineTo(ARect.Left+i-1, ARect.Bottom-i);
        end;
        InflateRect(ARect, -FBevelWidth, -FBevelWidth);
      end
      else begin
        Arc(ARect.Left, ARect.Top, ARect.Right-1, ARect.Bottom-1,
            ARect.Left, ARect.Bottom-1, ARect.Right-1, ARect.Top);
        InflateRect(ARect, -Diff-1, -Diff-1);
      end;
    end;
  end; { DrawBevel }

  procedure DrawDate(const ATop:integer);
  var DateText : string;
  begin
    { Datum zeichnen }
    with FBuffer.Canvas do begin
      Brush.Style:=bsClear;
      Font.Color:=FColors.Date;
      DateText:=DateToStr(FDate);
      TextOut(DispRect.Left+((AWidth-TextWidth(DateText)) div 2), ATop, DateText);
    end;
  end; { DrawDate }

  procedure DrawAlarmSymbol(const ALeft,ATop:integer;const BGColor:TColor);
  var ARect : TRect;
  begin
    { Alarmsymbol }
    FBuffer.Canvas.Brush.Color:=BGColor;
    FBuffer.Canvas.Brush.Style:=bsSolid;
    ARect:=Rect(ALeft, ATop, ALeft+FImgAlarm.Width, ATop+FImgAlarm.Height);
    SrcRect:=Rect(0, 0, FImgAlarm.Width, FImgAlarm.Height);
    FBuffer.Canvas.BrushCopy(ARect, FImgAlarm, SrcRect, FTransColorAlarm);
  end; { DrawAlarmSymbol }

  procedure DrawKindSymbol(const ALeft,ATop:integer;const BGColor:TColor);
  var ARect : TRect;
  begin
    { "Kind"-Symbol }
    FBuffer.Canvas.Brush.Color:=BGColor;
    FBuffer.Canvas.Brush.Style:=bsSolid;
    ARect:=Rect(ALeft, ATop, ALeft+FImgKind.Width, ATop+FImgKind.Height);
    SrcRect:=Rect(0, 0, FImgKind.Width, FImgKind.Height);
    FBuffer.Canvas.BrushCopy(ARect, FImgKind, SrcRect, FTransColorKind);
  end; { DrawKindSymbol }

begin
  {$IFNDEF SR_Delphi5_Up}
  { SizeChange bis Delphi 4 (ohne Resize-Handler) }
  if (Self.Width<>FOldWidth) or (Self.Height<>FOldHeight) then begin
    FOldWidth:=Self.Width;
    FOldHeight:=Self.Height;
    AdjustBounds;
  end;
  {$ENDIF}
  if assigned(FBuffer) and (FDisplayWidth>0) and (FDisplayHeight>0) then begin
    CltRect:=GetClientRect;
    with FBuffer.Canvas do begin

      { Border }
      Font.Assign(Self.Font);
      Brush.Color:=FColors.Border;
      Brush.Style:=bsSolid;
      Pen.Color:=FColors.Border;
      Rectangle(CltRect.Left, CltRect.Top, CltRect.Right, CltRect.Bottom);
      DispRect:=CltRect;

      if FBevelStyle<>cbNone then
        { Bevel }
        DrawBevel(CltRect);

      HorzOffs:=CltRect.Left-DispRect.Left;
      VertOffs:=CltRect.Top-DispRect.Top;
      DispRect:=Rect(HorzOffs, VertOffs,
                     CltRect.Left+FDisplayWidth-(2*HorzOffs),
                     CltRect.Top+FDisplayHeight-(2*VertOffs));
      AWidth:=DispRect.Right-DispRect.Left;
      AHeight:=DispRect.Bottom-DispRect.Top;
      Center.X:=(DispRect.Right-DispRect.Left) div 2;
      Center.Y:=(DispRect.Bottom-DispRect.Top) div 2;

      if FStyle=csDigital then begin
        { Digitaluhr }
        { Hintergrund }
        Brush.Color:=FColors.Background;
        Pen.Color:=FColors.Background;
        Rectangle(CltRect.Left, CltRect.Top, CltRect.Right, CltRect.Bottom);
        InflateRect(DispRect, -FBorderWidth, -FBorderWidth);

        { Digits und Seperator }
        try
          outText:=FormatDateTime('hh.mm.ss', FTime);
        except
          outText:='';
        end;
        CharWidth:=round((AWidth-4)/8);
        CharTop:=DispRect.Top+2;
        CharLeft:=DispRect.Left+2;
        Brush.Color:=FColors.Numbers;
        Pen.Color:=FColors.Numbers;
        for CharNum:=1 to 8 do begin
          if outText[CharNum]='.' then begin
            { Separator }
            Pen.Width:=1;
            if coDrawDigitShapes in FOptions then
              Pen.Color:=FDigitShapeColor
            else
              Pen.Color:=FColors.Background;
            SepLeft:=CharLeft+round((AWidth-12)/16)-3;
            if (FSeparator=dsDoublePoint) or (FSeparator=dsSemicolon) then begin
              SepTop:=CharTop+(FDigitHeight div 3)-2;
              Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);
            end;
            case FSeparator of
              dsDoublePoint,
              dsSemicolon   : SepTop:=CharTop+(FDigitHeight*2 div 3)-2;
              dsPoint       : SepTop:=CharTop+FDigitHeight-FLineWidth-1;
              dsComma       : SepTop:=CharTop+FDigitHeight-(FLineWidth*2)-3;
              dsApostrophe  : SepTop:=CharTop+2;
              else            SepTop:=CharTop+((FDigitHeight-FLineWidth) div 2);
            end;
            if (FSeparator=dsSemicolon) or (FSeparator=dsComma) or (FSeparator=dsApostrophe) then
              Polygon([Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth div 2)),
                       Point(SepLeft+FLineWidth, SepTop+(FLineWidth div 2)),
                       Point(SepLeft+(FLineWidth div 2), SepTop+(FLineWidth * 2))]);
            if (FSeparator=dsDoublePoint) or (FSeparator=dsPoint) or
             (FSeparator=dsSemicolon) or (FSeparator=dsComma) or (FSeparator=dsApostrophe) then
              Ellipse(SepLeft, SepTop, SepLeft+FLineWidth+1, SepTop+FLineWidth+1);
            if FSeparator=dsHyphen then begin
              SepLeft:=CharLeft+(CharWidth div 6);
              Rectangle(SepLeft, SepTop, SepLeft+round(CharWidth/3*2), SepTop+FLineWidth+1);
            end;
          end
          else
            { Digit }
            Draw(CharLeft, CharTop, FDigit[StrToInt(outText[CharNum])]);
          inc(CharLeft, CharWidth);
        end;
        if coShowDate in FOptions then
          DrawDate(CltRect.Bottom-FTextHeight-FBorderWidth);
        if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then
          DrawAlarmSymbol(CltRect.Right-FImgAlarm.Width-FBorderWidth,
                          CharTop, FColors.Background);
        if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then
          DrawKindSymbol(CltRect.Right-FImgKind.Width-FBorderWidth,
                         CharTop+FDigit[0].Height-FImgKind.Height, FColors.Background);
        { Variablen initialisieren }
        XRadius:=0;
        YRadius:=0;
      end
      else begin
        { Analoguhr }
        { Hintergrund }
        Pen.Color:=FColors.Background;
        Brush.Color:=FColors.Background;
        Brush.Style:=bsSolid;
        Ellipse(DispRect.Left, DispRect.Top, DispRect.Right-1, DispRect.Bottom-1);
        XRadius:=(DispRect.Right-DispRect.Left) div 2;
        YRadius:=(DispRect.Bottom-DispRect.Top) div 2;
      end;

      if FStyle=csClassic then begin
        if coShowDate in FOptions then
          DrawDate(Center.Y+((YRadius-FTextHeight) div 2));
        if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then begin
          if (coShowKind in FOptions) and assigned(FImgKind) then
            DrawAlarmSymbol(Center.X-((XRadius-FImgAlarm.Width) div 2)-FImgAlarm.Width,
                            Center.Y-(FImgAlarm.Height div 2), FColors.Background)
          else
            DrawAlarmSymbol(Center.X-(FImgAlarm.Width div 2),
                            Center.Y-((YRadius-FImgAlarm.Height) div 2)-FImgAlarm.Height, FColors.Background)
        end;
        if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then begin
          if (coShowAlarm in FOptions) and assigned(FImgAlarm) then
            DrawKindSymbol(Center.X+((XRadius-FImgKind.Width) div 2),
                           Center.Y-(FImgKind.Height div 2), FColors.Background)
          else
            DrawKindSymbol(Center.X-(FImgKind.Width div 2),
                           Center.Y-((YRadius-FImgKind.Height) div 2)-FImgKind.Height, FColors.Background)
        end;
        { Markierungen }
        if coShowTicks in FOptions then begin
          Pen.Color:=FTickColor;
          Pen.Width:=1;
          Pen.Style:=psSolid;
          for i:=1 to 12 do begin
            MoveTo(XCoord(Center.X-1, XRadius-1, 360-(i*30)),
                   YCoord(Center.Y-1, YRadius-1, 360-(i*30)));
            LineTo(XCoord(Center.X-1, XRadius-5, 360-(i*30)),
                   YCoord(Center.Y-1, YRadius-5, 360-(i*30)));
          end;
        end;

        { Ziffern }
        if FMarkingStyle<>msNone then begin
          Brush.Style:=bsClear;
          Font.Color:=FColors.Numbers;
          for i:=1 to 12 do begin
            if (FMarkingStyle=msAll) or ((FMarkingStyle=msQuarters) and ((i mod 3)=0)) then begin
              outText:=IntToStr(i);
              AlTextOut(XCoord(Center.X, XRadius-TextWidth(outText)-2, 360-(i*30)),
                        YCoord(Center.Y, YRadius-(FTextHeight div 2)-4, 360-(i*30)),
                        outText, taCenter, taCenter);
            end;
          end;
        end;

        { Zeiger }
        Pen.Color:=FColors.Border;
        Brush.Color:=FColors.Border;
        Brush.Style:=bsSolid;
        Ellipse(Center.X-(XRadius div 10), Center.Y-(YRadius div 10),
                Center.X+(XRadius div 10), Center.Y+(YRadius div 10));
        Pen.Color:=FColors.Hands;
        { Stunden }
        Pen.Width:=4;
        Grad:=360-((FHour mod 12)*30);
        Grad:=Grad-round(30*(FMinute/60));
        MoveTo(Center.X, Center.Y);
        LineTo(XCoord(Center.X, XRadius div 2,Grad),
               YCoord(Center.Y, YRadius div 2,Grad));
        { Minuten }
        Pen.Width:=2;
        MoveTo(Center.X, Center.Y);
        LineTo(XCoord(Center.X, XRadius-4,360-(FMinute*6)),
               YCoord(Center.Y, YRadius-4,360-(FMinute*6)));
        { Sekunden }
        if coShowSeconds in FOptions then begin
          Pen.Width:=1;
          Pen.Color:=FColors.Numbers;
          MoveTo(XCoord(Center.X, 5, 180-(FSecond*6)),
                 YCoord(Center.Y, 5, 180-(FSecond*6)));
          LineTo(XCoord(Center.X, XRadius-4, 360-(FSecond*6)),
                 YCoord(Center.Y, YRadius-4, 360-(FSecond*6)));
        end;
      end;
      if FStyle=csMovingPoints then begin
        Brush.Color:=FColors.Border;
        Brush.Style:=bsSolid;
        ElXRadius:=((XRadius-(XRadius div 5)) div 2)-2;
        ElYRadius:=((YRadius-(YRadius div 5)) div 2)-2;
        Ellipse(Center.X-ElXRadius, Center.Y-ElYRadius,
                Center.X+ElXRadius, Center.Y+ElYRadius);
        { Stunden und Minuten }
        if (FMinute=0) or not (coFadingColor in FOptions) then
          Brush.Color:=FColors.Hands
        else
          Brush.Color:=GetIntermediateColor(FColors.Hands, FColors.Background, round(7-(7/(60-FMinute))));
        Pen.Color:=Brush.Color;
        Grad:=360-((FHour mod 12)*30);
        Grad:=Grad-round(30*(FMinute/60));
        ElXRadius:=XRadius div 5;
        ElYRadius:=YRadius div 5;
        ElXAbstand:=(XRadius-ElXRadius)/120;
        ElYAbstand:=(YRadius-ElYRadius)/120;
        if FMinute=0 then begin
          ElCenter.X:=XCoord(Center.X, XRadius-2, Grad);
          ElCenter.Y:=YCoord(Center.Y, YRadius-2, Grad);
        end
        else begin
          ElCenter.X:=XCoord(Center.X, XRadius-2-round((60-FMinute)*ElXAbstand), Grad);
          ElCenter.Y:=YCoord(Center.Y, YRadius-2-round((60-FMinute)*ElYAbstand), Grad);
        end;
        Pie(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
            ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius,
            XCoord(ElCenter.X, ElXRadius, Grad+135), YCoord(ElCenter.Y, ElYRadius, Grad+135),
            XCoord(ElCenter.X, ElXRadius, Grad-135), YCoord(ElCenter.Y, ElYRadius, Grad-135));
        { Sekunden }
        if coShowSeconds in FOptions then begin
          Brush.Color:=FColors.Numbers;
          Pen.Color:=Brush.Color;
          ElXRadius:=ElXRadius div 3;
          ElYRadius:=ElYRadius div 3;
          ElCenter.X:=XCoord(Center.X, (XRadius div 3), 360-(FSecond*6));
          ElCenter.Y:=YCoord(Center.Y, (YRadius div 3), 360-(FSecond*6));
          Ellipse(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
                  ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius);
        end;
        if coShowDate in FOptions then
          DrawDate(Center.Y+((YRadius-FTextHeight) div 2));
        if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then begin
          if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then
            DrawAlarmSymbol(Center.X-FImgAlarm.Width-2,
                            Center.Y-(FImgAlarm.Height div 2), FColors.Border)
          else
            DrawAlarmSymbol(Center.X-(FImgAlarm.Width div 2),
                            Center.Y-(FImgAlarm.Height div 2), FColors.Border);
        end;
        if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then begin
          if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then
            DrawKindSymbol(Center.X+2,
                           Center.Y-(FImgKind.Height div 2), FColors.Border)
          else
            DrawKindSymbol(Center.X-(FImgKind.Width div 2),
                           Center.Y-(FImgKind.Height div 2), FColors.Border);
        end;
      end;
      if FStyle=csPieSlice then begin
        if (FMinute=0) or not (coFadingColor in FOptions) then
          Brush.Color:=FColors.Hands
        else
          Brush.Color:=GetIntermediateColor(FColors.Hands, FColors.Background, round(7-(7/(60-FMinute))));
        Pen.Color:=Brush.Color;
        { Stunden und Minuten }
        ElXAbstand:=(XRadius-(XRadius div 3)-4)/60;
        ElYAbstand:=(YRadius-(YRadius div 3)-4)/60;
        if FMinute=0 then begin
          ElXRadius:=(XRadius div 3)+round(ElXAbstand*60);
          ElYRadius:=(YRadius div 3)+round(ElYAbstand*60);
        end
        else begin
          ElXRadius:=(XRadius div 3)+round(ElXAbstand*FMinute);
          ElYRadius:=(YRadius div 3)+round(ElYAbstand*FMinute);
        end;
        Grad:=360-((FHour mod 12)*30);
        Grad:=Grad-round(30*(FMinute/60));
        Pie(Center.X-ElXRadius, Center.Y-ElYRadius,
            Center.X+ElXRadius, Center.Y+ElYRadius,
            XCoord(Center.X, ElXRadius, Grad), YCoord(Center.Y, ElYRadius, Grad),
            XCoord(Center.X, ElXRadius, 0), YCoord(Center.Y, ElYRadius, 0));
        Brush.Color:=FColors.Border;
        Brush.Style:=bsSolid;
        Pen.Color:=Brush.Color;
        Ellipse(Center.X-(XRadius div 3), Center.Y-(YRadius div 3),
                Center.X+(XRadius div 3), Center.Y+(YRadius div 3));
        { Sekunden }
        if coShowSeconds in FOptions then begin
          Brush.Color:=FColors.Numbers;
          Pen.Color:=Brush.Color;
          ElXRadius:=XRadius div 10;
          ElYRadius:=YRadius div 10;
          ElCenter.X:=XCoord(Center.X, (XRadius div 3), 360-(FSecond*6));
          ElCenter.Y:=YCoord(Center.Y, (YRadius div 3), 360-(FSecond*6));
          Ellipse(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
                  ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius);
        end;
        if coShowDate in FOptions then
          DrawDate(Center.Y+((YRadius-FTextHeight) div 2));
        if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then begin
          if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then
            DrawAlarmSymbol(Center.X-FImgAlarm.Width-2,
                            Center.Y-(FImgAlarm.Height div 2), FColors.Border)
          else
            DrawAlarmSymbol(Center.X-(FImgAlarm.Width div 2),
                            Center.Y-(FImgAlarm.Height div 2), FColors.Border);
        end;
        if (coShowKind in FOptions) and assigned(FImgKind) and (Kind=ckStopWatch) then begin
          if (coShowAlarm in FOptions) and assigned(FImgAlarm) and FAlarmActive then
            DrawKindSymbol(Center.X+2,
                           Center.Y-(FImgKind.Height div 2), FColors.Border)
          else
            DrawKindSymbol(Center.X-(FImgKind.Width div 2),
                           Center.Y-(FImgKind.Height div 2), FColors.Border);
        end;
      end;
    end;
    Canvas.Draw(0, 0, FBuffer);
  end;
end;

procedure TSRClock.Reset;
begin
  FTimeOffset:=Now;
  SetTime(0);
end;

{$IFDEF SR_Delphi5_Up}
procedure TSRClock.Resize;
begin
  AdjustBounds;

  inherited;
end;
{$ENDIF}

procedure TSRClock.SetAlarmActive(Value: boolean);
begin
  if FAlarmActive<>Value then begin
    FAlarmActive:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetAutoUpdate(Value: boolean);
begin
  if (FAutoUpdate<>Value) and (FKind=ckRealTime) then begin
    FAutoUpdate:=Value;
    FTimer.Enabled:=FAutoUpdate;
  end;
end;

procedure TSRClock.SetBevelStyle(Value: TClockBevel);
begin
  if Value<>FBevelStyle then begin
    FBevelStyle:=Value;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    Invalidate;
  end;
end;

procedure TSRClock.SetBevelWidth(Value: TBevelWidth);
begin
  if Value<>FBevelWidth then begin
    FBevelWidth:=Value;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    Invalidate;
  end;
end;

procedure TSRClock.SetBorderWidth(Value: TBorderWidth);
begin
  if Value<>FBorderWidth then begin
    FBorderWidth:=Value;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then begin
      AdjustBounds;
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    end;
    Invalidate;
  end;
end;

procedure TSRClock.SetColors(Value: TClockColors);
begin
  with FColors do begin
    Background:=Value.Background;
    Border:=Value.Border;
    Date:=Value.Date;
    Hands:=Value.Hands;
    Numbers:=Value.Numbers;
  end;
  ApplyNewColors;
end;

procedure TSRClock.SetKind(Value: TClockKind);
begin
  if Value<>FKind then begin
    FKind:=Value;
    if FKind=ckRealTime then begin
      FTimer.Enabled:=FAutoUpdate;
      SetTime(Now);
    end
    else begin
      FRunning:=false;
      FTimeOffset:=Now;
      SetTime(0);
    end;
  end;
end;

procedure TSRClock.SetLEDContrast(Value: TContrast);
begin
  if FLEDContrast<>Value then begin
    FLEDContrast:=Value;
    FSegmentOffColor:=GetIntermediateColor(FColors.Numbers, FColors.Background, FLEDContrast);
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    Invalidate;
  end;
end;

procedure TSRClock.SetLineWidth (Value: byte);
begin
  if (FLineWidth<>Value) and (Value>0) then begin
    FLineWidth:=Value;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    Invalidate;
  end;
end;

procedure TSRClock.SetOptions(Value: TClockOptions);
begin
  if Value<>FOptions then begin
    FOptions:=Value;
    AdjustBounds;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
  end;
end;

procedure TSRClock.SetSeparator(Value: TSeparator);
begin
  if Value<>FSeparator then begin
    FSeparator:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetMarkingStyle(Value: TMarkingStyle);
begin
  if Value<>FMarkingStyle then begin
    FMarkingStyle:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetStyle(Value: TClockStyle);
begin
  if Value<>FStyle then begin
    FStyle:=Value;
    AdjustBounds;
    if (FStyle=csDigital) and not (csLoading in ComponentState) then
      CreateDigitBitmaps(FDisplayWidth, FDisplayHeight);
    Invalidate;
  end;
end;

procedure TSRClock.SetTime(Value: TTime);
var msec : word;
begin
  if Value<>FTime then begin
    FTime:=Value;
    try
      DecodeTime(FTime, FHour, FMinute, FSecond, msec);
    except
      FHour:=0;
      FMinute:=0;
      FSecond:=0;
    end;
    Invalidate;
  end;
end;

procedure TSRClock.SetUpdateInterval(Value: word);
begin
  if (Value<>FUpdateInterval) and (Value>15) then begin
    FUpdateInterval:=Value;
    FTimer.Interval:=FUpdateInterval;
    Invalidate;
  end;
end;

procedure TSRClock.Start;
begin
  FTimeOffset:=Now-FTime;
  FTimer.Enabled:=true;
  FRunning:=true;
end;

procedure TSRClock.Stop;
begin
  FTimer.Enabled:=false;
  FRunning:=false;
end;

procedure TSRClock.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRClock]);
end;

end.
