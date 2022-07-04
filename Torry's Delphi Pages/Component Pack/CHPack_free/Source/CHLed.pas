unit CHLed;

{ ##############################################################################
  TCHLed

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 12.11.2002    - First Release
                        - ADD: Popupmenu and Buffered
                        - NEW: MouseWheel, MouseWheelDown, MouseWheelUp
  1.0.1 - 15.12.2002    - BUG: repair some memory leaks

  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  _CHClassProperty, _CHClassFunction;

type
  TLedMaskMode = (lmNone, lmClock, lmCustom, lmThousandSep, lmCommaOne, lmCommaTwo, lmCommaThree);
  TLedSeperator = (lsPoint, lsColon);
  TLedAlignment = (laLeft, laCenter, laRight);
  TLedPlaceholder = 0..9;

  TCHCustomLed = class;

  TCHLedSegment = class(TPersistent)
  private
    FOwner : TCHCustomLed;
    FSegmentWidth: Word;
    FSegmentPitch: Word;
    FShowInActive: Boolean;
    FActiveColor: TColor;
    FInActiveColor: TColor;
    procedure SetSegmentPitch(const Value: Word);
    procedure SetSegmentWidth(const Value: Word);
    procedure SetActiveColor(const Value: TColor);
    procedure SetInActiveColor(const Value: TColor);
    procedure SetShowInActive(const Value: Boolean);
  public
    constructor Create(AOwner: TCHCustomLed); virtual;
  protected

  published
    property ActiveColor : TColor read FActiveColor Write SetActiveColor;
    property InActiveColor : TColor read FInActiveColor Write SetInActiveColor;
    property Width : Word read FSegmentWidth Write SetSegmentWidth;
    property Pitch : Word read FSegmentPitch Write SetSegmentPitch;
    property ShowInActive : Boolean read FShowInActive Write SetShowInActive;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHSeperator = class(TPersistent)
  private
    FOwner : TCHCustomLed;
    FSeperator: TLedSeperator;
    FSepWidth: Word;
    procedure SetSeperator(const Value: TLedSeperator);
    procedure SetSepWidth(const Value: Word);
  public
    constructor Create(AOwner: TCHCustomLed); virtual;
  protected

  published
    property Seperator : TLedSeperator read FSeperator write SetSeperator;
    property Width : Word read FSepWidth write SetSepWidth;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHLedMask = class(TPersistent)
  private
    FOwner : TCHCustomLed;
    FLedMask: String;
    FLedMaskMode: TLedMaskMode;
    FLedPitch: Word;
    FLedHeight: Word;
    FLedWidth: Word;
    FLedPlaceholder: TLedPlaceholder;
    procedure SetLedPitch(const Value: Word);
    procedure SetLedMask(const Value: String);
    procedure SetLedMode(const Value: TLedMaskMode);
    procedure SetLedHeight(const Value: Word);
    procedure SetLedWidth(const Value: Word);
    procedure SetLedPlaceholder(const Value: TLedPlaceholder);
  protected

  public
    constructor Create(AOwner: TCHCustomLed); virtual;
  published
    property Mode : TLedMaskMode read FLedMaskMode Write SetLedMode;
    property Mask : String read FLedMask Write SetLedMask;
    property Pitch : Word read FLedPitch Write SetLedPitch;
    property Placeholder : TLedPlaceholder read FLedPlaceholder Write SetLedPlaceholder;
    property Width : Word read FLedWidth Write SetLedWidth;
    property Height : Word read FLedHeight Write SetLedHeight;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHCustomLed = class(TCustomControl)
  private
    FFill : TCHFillB;
    FLedSeperator: TCHSeperator;
    FLedMask : TCHLedMask;
    FLedSegment : TCHLedSegment;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FWheelAccumulator: Integer;

    FLedValue: TCaption;
    FMask: string;
    FBackColor: TColor;
    FMaskMode: TLedMaskMode;
    FAutosize: Boolean;
    FLedAlignment: TLedAlignment;
    FClientRect : TRect;
    FBuffered: Boolean;

    function GetLedLen(sMask : string) : Integer;
    function GetMask : string;
    function GetValue(Mask, Placeholder : string; Sep : Char) : string;
    function GetSeperator : Char;
    procedure DrawLed(XPos, YPos : Word; Value : Char);
    function GetRoundedInt(const Value : Integer; RoundDown : Boolean) : Integer;
    procedure CMTextChanged(var Msg : TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message : TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure SetLedValue(const Value: TCaption);
    procedure SetBackColor(const Value: TColor);
    procedure SetLedAutosize(const Value: Boolean);
    procedure SetLedAlignment(const Value: TLedAlignment);
    procedure UpdateChanges(Sender: TObject);
    procedure SetBuffered(const Value: Boolean);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;

    property Autosize : Boolean read FAutosize Write SetLedAutosize;
    property Alignment : TLedAlignment read FLedAlignment Write SetLedAlignment;
    property Buffered : Boolean read FBuffered write SetBuffered;
    property Color : TColor read FBackColor Write SetBackColor;
    property Fill : TCHFillB read FFill Write FFill;
    property Led : TCHLedMask read FLedMask write FLedMask;
    property Seperator : TCHSeperator read FLedSeperator write FLedSeperator;
    property Segment : TCHLedSegment read FLedSegment Write FLedSegment;
    property Value : TCaption read FLedValue Write SetLedValue;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHLed = class(TCHCustomLed)
  published
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    property Autosize;
    property Alignment;
    property Buffered;
    property Color;
    property Fill;
    property Led;
    property PopupMenu;
    property Seperator;
    property Segment;
    property Value;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHLed]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFill := TCHFillB.Create;
  FFill.OnChange := UpdateChanges;
  FLedSegment := TCHLedSegment.Create(Self);
  FLedSeperator := TCHSeperator.Create(Self);
  FLedMask := TCHLedMask.Create(Self);

  FMaskMode := lmNone;
  FMask := '';
  FLedValue := '0';
  FAutosize := True;
  FBuffered := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomLed.Destroy;
begin
  FFill.Free;
  FLedSeperator.Free;
  FLedSegment.Free;
  FLedMask.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.Paint;
type
  TNumber = set of Char;
var
  nPos : Word;
  nAllWidth, nAllHeight, nLedPitch, nNextPos, nXPos, nYPos : Integer;
  sPlaceStr, sMask, sValue : string;
  sSepChar : Char;
  Number : TNumber;
begin
  inherited Paint;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := FBackColor;
  Canvas.Brush.Color := FBackColor;
  Canvas.Rectangle(0, 0, Width, Height);

  Number := ['0','1','2','3','4','5','6','7','8','9'];
  FClientRect := GetClientRect;
  sPlaceStr := IntToStr(FLedMask.Placeholder);
  sMask := GetMask;
  sSepChar := GetSeperator;
  sValue := GetValue(sMask, sPlaceStr, sSepChar);
  nAllWidth :=  GetLedLen(sMask);
  nLedPitch := FLedMask.FLedPitch;
  nAllHeight := FLedMask.FLedHeight;

  // Autosize
  if FAutosize then
  begin
    Width := nAllWidth;
    Height := nAllHeight;
  end;

  // Transparent
  if (FFill.Transparent) then
  begin
    DrawTransparentBmp(Self, Canvas);
    Canvas.CopyRect(FClientRect, canvas, FClientRect);
    Brush.Style := bsClear;
  end;

  // Alignment Left
  if FLedAlignment = laLeft then
  begin
    nNextPos := 0;
    nYPos := 0;
  end
  // Alignment Right
  else if FLedAlignment = laRight then
  begin
    nNextPos := Width - nAllWidth;
    nYPos := 0;
  end
  // Alignment Center
  else
  begin
    nNextPos := Width - nAllWidth - ((Width - nAllWidth) div 2);
    nYPos := 0;
  end;


  // Draw Led
  for nPos := 1 to Length(sValue) do
  begin
    if sValue[nPos] in Number then
    begin
      nXPos := nNextPos;
      if sValue[nPos +1] in Number then
        nNextPos := nXPos + FLedMask.FLedWidth + nLedPitch
      else
        nNextPos := nXPos + FLedMask.FLedWidth;
    end
    else
    begin
      nXPos := nNextPos;
      nNextPos := nXPos + FLedSeperator.Width ;
    end;

    DrawLed(nXPos, nYPos, sValue[nPos]);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.GetLedLen(sMask : string): Integer;
var
  nAllWidth, nLedPitch, nPos : Integer;
begin
  nAllwidth := 0;
  nLedPitch := FLedMask.FLedPitch;

  for nPos := 1 to Length(sMask) do
  begin
    if sMask[nPos] = 'x' then
    begin
      if sMask[nPos +1] = 'x' then
        nAllWidth := nAllWidth + FLedMask.Width + nLedPitch
      else
        nAllWidth := nAllWidth + FLedMask.Width;
    end
    else if sMask[nPos] = '|' then
      nAllWidth := nAllWidth + FLedSeperator.Width;
  end;

  Result := nAllWidth;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.GetMask: string;
var
  sMask : string;
  I : Integer;
begin
  Result := 'x';
  if Length(FLedMask.Mask) > 0 then
    sMask := FLedMask.Mask
  else
  begin
    for I := 1 to Length(FLedValue) do
      sMask := sMask + 'x';
  end;
  Result := sMask;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.GetValue(Mask, Placeholder : string; Sep : Char) : string;
var
  sValue : string;
  I, J, N, MaskValueCount, nPlaceCount : Integer;
begin
  sValue := '';
  J := 1;
  N := 0;
  MaskValueCount := 0;

  for I := 1 to Length(Mask) do
  begin
    if Mask[I] = 'x' then
      Inc(MaskValueCount);
  end;
  nPlaceCount := MaskValueCount - Length(FLedValue);

  if Length(Mask) > 0 then
  begin
    for I := 1 to Length(Mask) do
    begin
      if Mask[I] = 'x' then
      begin
        Inc(N);
        if N > nPlaceCount then
        begin
          sValue := sValue + FLedValue[J];
          Inc(J);
        end
        else
        begin
          sValue := sValue + Placeholder;
        end;
      end
      else
      begin
        sValue := sValue + string(Sep);
      end;
    end;
  end;

  Result := sValue;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.GetSeperator: Char;
begin
  if FLedSeperator.FSeperator = lsPoint then
    Result := '.'
  else
    Result := ':';
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.DrawLed(XPos, YPos: Word; Value: Char);

//    -- pTx --
//    |        |
//   pLTx     pRTx
//    |        |
//    -- pMx --
//    |        |
//   pLBx     pRBx
//    |        |
//    -- pBx --
type
  TValueSet = set of Char;

var
  pT1, pT2, pT3, pT4,
  pB1, pB2, pB3, pB4,
  pM1, pM2, pM3, pM4, pM5, pM6,
  pLT1, pLT2, pLT3, pLT4,
  pLB1, pLB2, pLB3, pLB4,
  pRT1, pRT2, pRT3, pRT4,
  pRB1, pRB2, pRB3, pRB4,
  pPoint1, pPoint2, pPoint3, pPoint4 : TPoint;

  nDiv, I, nSegWidth, nMidWidth, nSegPitch : Word;

  aSegValue : array[1..9] of TValueSet;
  aSegNum : TValueSet;
  aSegColor : array[1..9] of TColor;
  aSegDraw : array[1..9] of Boolean;
begin
  nSegWidth := FLedSegment.FSegmentWidth;
  nSegPitch := FLedSegment.FSegmentPitch;

  aSegNum := ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'];
  aSegValue[1] := ['2', '3', '5', '6', '7', '8', '9', '0'];
  aSegValue[2] := ['1', '2', '3', '4', '7', '8', '9', '0'];
  aSegValue[3] := ['1', '3', '4', '5', '6', '7', '8', '9', '0'];
  aSegValue[4] := ['2', '3', '5', '6', '8', '0'];
  aSegValue[5] := ['2', '6', '8', '0'];
  aSegValue[6] := ['4', '5', '6', '8', '9', '0'];
  aSegValue[7] := ['2', '3', '4', '5', '6', '8', '9'];

  aSegValue[8] := [':'];
  aSegValue[9] := ['.'];

  for I := 1 to 9 do
    aSegColor[I] := FLedSegment.FInActiveColor;

  for I := 1 to 9 do
    aSegDraw[I] := False;

  if Value in aSegNum then
  begin
    for I := 1 to 7 do
    begin
      if Value in aSegValue[I] then
      begin
        aSegDraw[I] := True;
        aSegColor[I] := FLedSegment.FActiveColor;
      end
      else
      begin
        if FLedSegment.FShowInActive then
          aSegDraw[I] := True
        else if (FFill.Transparent) and (FLedSegment.FShowInActive) then
          aSegDraw[I] := True;
      end;
    end;
  end
  else if Value in aSegValue[8] then
  begin
    aSegColor[8] := FLedSegment.FActiveColor;
    aSegDraw[8] := True;
  end
  else if Value in aSegValue[9] then
  begin
    aSegColor[9] := FLedSegment.FActiveColor;
    aSegDraw[9] := True;
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := FBackColor;
  Canvas.Brush.Color := FBackColor;
  Canvas.Rectangle(XPos, YPos, XPos + FLedMask.Pitch + FLedMask.Width, FLedMask.Height);

  // Top
  pT1.X := XPos + nSegPitch;
  pT1.Y := YPos;
  pT2.X := pT1.X + FLedMask.FLedWidth - ((nSegPitch * 2) +1);
  pT2.Y := pT1.Y;
  pT3.X := pT2.X - nSegWidth + 1;
  pT3.Y := pT1.Y + nSegWidth - 1;
  pT4.X := pT1.X + nSegWidth - 1;
  pT4.Y := pT3.Y;

  if aSegDraw[1] then
  begin
    Canvas.Brush.Color := aSegColor[1];
    Canvas.Pen.Color := aSegColor[1];
    Canvas.Polygon([Point(pT1.X, pT1.Y),
                    Point(pT2.X, pT2.Y),
                    Point(pT3.X, pT3.Y),
                    Point(pT4.X, pT4.Y)]);
  end;


  // Middle
  nMidWidth := GetRoundedInt(nSegWidth, True);
  nDiv := (nMidWidth) div 2;


  pM1.X := XPos  + (nSegPitch);
  pM1.Y := YPos + (FLedMask.FLedHeight div 2);
  pM2.X := pM1.X + nMidWidth;
  pM2.Y := pM1.Y - nDiv;
  pM4.X := (pM1.X + FLedMask.FLedWidth) - (nSegPitch * 2) -1;
  pM4.Y := pM1.Y;
  pM3.X := pM4.X - nMidWidth;
  pM3.Y := pM2.Y;
  pM5.X := pM3.X +1;
  pM5.Y := pM4.Y + nDiv;
  pM6.X := pM2.X -1;
  pM6.Y := pM5.Y;

  if aSegDraw[7] then
  begin
    Canvas.Brush.Color := aSegColor[7];
    Canvas.Pen.Color := aSegColor[7];
    Canvas.Polygon([Point(pM1.X, pM1.Y),
                    Point(pM2.X, pM2.Y),
                    Point(pM3.X, pM3.Y),
                    Point(pM4.X, pM4.Y),
                    Point(pM5.X, pM5.Y),
                    Point(pM6.X, pM6.Y)]);
  end;


  // Bottom
  pB1.X := XPos + nSegPitch;
  pB1.Y := YPos + (FLedMask.FLedHeight - 1);
  pB2.X := pB1.X + FLedMask.FLedWidth - ((nSegPitch * 2) +1);
  pB2.Y := pB1.Y;
  pB3.X := pB2.X - nSegWidth + 1;
  pB3.Y := pB1.Y - nSegWidth + 1;
  pB4.X := pB1.X + nSegWidth - 1;
  pB4.Y := pB3.Y;

  if aSegDraw[4] then
  begin
    Canvas.Brush.Color := aSegColor[4];
    Canvas.Pen.Color := aSegColor[4];
    Canvas.Polygon([Point(pB1.X, pB1.Y),
                    Point(pB2.X, pB2.Y),
                    Point(pB3.X, pB3.Y),
                    Point(pB4.X, pB4.Y)]);
  end;

  // Left Top
  pLT1.X := pT1.X - nSegPitch;
  pLT1.Y := pT1.Y + nSegPitch;
  pLT2.X := pLT1.X + nSegWidth - 1;
  pLT2.Y := pLT1.Y + nSegWidth - 1;
  pLT3.X := pLT2.X;
  pLT3.Y := pM2.Y - (nSegPitch + 1);
  pLT4.X := pLT1.X;
  pLT4.Y := pM1.Y - (nSegPitch + 1);

  if aSegDraw[6] then
  begin
    Canvas.Brush.Color := aSegColor[6];
    Canvas.Pen.Color := aSegColor[6];
    Canvas.Polygon([Point(pLT1.X, pLT1.Y),
                    Point(pLT2.X, pLT2.Y),
                    Point(pLT3.X, pLT3.Y),
                    Point(pLT4.X, pLT4.Y)]);
  end;


  // Left Bottom
  pLB1.X := pT1.X - nSegPitch;
  pLB1.Y := pM1.Y + nSegPitch +1;
  pLB2.X := pLB1.X + nSegWidth -1;
  pLB2.Y := pM6.Y + (nSegPitch +1);
  pLB3.X := pLB2.X;
  pLB3.Y := pB4.Y - nSegPitch;
  pLB4.X := pLB1.X;
  pLB4.Y := pB1.Y - nSegPitch;

  if aSegDraw[5] then
  begin
    Canvas.Brush.Color := aSegColor[5];
    Canvas.Pen.Color := aSegColor[5];
    Canvas.Polygon([Point(pLB1.X, pLB1.Y),
                    Point(pLB2.X, pLB2.Y),
                    Point(pLB3.X, pLB3.Y),
                    Point(pLB4.X, pLB4.Y)]);
  end;


  // Right Top
  pRT1.X := pT2.X + nSegPitch;
  pRT1.Y := pT2.Y + nSegPitch;
  pRT2.X := pRT1.X - nSegWidth + 1;
  pRT2.Y := pRT1.Y + nSegWidth - 1;
  pRT3.X := pRT2.X;
  pRT3.Y := pM3.Y - (nSegPitch + 1);
  pRT4.X := pRT1.X;
  pRT4.Y := pM4.Y - (nSegPitch + 1);

  if aSegDraw[2] then
  begin
    Canvas.Brush.Color := aSegColor[2];
    Canvas.Pen.Color := aSegColor[2];
    Canvas.Polygon([Point(pRT1.X, pRT1.Y),
                    Point(pRT2.X, pRT2.Y),
                    Point(pRT3.X, pRT3.Y),
                    Point(pRT4.X, pRT4.Y)]);
  end;


  // Right Bottom
  pRB1.X := pT2.X + nSegPitch;
  pRB1.Y := pM4.Y + nSegPitch + 1;
  pRB2.X := pRB1.X - nSegWidth + 1;
  pRB2.Y := pM5.Y + (nSegPitch +1);
  pRB3.X := pRB2.X;
  pRB3.Y := pB3.Y - nSegPitch;
  pRB4.X := pRB1.X;
  pRB4.Y := pB2.Y - nSegPitch;

  if aSegDraw[3] then
  begin
    Canvas.Brush.Color := aSegColor[3];
    Canvas.Pen.Color := aSegColor[3];
    Canvas.Polygon([Point(pRB1.X, pRB1.Y),
                    Point(pRB2.X, pRB2.Y),
                    Point(pRB3.X, pRB3.Y),
                    Point(pRB4.X, pRB4.Y)]);
  end;


  // (:)
  if aSegDraw[8] then
  begin
    pPoint1.Y := YPos + Round((FLedMask.FLedHeight / 3) - (nSegWidth / 2));
    pPoint1.X := XPos + Round((FLedSeperator.Width / 2) - (nSegWidth / 2));
    pPoint2.Y := pPoint1.Y;
    pPoint2.X := pPoint1.X + nSegWidth -1;
    pPoint3.Y := pPoint1.Y + nSegWidth -1;
    pPoint3.X := pPoint2.X;
    pPoint4.Y := pPoint3.Y;
    pPoint4.X := pPoint1.X;

    Canvas.Brush.Color := aSegColor[8];
    Canvas.Pen.Color := aSegColor[8];
    Canvas.Polygon([Point(pPoint1.X, pPoint1.Y),
                    Point(pPoint2.X, pPoint2.Y),
                    Point(pPoint3.X, pPoint3.Y),
                    Point(pPoint4.X, pPoint4.Y)]);

    pPoint1.Y := YPos + Round((FLedMask.FLedHeight / 1.5) - (nSegWidth / 2));
    pPoint1.X := XPos + Round((FLedSeperator.Width / 2) - (nSegWidth / 2));
    pPoint2.Y := pPoint1.Y;
    pPoint2.X := pPoint1.X + nSegWidth -1;
    pPoint3.Y := pPoint1.Y + nSegWidth -1;
    pPoint3.X := pPoint2.X;
    pPoint4.Y := pPoint3.Y;
    pPoint4.X := pPoint1.X;

    Canvas.Brush.Color := aSegColor[8];
    Canvas.Pen.Color := aSegColor[8];
    Canvas.Polygon([Point(pPoint1.X, pPoint1.Y),
                    Point(pPoint2.X, pPoint2.Y),
                    Point(pPoint3.X, pPoint3.Y),
                    Point(pPoint4.X, pPoint4.Y)]);
  end;

  // (.)
  if aSegDraw[9] then
  begin
    pPoint1.Y := FLedMask.FLedHeight - nSegWidth;
    pPoint1.X := XPos + Round((FLedSeperator.Width / 2) - (nSegWidth / 2));
    pPoint2.Y := pPoint1.Y;
    pPoint2.X := pPoint1.X + nSegWidth -1;
    pPoint3.Y := pPoint1.Y + nSegWidth -1;
    pPoint3.X := pPoint2.X;
    pPoint4.Y := pPoint3.Y;
    pPoint4.X := pPoint1.X;

    Canvas.Brush.Color := aSegColor[9];
    Canvas.Pen.Color := aSegColor[9];
    Canvas.Polygon([Point(pPoint1.X, pPoint1.Y),
                    Point(pPoint2.X, pPoint2.Y),
                    Point(pPoint3.X, pPoint3.Y),
                    Point(pPoint4.X, pPoint4.Y)]);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.SetLedValue(const Value: TCaption);
begin
  if FLedValue <> Value then
  begin
    FLedValue := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.SetLedAutosize(const Value: Boolean);
begin
  if FAutosize <> Value then
  begin
    FAutosize := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.SetLedAlignment(const Value: TLedAlignment);
begin
  if FLedAlignment <> Value then
  begin
    FLedAlignment := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.SetBuffered(const Value: Boolean);
begin
  FBuffered := Value;
  if FBuffered then
    DoubleBuffered := True
  else
    DoubleBuffered := False;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  Repaint;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLed.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then FWheelAccumulator := -FWheelAccumulator;
        Result := DoMouseWheelDown(Shift, MousePos);
      end
      else
        Result := DoMouseWheelUp(Shift, MousePos);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHCustomLed.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;




{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHCustomLedSegment }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHLedSegment.Create(AOwner: TCHCustomLed);
begin
  inherited Create;
  FOwner := AOwner;

  FActiveColor := clLime;
  FInActiveColor := clGreen;
  FShowInActive := True;
  FSegmentWidth := 3;
  FSegmentPitch := 1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedSegment.SetActiveColor(const Value: TColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedSegment.SetInActiveColor(const Value: TColor);
begin
  if FInActiveColor <> Value then
  begin
    FInActiveColor := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedSegment.SetSegmentPitch(const Value: Word);
begin
  if FSegmentPitch <> Value then
  begin
    FSegmentPitch := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedSegment.SetShowInActive(const Value: Boolean);
begin
  if FShowInActive <> Value then
  begin
    FShowInActive := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedSegment.SetSegmentWidth(const Value: Word);
begin
  if FSegmentWidth <> Value then
  begin
    FSegmentWidth := Value;
    FOwner.Repaint;
  end;
end;




{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHSeperator }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHSeperator.Create(AOwner: TCHCustomLed);
begin
  inherited Create;
  FOwner := AOwner;

  FSeperator := lsPoint;
  FSepWidth := 10;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHSeperator.SetSeperator(const Value: TLedSeperator);
begin
  if FSeperator <> Value then
  begin
    FSeperator := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHSeperator.SetSepWidth(const Value: Word);
begin
  if FSepWidth <> Value then
  begin
    FSepWidth := Value;
    FOwner.Repaint;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHCustomLedMask }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHLedMask.Create(AOwner: TCHCustomLed);
begin
  inherited Create;
  FOwner := AOwner;

  FLedMaskMode := lmNone;
  FLedPitch := 3;
  FLedHeight := 30;
  FLedWidth := 20;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedHeight(const Value: Word);
begin
  if FLedHeight <> Value then
  begin
    FLedHeight := Value;
    FOwner.Height := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedPitch(const Value: Word);
begin
  if FLedPitch <> Value then
  begin
    FLedPitch := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedWidth(const Value: Word);
begin
  if FLedWidth <> Value then
  begin
    FLedWidth := Value;
    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedMask(const Value: string);
type
  TMaskChar = set of Char;
var
  nLen, nPos : Word;
  sInput, sOutput : String;
  MaskChar : TMaskChar;
begin
  MaskChar := ['x', '|'];

  if FLedMask <> Value then
  begin
    sInput := Value;
    nLen := Length(sInput);
    for nPos := 1 to nLen do
    begin
      if sInput[nPos] in MaskChar then
        sOutput := sOutput + sInput[nPos];
    end;
    FLedMask := sOutput;

    if FLedMask = 'xx|xx|xx' then
    begin
      FLedMaskMode := lmClock;
      Self.FOwner.FLedSeperator.FSeperator := lsColon;
    end
    else
    begin
      if FLedMask = 'xxx|xxx' then
        FLedMaskMode := lmThousandSep
      else if FLedMask = 'x|x' then
        FLedMaskMode := lmCommaOne
      else if FLedMask = 'x|xx' then
        FLedMaskMode := lmCommaTwo
      else if FLedMask = 'x|xxx' then
        FLedMaskMode := lmCommaThree
      else if FLedMask = '' then
        FLedMaskMode := lmNone
      else
        FLedMaskMode := lmCustom;

      Self.FOwner.FLedSeperator.FSeperator := lsPoint;
    end;

    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedMode(const Value: TLedMaskMode);
begin
  if FLedMaskMode <> Value then
  begin
    FLedMaskMode := Value;
    if FLedMaskMode <> lmCustom then
    begin
      if FLedMaskMode = lmNone then
        FLedMask := ''
      else if FLedMaskMode = lmClock then
      begin
        FLedMask := 'xx|xx|xx';
        Self.FOwner.FLedSeperator.FSeperator := lsColon;
      end
      else
      begin
        if FLedMaskMode = lmThousandSep then
          FLedMask := 'xxx|xxx'
        else if FLedMaskMode = lmCommaOne then
          FLedMask := 'x|x'
        else if FLedMaskMode = lmCommaTwo then
          FLedMask := 'x|xx'
        else if FLedMaskMode = lmCommaThree then
          FLedMask := 'x|xxx';

        Self.FOwner.FLedSeperator.FSeperator := lsPoint;
      end;
    end;

    FOwner.Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLedMask.SetLedPlaceholder(const Value: TLedPlaceholder);
begin
  if FLedPlaceholder <> Value then
  begin
    FLedPlaceholder := Value;
    FOwner.Repaint;
  end;
end;




function TCHCustomLed.GetRoundedInt(const Value: Integer;
  RoundDown: Boolean): Integer;
var
  nMod, nResult : Integer;
begin
  nMod := Value mod 2;

  if nMod <> 0 then
  begin
    if RoundDown then
      nResult := Value -1
    else
      nResult := Value +1;
  end
  else
    nResult := Value;

  Result := nResult;

end;

end.
