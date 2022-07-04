
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsGrad;
  {-gadient component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, Graphics, Messages,
  EsConst, EsData;

const
  MinColorBands = 2;   {two bands isn't much of a gradient}
  MaxColorBands = 256; {more than 256 colors is not noticeable, and slows painting}

type
  TGradDirection = (dHorizontal, dVertical);
  TEsColorBand = MinColorBands..MaxColorBands;

  TEsCustomGradient = class(TGraphicControl)
  protected {private}
    {.Z+}
    {property variables}
    FFromColor  : TColor;
    FToColor    : TColor;
    FDirection  : TGradDirection;
    FColorBands : TEsColorBand;

    {internal variables}
    gGradColors : array[0..MaxColorBands-1] of TColorRef;
    gPalette    : HPalette;

    {property methods}
    function GetVersion : string;
    procedure SetColorBands(Value : TEsColorBand);
    procedure SetDirection(Value : TGradDirection);
    procedure SetFromColor(Value : TColor);
    procedure SetToColor(Value : TColor);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure gCalculateColors;
    procedure gFillRectGradient(DC : hDC; const R : TRect);

    {windows message methods}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd);
      message WM_ERASEBKGND;
    {.Z-}

  protected
    {.Z+}
    function PaletteChanged(Foreground : Boolean) : Boolean;
      override;
    procedure Paint;
      override;
    {.Z-}

    {protected properties}
    property ColorBands : TEsColorBand
      read FColorBands
      write SetColorBands
      default 128;

    property Direction : TGradDirection
      read FDirection
      write SetDirection
      default dHorizontal;

    property FromColor : TColor
      read FFromColor
      write SetFromColor
      default clRed;

    property ToColor : TColor
      read FToColor
      write SetToColor
      default clYellow;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

  public
    {.Z+}
    constructor Create(AComponent : TComponent);
      override;
    destructor Destroy;
      override;
    procedure PaintTo(DC : TEshDC; R : TRect);
    {.Z-}
  end;

  TEsGradient = class(TEsCustomGradient)
  published
    {properties}
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
    property Align;
    property ColorBands;
    property Direction;
    property FromColor;
    property ToColor;
    property Version;

    {events}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF WIN32}
  end;


implementation


constructor TEsCustomGradient.Create(AComponent : TComponent);
begin
  inherited Create(AComponent);

  ControlStyle := ControlStyle + [csOpaque];

  FColorBands := 128;
  FDirection  := dHorizontal;
  FFromColor  := clRed;
  FToColor    := clYellow;

  Width       := 100;
  Height      := 100;

  gCalculateColors;

end;

destructor TEsCustomGradient.Destroy;
begin
  if gPalette <> 0 then begin
    DeleteObject(gPalette);
    gPalette := 0;
  end;

  inherited Destroy;
end;

procedure TEsCustomGradient.gCalculateColors;
var
  ToColor   : TRGBMap;
  FromColor : TRGBMap;
  RedPct    : Double;
  GreenPct  : Double;
  BluePct   : Double;
  Band      : Byte;
  LogPal    : PLogPalette;
  LogSize   : Word;
begin
  ToColor.RGBValue := ColorToRGB(FToColor);
  FromColor.RGBValue := ColorToRGB(FFromColor);
  {figure out the percentage of each RGB value needed for banding}
  with ToColor do begin
    RedPct   := (ToColor.Red - FromColor.Red) / (FColorBands-1);
    GreenPct := (ToColor.Green - FromColor.Green) / (FColorBands-1);
    BluePct  := (ToColor.Blue - FromColor.Blue)/ (FColorBands-1);
  end;

  {release any existing palette}
  if gPalette <> 0 then begin
    DeleteObject(gPalette);
    gPalette := 0;
  end;

  {create our palette}
  LogSize := SizeOf(TLogPalette) + (FColorBands-1)*SizeOf(TPaletteEntry);
  GetMem(LogPal, LogSize);
  try
    LogPal^.palVersion := $300;
    LogPal^.palNumEntries := FColorBands;

    {use the percentage of each color to create each band color}
    for Band := 0 to Pred(FColorBands) do begin
      gGradColors[Band] := RGB(FromColor.Red + Round(RedPct*Band),
                               FromColor.Green + Round(GreenPct*Band),
                               FromColor.Blue + Round(BluePct*Band));
      LogPal^.palPalEntry[Band].peRed := FromColor.Red + Round(RedPct*Band);
      LogPal^.palPalEntry[Band].peGreen := FromColor.Green + Round(GreenPct*Band);
      LogPal^.palPalEntry[Band].peBlue := FromColor.Blue + Round(BluePct*Band);
      LogPal^.palPalEntry[Band].peFlags := 0;
    end;

    gPalette := CreatePalette(LogPal^);
  finally
    FreeMem(LogPal, LogSize);
  end;
end;

procedure TEsCustomGradient.gFillRectGradient(DC : hDC; const R : TRect);
  {-paint the given rectangle with the gradient pattern}
var
  OldBrush : hBrush;
  Brush    : hBrush;
  Step     : Double;
  Band     : Integer;
  H, W     : Integer;
  X, Y     : Integer;
begin
  {determine how large each band should be in order to cover the}
  {rectangle (one band for every color intensity level)}
  case FDirection of
    dHorizontal :
      begin
        Step := (R.Right - R.Left) / FColorBands;
        H := R.Bottom - R.Top;
        W := Round(1.5*Step);
        if W < 1 then W := 1;
        {start filling bands}
        for Band := 0 to Pred(FColorBands) do begin
          {create a brush with the appropriate color for this band}
          Brush := CreateSolidBrush(gGradColors[Band]);
          try
            {select that brush into the temporary DC}
            OldBrush := SelectObject(DC, Brush);
            try
              X := Round(Band*Step);
              {fill the rectangle using the selected brush}
              PatBlt(DC, X, 0, W, H, PATCOPY);
            finally
              {clean up the brush}
              SelectObject(DC, OldBrush);
            end;
          finally
            DeleteObject(Brush);
          end;
        end;
      end;
    dVertical :
      begin
        Step := (R.Bottom - R.Top) / FColorBands;
        W := R.Right - R.Left;
        H := Round(1.5*Step);
        if H < 1 then H := 1;
        {start filling bands}
        for Band := 0 to Pred(FColorBands) do begin
          {create a brush with the appropriate color for this band}
          Brush := CreateSolidBrush(gGradColors[Band]);
          try
            {select that brush into the temporary DC}
            OldBrush := SelectObject(DC, Brush);
            try
              Y := Round(Band*Step);
              {fill the rectangle using the selected brush}
              PatBlt(DC, 0, Y, W, H, PATCOPY);
            finally
              {clean up the brush}
              SelectObject(DC, OldBrush);
            end;
          finally
            DeleteObject(Brush);
          end;
        end;
      end;
  end;
end;

function TEsCustomGradient.GetVersion : string;
begin
  Result := EsVersionStr;
end;

procedure TEsCustomGradient.Paint;
begin
  PaintTo(Canvas.Handle, ClientRect);
end;

procedure TEsCustomGradient.PaintTo(DC : TEshDC; R : TRect);
var
  TmpDC   : hDC;
  Bmp     : hBitmap;
  OldBmp  : hBitmap;
  OldPal  : hPalette;
  OldPal2 : hPalette;
  CW, CH  : Integer;
begin
  {get the width and height}
  CW := R.Right-R.Left;
  CH := R.Bottom-R.Top;

  {select our palette into the canvas}
  OldPal := SelectPalette(DC, gPalette, True);
  RealizePalette(DC);
  try
    {create a temporary device context}
    TmpDC := CreateCompatibleDC(DC);
    try
      {create a bitmap to draw on}
      Bmp := CreateCompatibleBitmap(DC, CW, CH);
      try
        {select the bitmap into the temporary DC}
        OldBmp := SelectObject(TmpDC, Bmp);
        try
          {select our palette into the temp DC}
          OldPal2 := SelectPalette(TmpDC, gPalette, True);
          RealizePalette(TmpDC);
          try
            {draw the gradient on the temporary device context}
            gFillRectGradient(TmpDC, R);
            {copy temporary device context to ours}
            BitBlt(DC, 0, 0, CW, CH, TmpDC, 0, 0, SRCCOPY);
          finally
            if OldPal2 > 0 then
              SelectPalette(TmpDC, OldPal2, True);
          end;
        finally
          SelectObject(TmpDC, OldBmp);
        end;
      finally
        DeleteObject(Bmp);
      end;
    finally
      DeleteDC(TmpDC);
    end;
  finally
    if OldPal <> 0 then
      SelectPalette(Canvas.Handle, OldPal, True);
  end;
end;

function TEsCustomGradient.PaletteChanged(Foreground : Boolean) : Boolean;
begin
  gCalculateColors;
  Refresh;
  Result := True;
end;

procedure TEsCustomGradient.SetColorBands(Value : TEsColorBand);
begin
  if (Value <> FColorBands) and (Value >= MinColorBands) and
     (Value <= MaxColorBands) then begin
    FColorBands := Value;
    gCalculateColors;
    Invalidate;
  end;
end;

procedure TEsCustomGradient.SetDirection(Value : TGradDirection);
begin
  if Value <> FDirection then begin
    FDirection := Value;
    gCalculateColors;
    Invalidate;
  end;
end;

procedure TEsCustomGradient.SetFromColor(Value : TColor);
begin
  if Value <> FFromColor then begin
    FFromColor := Value;
    gCalculateColors;
    Invalidate;
  end;
end;

procedure TEsCustomGradient.SetToColor(Value : TColor);
begin
  if Value <> FToColor then begin
    FToColor := Value;
    gCalculateColors;
    Invalidate;
  end;
end;

procedure TEsCustomGradient.SetVersion(const Value : string);
begin
end;

procedure TEsCustomGradient.WMEraseBkgnd(var Msg : TWMEraseBkgnd);
begin
  Msg.Result := 1;   {don't erase background, just say we did}
end;

end.
