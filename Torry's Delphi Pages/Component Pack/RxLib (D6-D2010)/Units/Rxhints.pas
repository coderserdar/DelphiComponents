{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxHints;

{$I RX.INC}

interface

uses
  Windows, Messages,
  Graphics, Classes, Controls, Forms, Dialogs;

type
  THintStyle = (hsRectangle, hsRoundRect, hsEllipse);
  THintPos = (hpTopRight, hpTopLeft, hpBottomRight, hpBottomLeft);
  THintShadowSize = 0..15;

  TRxHintWindow = class(THintWindow)
  private
    FSrcImage: TBitmap;
    FImage: TBitmap;
    FPos: THintPos;
    FRect: TRect;
    FTextRect: TRect;
    FTileSize: TPoint;
    FRoundFactor: Integer;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
{$IFDEF RX_D3}
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
{$ENDIF}
    function CreateRegion(Shade: Boolean): HRgn;
    procedure FillRegion(Rgn: HRgn; Shade: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
{$IFDEF RX_D3}
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
{$ENDIF}
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; {$IFDEF RX_D3} override; {$ENDIF}
  end;

procedure SetHintStyle(Style: THintStyle; ShadowSize: THintShadowSize;
  Tail: Boolean; Alignment: TAlignment);
procedure SetStandardHints;
procedure RegisterHintWindow(AClass: THintWindowClass);
function GetHintControl: TControl;

implementation

uses
  SysUtils,
  rxVclUtils, rxAppUtils, rxMaxMin;

const
  HintStyle: THintStyle = hsRectangle;
  HintShadowSize: THintShadowSize = 0;
  HintTail: Boolean = False;
  HintAlignment: TAlignment = taLeftJustify;

{ Utility routines }

procedure RegisterHintWindow(AClass: THintWindowClass);
begin
  HintWindowClass := AClass;
  with Application do
    if ShowHint then
    begin
      ShowHint := False;
      ShowHint := True;
    end;
end;

procedure SetStandardHints;
begin
  RegisterHintWindow(THintWindow);
end;

procedure SetHintStyle(Style: THintStyle; ShadowSize: THintShadowSize;
  Tail: Boolean; Alignment: TAlignment);
begin
  HintStyle := Style;
  HintShadowSize := ShadowSize;
  HintTail := Tail;
  HintAlignment := Alignment;
  RegisterHintWindow(TRxHintWindow);
end;

function GetHintControl: TControl;
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  Result := FindDragTarget(CursorPos, True);
  while (Result <> nil) and not Result.ShowHint do
    Result := Result.Parent;
  if (Result <> nil) and (csDesigning in Result.ComponentState) then
    Result := nil;
end;

procedure StandardHintFont(AFont: TFont);
var
  NonClientMetrics: TNonClientMetrics;
begin
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    AFont.Handle := CreateFontIndirect(NonClientMetrics.lfStatusFont)
  else
  begin
    AFont.Name := 'MS Sans Serif';
    AFont.Size := 8;
  end;
  AFont.Color := clInfoText;
end;

{$IFNDEF RX_D3}
function GetCursorHeightMargin: Integer;
{ Return number of scanlines between the scanline containing cursor hotspot
  and the last scanline included in the cursor mask. }
var
  IconInfo: TIconInfo;
  BitmapInfoSize: Integer;
  BitmapBitsSize: Integer;
  Bitmap: PBitmapInfoHeader;
  Bits: Pointer;
  BytesPerScanline, ImageSize: Integer;

    function FindScanline(Source: Pointer; MaxLen: Cardinal;
      Value: Cardinal): Cardinal; assembler;
    asm
            PUSH    ECX
            MOV     ECX,EDX
            MOV     EDX,EDI
            MOV     EDI,EAX
            POP     EAX
            REPE    SCASB
            MOV     EAX,ECX
            MOV     EDI,EDX
    end;

begin
  { Default value is entire icon height }
  Result := GetSystemMetrics(SM_CYCURSOR);
  if GetIconInfo(GetCursor, IconInfo) then
  try
    GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
    Bitmap := AllocMem(BitmapInfoSize + BitmapBitsSize);
    try
      Bits := Pointer(Longint(Bitmap) + BitmapInfoSize);
      if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and
        (Bitmap^.biBitCount = 1) then
      begin
        { Point Bits to the end of this bottom-up bitmap }
        with Bitmap^ do
        begin
          BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
          ImageSize := biWidth * BytesPerScanline;
          Bits := Pointer(Integer(Bits) + BitmapBitsSize - ImageSize);
          { Use the width to determine the height since another mask bitmap
            may immediately follow }
          Result := FindScanline(Bits, ImageSize, $FF);
          { In case the and mask is blank, look for an empty scanline in the
            xor mask. }
          if (Result = 0) and (biHeight >= 2 * biWidth) then
            Result := FindScanline(Pointer(Integer(Bits) - ImageSize),
              ImageSize, $00);
          Result := Result div BytesPerScanline;
        end;
        Dec(Result, IconInfo.yHotSpot);
      end;
    finally
      FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
    end;
  finally
    if IconInfo.hbmColor <> 0 then
      DeleteObject(IconInfo.hbmColor);
    if IconInfo.hbmMask <> 0 then
      DeleteObject(IconInfo.hbmMask);
  end;
end;
{$ENDIF}

{ TRxHintWindow }

constructor TRxHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StandardHintFont(Canvas.Font);
  FImage := TBitmap.Create;
  FSrcImage := TBitmap.Create;
end;

destructor TRxHintWindow.Destroy;
begin
  FSrcImage.Free;
  FImage.Free;
  inherited Destroy;
end;

procedure TRxHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER;
end;

{$IFDEF RX_D3}
procedure TRxHintWindow.WMNCPaint(var Message: TMessage);
begin
end;
{$ENDIF}

procedure TRxHintWindow.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

function TRxHintWindow.CreateRegion(Shade: Boolean): HRgn;
var
  R: TRect;
  W, TileOffs: Integer;
  Tail, Dest: HRgn;
  P: TPoint;

  function CreatePolyRgn(const Points: array of TPoint): HRgn;
  type
    PPoints = ^TPoints;
    TPoints = array[0..0] of TPoint;
  begin
    Result := CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, WINDING);
  end;

begin
  R := FRect;
  Result := 0;
  if Shade then OffsetRect(R, HintShadowSize, HintShadowSize);
  case HintStyle of
    hsRoundRect: Result := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom,
      FRoundFactor, FRoundFactor);
    hsEllipse: Result := CreateEllipticRgnIndirect(R);
    hsRectangle: Result := CreateRectRgnIndirect(R);
  end;
  if HintTail then
  begin
    R := FTextRect;
    GetCursorPos(P);
    TileOffs := 0;
    if FPos in [hpTopLeft, hpBottomLeft] then
      TileOffs := Width;
    if Shade then
    begin
      OffsetRect(R, HintShadowSize, HintShadowSize);
      Inc(TileOffs, HintShadowSize);
    end;
    W := Min(Max(8, Min(WidthOf(R), HeightOf(R)) div 4), WidthOf(R) div 2);
    case FPos of
      hpTopRight:
        Tail := CreatePolyRgn([Point(TileOffs, Height - HintShadowSize),
          Point(R.Left + W div 4, R.Bottom), Point(R.Left + 2 * W, R.Bottom)]);
      hpTopLeft:
        Tail := CreatePolyRgn([Point(TileOffs, Height - HintShadowSize),
          Point(R.Right - W div 4, R.Bottom), Point(R.Right - 2 * W, R.Bottom)]);
      hpBottomRight:
        Tail := CreatePolyRgn([Point(TileOffs, 0),
          Point(R.Left + W div 4, R.Top), Point(R.Left + 2 * W, R.Top)]);
      else {hpBottomLeft}
        Tail := CreatePolyRgn([Point(TileOffs, 0),
          Point(R.Right - W div 4, R.Top), Point(R.Right - 2 * W, R.Top)]);
    end;
    try
      Dest := Result;
      Result := CreateRectRgnIndirect(R);
      try
        CombineRgn(Result, Dest, Tail, RGN_OR);
      finally
        if Dest <> 0 then
          DeleteObject(Dest);
      end;
    finally
      DeleteObject(Tail);
    end;
  end;
end;

procedure TRxHintWindow.FillRegion(Rgn: HRgn; Shade: Boolean);
begin
  if Shade then
  begin
    FImage.Canvas.Brush.Bitmap :=
{$IFDEF RX_D4}
      AllocPatternBitmap(clBtnFace, clWindowText);
{$ELSE}
      CreateTwoColorsBrushPattern(clBtnFace, clWindowText);
{$ENDIF}
    FImage.Canvas.Pen.Style := psClear;
  end
  else
  begin
    FImage.Canvas.Pen.Style := psSolid;
    FImage.Canvas.Brush.Color := Color;
  end;
  try
    PaintRgn(FImage.Canvas.Handle, Rgn);
    if not Shade then
    begin
      FImage.Canvas.Brush.Color := Font.Color;
      if (HintStyle = hsRectangle) and not HintTail then
        DrawEdge(FImage.Canvas.Handle, FRect, BDR_RAISEDOUTER, BF_RECT)
      else
        FrameRgn(FImage.Canvas.Handle, Rgn, FImage.Canvas.Brush.Handle, 1, 1);
    end;
  finally
    if Shade then
    begin
{$IFDEF RX_D4}
      FImage.Canvas.Brush.Bitmap := nil;
{$ELSE}
      FImage.Canvas.Brush.Bitmap.Free;
{$ENDIF}
      FImage.Canvas.Pen.Style := psSolid;
    end;
    FImage.Canvas.Brush.Color := Color;
  end;
end;

procedure TRxHintWindow.Paint;
var
  R: TRect;
  FShadeRgn, FRgn: HRgn;

  procedure PaintText(R: TRect);
  const
    Flag: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  begin
    DrawText(FImage.Canvas.Handle, PChar(Caption),
      -1, R, DT_NOPREFIX or DT_WORDBREAK or Flag[HintAlignment]
      {$IFDEF RX_D4} or DrawTextBiDiModeFlagsReadingOnly {$ENDIF});
  end;

begin
  R := ClientRect;
  FImage.Handle := CreateCompatibleBitmap(Canvas.Handle,
    WidthOf(ClientRect), HeightOf(ClientRect));
  FImage.Canvas.Font := Self.Canvas.Font;
  if (HintStyle <> hsRectangle) or (HintShadowSize > 0) or HintTail then
    FImage.Canvas.Draw(0, 0, FSrcImage);
  FRgn := CreateRegion(False);
  FShadeRgn := CreateRegion(True);
  try
    FillRegion(FShadeRgn, True);
    FillRegion(FRgn, False);
  finally
    DeleteObject(FShadeRgn);
    DeleteObject(FRgn);
  end;
  R := FTextRect;
  if HintAlignment = taLeftJustify then
    Inc(R.Left, 2);
  PaintText(R);
  Canvas.Draw(0, 0, FImage);
end;

procedure TRxHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  R: TRect;
  ScreenDC: HDC;
  P: TPoint;
begin
  Caption := AHint;
  GetCursorPos(P);
  FPos := hpBottomRight;
  R := CalcHintRect(Screen.Width, AHint, nil);
{$IFDEF RX_D3}
  OffsetRect(R, Rect.Left - R.Left, Rect.Top - R.Top);
{$ELSE}
  OffsetRect(R, P.X, P.Y + GetCursorHeightMargin);
{$ENDIF}
  Rect := R;
  BoundsRect := Rect;

  if HintTail then
  begin
    Rect.Top := P.Y - Height - 3;
    if Rect.Top < 0 then
      Rect.Top := BoundsRect.Top
    else
      Rect.Bottom := Rect.Top + HeightOf(BoundsRect);

    Rect.Left := P.X + 1;
    if Rect.Left < 0 then
      Rect.Left := BoundsRect.Left
    else
      Rect.Right := Rect.Left + WidthOf(BoundsRect);
  end;

  if Rect.Top + Height > Screen.Height then
  begin
    Rect.Top := Screen.Height - Height;
    if Rect.Top <= P.Y then
      Rect.Top := P.Y - Height - 3;
  end;
  if Rect.Left + Width > Screen.Width then
  begin
    Rect.Left := Screen.Width - Width;
    if Rect.Left <= P.X then
      Rect.Left := P.X - Width -3;
  end;
  if Rect.Left < 0 then
  begin
    Rect.Left := 0;
    if Rect.Left + Width >= P.X then
      Rect.Left := P.X - Width - 1;
  end;
  if Rect.Top < 0 then
  begin
    Rect.Top := 0;
    if Rect.Top + Height >= P.Y then
      Rect.Top := P.Y - Height - 1;
  end;

  if (HintStyle <> hsRectangle) or (HintShadowSize > 0) or HintTail then
  begin
    FPos := hpBottomRight;
    if (Rect.Top + Height < P.Y) then
      FPos := hpTopRight;
    if (Rect.Left + Width < P.X) then
    begin
      if FPos = hpBottomRight then
        FPos := hpBottomLeft
      else
        FPos := hpTopLeft;
    end;
    if HintTail then
    begin
      if (FPos in [hpBottomRight, hpBottomLeft]) then
      begin
        OffsetRect(FRect, 0, FTileSize.Y);
        OffsetRect(FTextRect, 0, FTileSize.Y);
      end;
      if (FPos in [hpBottomRight, hpTopRight]) then
      begin
        OffsetRect(FRect, FTileSize.X, 0);
        OffsetRect(FTextRect, FTileSize.X, 0);
      end;
    end;
    if HandleAllocated then
    begin
      SetWindowPos(Handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_HIDEWINDOW or
        SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE);
      if Screen.ActiveForm <> nil then
        UpdateWindow(Screen.ActiveForm.Handle);
    end;
    ScreenDC := GetDC(0);
    try
      with FSrcImage do
      begin
        Width := WidthOf(BoundsRect);
        Height := HeightOf(BoundsRect);
        BitBlt(Canvas.Handle, 0, 0, Width, Height, ScreenDC, Rect.Left,
          Rect.Top, SRCCOPY);
      end;
    finally
      ReleaseDC(0, ScreenDC);
    end;
  end;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
end;

function TRxHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
const
  Flag: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  A: Integer;
  X, Y, Factor: Double;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle,
    PChar(AHint),
    -1, Result, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX or Flag[HintAlignment]
    {$IFDEF RX_D4} or DrawTextBiDiModeFlagsReadingOnly {$ENDIF});
  Inc(Result.Right, 8);
  Inc(Result.Bottom, 4);
  FRect := Result;
  FTextRect := Result;
  InflateRect(FTextRect, -1, -1);
  case HintAlignment of
    taCenter: OffsetRect(FTextRect, -1, 0);
    taRightJustify: OffsetRect(FTextRect, -4, 0);
  end;
  FRoundFactor := Max(6, Min(WidthOf(Result), HeightOf(Result)) div 4);
  if HintStyle = hsRoundRect then
    InflateRect(FRect, FRoundFactor div 4, FRoundFactor div 4)
  else if HintStyle = hsEllipse then
  begin
    X := WidthOf(FRect) / 2;
    Y := HeightOf(FRect) / 2;
    if (X <> 0) and (Y <> 0) then
    begin
      Factor := Round(Y / 3);
      A := Round(Sqrt((Sqr(X) * Sqr(Y + Factor)) / (Sqr(Y + Factor) - Sqr(Y))));
      InflateRect(FRect, A - Round(X), Round(Factor));
    end;
  end;
  Result := FRect;
  OffsetRect(FRect, -Result.Left, -Result.Top);
  OffsetRect(FTextRect, -Result.Left, -Result.Top);
  Inc(Result.Right, HintShadowSize);
  Inc(Result.Bottom, HintShadowSize);
  if HintTail then
  begin
    FTileSize.Y := Max(14, Min(WidthOf(FTextRect), HeightOf(FTextRect)) div 2);
    FTileSize.X := FTileSize.Y - 8;
    Inc(Result.Right, FTileSize.X);
    Inc(Result.Bottom, FTileSize.Y);
  end;
end;

{$IFDEF RX_D3}
procedure TRxHintWindow.ActivateHintData(Rect: TRect; const AHint: string;
  AData: Pointer);
begin
  ActivateHint(Rect, AHint);
end;
{$ENDIF}

end.