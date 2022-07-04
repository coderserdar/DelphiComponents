{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDECommon;

{$I SweetDrawing.inc}

{$IFDEF SCDE_DELPHI6_UP}
  {$WARNINGS OFF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, {$IFDEF SCDE_DELPHI6_UP} Variants, {$ENDIF} SCDEConsts, SCDEResStrs;

type
  {$IFDEF SCDE_DELPHI4_AND_EARLY}
  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: HWND;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;
  {$ENDIF}

  ESCOSError = class(Exception)
  public
    ErrorCode: DWORD;
  end;

  TSCDeThumbStyle = (sctsArrow, sctsExArrow, sctsMac, sctsMetal, sctsNew, sctsPS,
    sctsPSNew, sctsWindows);

  TSCDeGradient = (scdgNone, scdgLeftToRight, scdgRightToLeft, scdgTopToBottom,
    scdgBottomToTop);


function  scdGetRealColor(AColor: COLORREF): COLORREF;
function  scdOffsetColor(BaseColor: COLORREF; DeltaR, DeltaG, DeltaB: Integer; Light: Boolean): COLORREF;
function  scdBlendedColor(AColor: TColor; DeltaR, DeltaG, DeltaB: Integer; Light: Boolean): TColor;
function  scdGetLightColor(BtnFaceColor, HighlightColor, WindowColor: Integer): COLORREF;
function  scdIsColorLight(AColor: TColor): Boolean;
function  scdBlendColor(AColor: TColor; Increment: Integer): TColor;
function  scdMixColors(AColor, WithColor: TColor; Percent: Integer): TColor;


{$IFDEF SCDE_DELPHI4_AND_EARLY}
procedure scdFreeAndNil(var Obj);
function  scdAnsiSameText(const S1, S2: string): Boolean;
function  scdSameText(const S1, S2: string): Boolean;
{$ENDIF}

procedure scdRaiseLastOSError;

function  scdWinOSUsesGradientCaption: Boolean;
procedure scdInitiateCaptionGradientColors;
function  scdGetParentForm(Control: TControl): TCustomForm;


function  scdGetBtnHighlightOf(C: TColor): TColor;
function  scdGetBtnShadowOf(C: TColor): TColor;
function  scdGetBtnFaceOf(C: TColor): TColor;
function  scdGet3DDkShadowOf(C: TColor): TColor;

function  scdGetOfficeXPBtnColor: COLORREF;
function  scdGetOfficeXPBtnColorOf(AColor: TColor): COLORREF;
function  scdGetOfficeXPDownedColor: COLORREF;
function  scdGetOfficeXPDownedSelColor: COLORREF;
function  scdGetOfficeXPSelColor: COLORREF;


function  scdIsOneOfParents(C: TControl; P: TWinControl): Boolean;


procedure scdFillGradientRectVertical(ACanvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor; ColorCount: Integer; Reverse: Boolean);
procedure scdFillGradientRectHorizontal(ACanvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor; ColorCount: Integer; Reverse: Boolean);
procedure scdFillGradientRect(ACanvas: TCanvas; ARect: TRect; ColorCount: Integer;
  Orientation: TSCDeGradient; StartColor, EndColor: TColor);
procedure scdDrawGradient(ACanvas: TCanvas; R: TRect;
  Orientation: TSCDeGradient; StartColor, EndColor: TColor);


procedure scdDrawXPFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; IsDown, IsHot: Boolean);
procedure scdDrawXPFace2(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; IsThumb, IsDown, IsHot: Boolean);
procedure scdDraw3DFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot: Boolean);
procedure scdDrawNewFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot: Boolean);
procedure scdDrawSports2Face(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl: TColor; IsDown, IsHot: Boolean);
procedure scdDrawOffice12Face(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot, HasFrame, Rounded: Boolean);

  
procedure scdFillTriGradient(ACanvas: TCanvas; ARect: TRect; Color1, Color2,
  Color3: TColor; ColorCount: Integer);
procedure scdDrawTriGradient(ACanvas: TCanvas; R: TRect; Color1, Color2, Color3: TColor);
procedure CbFastFillTriGradient(ACanvas: TCanvas; ARect: TRect; Color1, Color2, Color3: TColor);
procedure scdFillFourGradient(ACanvas: TCanvas; ARect: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor; ColorCount: Integer);
procedure scdDrawFourGradient(ACanvas: TCanvas; R: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor);


procedure scdFillVerticalRainbow(ACanvas: TCanvas; ARect: TRect; Reverse: Boolean);
procedure scdFillHorizontalRainbow(ACanvas: TCanvas; ARect: TRect; Reverse: Boolean);
procedure scdFillVerticalExRainbow(ACanvas: TCanvas; ARect: TRect; ExColor: TColor;
  VertReverse, HorzReverse: Boolean);
procedure scdFillHorizontalExRainbow(ACanvas: TCanvas; ARect: TRect; ExColor: TColor;
  VertReverse, HorzReverse: Boolean);


function  scdVerticalGradientColorOf(P: TPoint; R: TRect; StartColor, EndColor: TColor;
  ColorCount: Integer; Reverse: Boolean): TColor;
function  scdHorizontalGradientColorOf(P: TPoint; R: TRect; StartColor, EndColor: TColor;
  ColorCount: Integer; Reverse: Boolean): TColor;
function  scdGradientFourColorOf(P: TPoint; R: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor; ColorCount: Integer): TColor;


procedure scdDrawFocusRect(C: TCanvas; R: TRect; LineColor: TColor);
procedure scdFrame3D(C: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor;
  LineWidth, Rounded: Integer; UseNoneColor: Boolean = True);
procedure scdRoundRect(C: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor;
  LineWidth: Integer);
procedure scdFillCorners(C: TCanvas; ARect: TRect; AColor: TColor; Rounded: Integer);
procedure scdFillWithColors(C: TCanvas; R: TRect; AColor, WithColor: TColor);


procedure scdDrawEdgeEx(DC: HDC; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges = SCDE_AllBorderEdges);
procedure scdDrawEdge(ACanvas: TCanvas; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges = SCDE_AllBorderEdges);
procedure scdDrawBevelEx(DC: HDC; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges = SCDE_AllBorderEdges);
procedure scdDrawBevel(ACanvas: TCanvas; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges = SCDE_AllBorderEdges);


procedure scdDrawDownSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor = clNone;
  FaceColor: TColor = clNone);
procedure scdDrawUpSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor = clNone;
  FaceColor: TColor = clNone);
procedure scdDrawRightSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle;  Blind: Boolean; FrameColor: TColor = clNone;
  FaceColor: TColor = clNone);
procedure scdDrawLeftSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle;  Blind: Boolean; FrameColor: TColor = clNone;
  FaceColor: TColor = clNone);


procedure scdDrawXPButton(C: TCanvas; R: TRect; Cl, BkCl: TColor; IsDown, IsHot: Boolean);
procedure scdDrawXPThumb(C: TCanvas; R: TRect; Cl: TColor; IsDown, IsHot: Boolean);


function  scdMouseWheelLine: Integer;
function  scdGetWinOperatingSystem: Integer;
function  scdCbxAlphaBlend(Src1, Src2: TBitmap; Amount: Extended): TBitmap;


procedure scdHSVtoRGB(const H, S, V: Integer; var R, G, B: Byte);
procedure scRGBToHSV(const R, G, B: Byte; var H, S, V: Integer);
procedure scdHSLtoRGB(const H, S, L: Word; var R, G, B: Byte);
procedure scRGBToHSL(const R, G, B: Byte; var H, S, L: Word);
procedure scdHSBtoRGB(const H, S, Br: Integer; var R, G, B: Byte);
procedure scRGBToHSB(const R, G, B: Byte; var H, S, Br: Integer);
procedure scRGBToCMYK(const R, G, B: Byte; var C, M, Y, K: Byte);
procedure scdCMYKToRGB(C, M, Y, K: Byte; var R, G, B : Byte);

function  scdHSVtoColor(const H, S, V: Integer): TColor;
procedure scdColorToHSV(const Color: TColor; var H, S, V: Integer);
function  scdHSLtoColor(const H, S, L: Word): TColor;
procedure scdColorToHSL(const Color: TColor; var H, S, L: Word);
function  scdHSBtoColor(const H, S, Br: Integer): TColor;
procedure scdColorToHSB(const Color: TColor; var H, S, Br: Integer);
function  scdCMYKToColor(const C, M, Y, K: Byte): TColor;
procedure scdColorToCMYK(const Color: TColor; var C, M, Y, K: Byte);


function  scdDblClickTime: DWord;
function  scdKeyboardDelay: Integer;
function  scdKeyBoardSpeed: Integer;
procedure scdKillMessage(Wnd: HWnd; Msg: Integer);
function  scdVertScrollbarWidth: Integer;
function  scdHorzScrollbarHeight: Integer;
function  scdVertScrollButtonHeight: Integer;
function  scdHorzScrollButtonWidth: Integer;
procedure scdApplicationCancelHint;
function  scdApplicationHintHidePause: Integer;


function scdIsHex(S: string): Boolean;
function scdIsFloat(const S: string): Boolean; overload;
function scdIsFloat(const S: string; out Value: Extended): Boolean; overload;
function scdIsFloat(const S: string; Currency: Boolean): Boolean; overload;
function scdIsFloat2(const S: string; out Value: Extended): Boolean;
function scdStrToFloatDef(const S: String; Def: Extended): Extended;
function scdIsInteger(const S: string): Boolean; overload;
function scdIsInteger(const S: string; out Value: Integer): Boolean; overload;
function scdIsInt64(const S: string; out Value: Int64): Boolean;


function scdFloatToStr(Value: Extended; IsCurrency: Boolean): String;
function scdCurrencyToStr(Value: Extended; UseThousandsSeperator: Boolean;
  DecimalPlaces: Byte): String;
function scdArrangeDecimalSeperator(S: String; IsCurrency: Boolean; DecimalPlaces: Integer = -1): String;


function  scdGetSpecialKeyName(ShortCut: TShortCut): string;
function  scdShortCutToText(ShortCut: TShortCut): string;
function  scdTextToShortCut(Text: string): TShortCut;
function  scdShortCut(Key: Word; Shift: TShiftState): TShortCut;
procedure scdShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
function  scdShiftStateToKeys(Shift: TShiftState): Word;
function  scdKeysToShiftState(Keys: Word): TShiftState;
function  scdKeyDataToShiftState(KeyData: Longint): TShiftState;
function  scdKeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;


function  scdComponentToString(Component: TComponent): string;
function  scdStringToComponent(Value: string): TComponent;


function  scdVarEquals(const V1, V2: Variant): Boolean;


const
  clSCDEMacAqua = $00DD8040;
  scdRainbowColors: array[0..6] of TColor = (clRed, clYellow, clLime, clAqua,
    clBlue, clFuchsia, clRed);

implementation

function scdIsOneOfParents(C: TControl; P: TWinControl): Boolean;
var
  W: TWinControl;
begin
  Result := (C <> nil) and (P <> nil);
  if not Result then Exit;

  W := C.Parent;
  while W <> nil do
  begin
    Result := W = P;
    if Result then Exit;

    W := W.Parent;
  end;
end;


var
  GradientLibrary: THandle;
  // GradientFill: function(DC: hDC; pVertex: Pointer; dwNumVertex: DWORD;
  //     pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWord; stdcall;

  GradientFill: function(DC: hDC; pVertex: LongInt; dwNumVertex: DWORD;
      pMesh: LongInt; dwNumMesh, dwMode: DWORD): DWord; stdcall;

type
  TRIVERTEX = packed record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: Word;
  end;

function scdCbxAlphaBlend(Src1, Src2: TBitmap; Amount: Extended): TBitmap;
var
  w, h, x, y, XFactor: Integer;
  AmountFit: Extended;
  Ps1, Ps2, Pd: PByteArray;
begin
  Result := TBitmap.Create;

  w := Src1.Width;
  h := Src1.Height;

  if (w <> Src2.Width) or (h <> Src2.Height) then
    raise Exception.Create('Source dimensions are not the identical.');

  AmountFit := 1-Amount;

  Result.Width  := w;
  Result.Height := h;
  Result.PixelFormat := pf24bit;

  if Src1.PixelFormat <> pf24bit then Src1.PixelFormat := pf24bit;
  if Src2.PixelFormat <> pf24bit then Src2.PixelFormat := pf24bit;

  for y := 0 to h - 1 do
  begin
    Ps1 := Src1.ScanLine[y];
    Ps2 := Src2.ScanLine[y];
    Pd  := Result.ScanLine[y];

    for x := 0 to w - 1 do
    begin
      XFactor := 3*x;
      Pd[XFactor] := Round(AmountFit*Ps1[XFactor] + Amount*Ps2[XFactor]);
      Pd[XFactor + 1] := Round(AmountFit*Ps1[XFactor + 1] + Amount*Ps2[XFactor + 1]);
      Pd[XFactor + 2] := Round(AmountFit*Ps1[XFactor + 2] + Amount*Ps2[XFactor + 2]);
    end;
  end;
end;

function scdMouseWheelLine: Integer;
begin
  Result := 1;
  if (scdCurrentOperatingSystem >= scdOsWinNT) and
    not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0) then
    Result := 1;
end;

function scdGetWinOperatingSystem: Integer;
var
  osVerInfo: TOSVersionInfo;
  majorVer, minorVer: Integer;
begin
  Result := scdOsUnknown;
  { set operating system type flag }
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
   majorVer := osVerInfo.dwMajorVersion;
   minorVer := osVerInfo.dwMinorVersion;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT: { Windows NT/2000 }
        begin
          if majorVer <= 4 then
            Result := scdOsWinNT
          else if (majorVer = 5) and (minorVer= 0) then
            Result := scdOsWin2000
          else if (majorVer = 5) and (minorVer = 1) then
            Result := scdOsXP
          else if (majorVer >= 5) then
            Result := scdOsOverXP
          else
            Result := scdOsUnknown;
        end;
      VER_PLATFORM_WIN32_WINDOWS:  { Windows 9x/ME }
        begin
          if (majorVer < 4) then
            Result := scdOsBelow95
          else if (majorVer = 4) and (minorVer = 0) then
            Result := scdOsWin95
          else if (majorVer = 4) and (minorVer = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              Result := scdOsWin98SE
            else
              Result := scdOsWin98;
          end
          else if (majorVer = 4) and (minorVer = 90) then
            Result := scdOsWinME
          else
            Result := scdOsUnknown;
        end;
    else
      Result := scdOsUnknown;
    end;
  end;
end;

function scdGetRealColor(AColor: COLORREF): COLORREF;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetNearestColor(DC, AColor);
  ReleaseDC(0, DC);
end;

function scdOffsetColor(BaseColor: COLORREF; DeltaR, DeltaG, DeltaB: Integer;
  Light: Boolean): COLORREF;
var
  R, G, B: Integer;
begin
  if Light then
  begin
    R := GetRValue(BaseColor) + Abs(DeltaR);
    if R > 255 then R := 255;
    G := GetGValue(BaseColor) + Abs(DeltaG);
    if G > 255 then G := 255;
    B := GetBValue(BaseColor) + Abs(DeltaB);
    if B > 255 then B := 255;
  end else
  begin
    R := GetRValue(BaseColor) - Abs(DeltaR);
    if R < 0 then R := 0;
    G := GetGValue(BaseColor) - Abs(DeltaG);
    if G < 0 then G := 0;
    B := GetBValue(BaseColor) - Abs(DeltaB);
    if B < 0 then B := 0;
  end;
  Result := RGB(R, G, B);
end;

function scdBlendedColor(AColor: TColor; DeltaR, DeltaG, DeltaB: Integer;
  Light: Boolean): TColor;
var
  AColorRef: TColorRef;
begin
  AColorRef := TColorRef(ColorToRGB(AColor));
  Result := scdGetRealColor(scdOffsetColor(AColorRef, DeltaR, DeltaG, DeltaB, Light));
end;

function scdGetLightColor(BtnFaceColor, HighlightColor, WindowColor: Integer): COLORREF;
var
  ABtnFace, AHighlight, AWindow: COLORREF;

  function GetLightIndex(ABtnFaceValue, AHighlightValue, AWindowValue: Byte): Integer;
  begin
    Result := MulDiv(ABtnFaceValue, BtnFaceColor, 100) +
      MulDiv(AHighlightValue, HighlightColor, 100) +
      MulDiv(AWindowValue, WindowColor, 100);

    if Result < 0 then Result := 0;
    if Result > 255 then Result := 255;
  end;

begin
  ABtnFace   := GetSysColor(COLOR_BTNFACE);
  AHighlight := GetSysColor(COLOR_HIGHLIGHT);
  AWindow    := GetSysColor(COLOR_WINDOW);

  if (ABtnFace = 0) or (ABtnFace = $FFFFFF) then
    Result := AHighlight
  else
    Result := RGB(
      GetLightIndex(GetRValue(ABtnFace), GetRValue(AHighlight), GetRValue(AWindow)),
      GetLightIndex(GetGValue(ABtnFace), GetGValue(AHighlight), GetGValue(AWindow)),
      GetLightIndex(GetBValue(ABtnFace), GetBValue(AHighlight), GetBValue(AWindow)));
end;

function scdIsColorLight(AColor: TColor): Boolean;
var
  AColorRef: TColorRef;
begin
  AColorRef := TColorRef(ColorToRGB(AColor));
  Result := Round((GetRValue(AColorRef) +
    GetBValue(AColorRef) + GetGValue(AColorRef))/3) >= 225;
end;

function scdBlendColor(AColor: TColor; Increment: Integer): TColor;
var
  C: LongInt;
  R, G, B: Integer;
begin
  if Increment < -255 then
    Increment := -255;

  if Increment > 255 then
    Increment := 255;

  C := ColorToRGB(AColor);

  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);

  Inc(R, Increment);
  if R < 0 then R := 0
  else
  if R > 255 then R := 255;

  Inc(G, Increment);
  if G < 0 then G := 0
  else
  if G > 255 then G := 255;

  Inc(B, Increment);
  if B < 0 then B := 0
  else
  if B > 255 then B := 255;

  Result := RGB(R, G, B);
end;

function scdMixColors(AColor, WithColor: TColor; Percent: Integer): TColor;
var
  C1, C2: LongInt;
  R1, G1, B1,
  R2, G2, B2: Integer;
begin
  if Percent < -100 then
    Percent := -100;

  if Percent > 100 then
    Percent := 100;

  C1 := ColorToRGB(AColor);
  C2 := ColorToRGB(WithColor);

  if C1 = C2 then
  begin
    Result := AColor;
    Exit;
  end;

  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  Inc(R1, Round(Percent*(R2 - R1)/100));
  if R1 < 0 then R1 := 0
  else
  if R1 > 255 then R1 := 255;

  Inc(G1, Round(Percent*(G2 - G1)/100));
  if G1 < 0 then G1 := 0
  else
  if G1 > 255 then G1 := 255;

  Inc(B1, Round(Percent*(B2 - B1)/100));
  if B1 < 0 then B1 := 0
  else
  if B1 > 255 then B1 := 255;

  Result := RGB(R1, G1, B1);
end;

function scdGetBtnHighlightOf(C: TColor): TColor;
var
  Cl: LongInt;
  R, G, B: Integer;
begin
  // Result := scdBlendedColor(C, 220, 220, 220, True);

  Cl := ColorToRGB(C);

  R := GetRValue(Cl);
  G := GetGValue(Cl);
  B := GetBValue(Cl);

  Inc(R, Round(7*(255 - R) / 8));
  Inc(G, Round(7*(255 - G) / 8));
  Inc(B, Round(7*(255 - B) / 8));

  Result := RGB(R, G, B);
end;

function scdGetBtnShadowOf(C: TColor): TColor;
var
  Cl: LongInt;
  R, G, B: Integer;
begin
  // Result := scdBlendedColor(C, 96, 96, 96, False);

  Cl := ColorToRGB(C);

  R := GetRValue(Cl);
  G := GetGValue(Cl);
  B := GetBValue(Cl);

  Dec(R, Round(3*R / 8));
  Dec(G, Round(3*G / 8));
  Dec(B, Round(3*B / 8));

  Result := RGB(R, G, B);
end;

function scdGetBtnFaceOf(C: TColor): TColor;
begin
  // Result := scdBlendedColor(C, 24, 24, 24, False);
  Result := C;
end;

function scdGet3DDkShadowOf(C: TColor): TColor;
var
  Cl: LongInt;
  R, G, B: Integer;
begin
  // Result := scdBlendedColor(C, 160, 160, 160, False);

  Cl := ColorToRGB(C);

  R := GetRValue(Cl);
  G := GetGValue(Cl);
  B := GetBValue(Cl);

  Dec(R, Round(5*R / 7));
  Dec(G, Round(5*G / 7));
  Dec(B, Round(5*B / 7));

  Result := RGB(R, G, B);
end;

function scdGetOfficeXPBtnColor: COLORREF;

  function GetLightValue(Value: Byte): Byte;
  begin
    Result := Value + MulDiv(255 - Value, 16, 100);
  end;

begin
  Result := GetSysColor(COLOR_BTNFACE);
  Result := RGB(GetLightValue(GetRValue(Result)),
    GetLightValue(GetGValue(Result)), GetLightValue(GetBValue(Result)));

  Result := scdGetRealColor(Result);
end;

function scdGetOfficeXPBtnColorOf(AColor: TColor): COLORREF;

  function GetLightValue(Value: Byte): Byte;
  begin
    Result := Value + MulDiv(255 - Value, 16, 100);
  end;

begin
  Result := ColorToRGB(AColor);
  Result := RGB(GetLightValue(GetRValue(Result)),
    GetLightValue(GetGValue(Result)), GetLightValue(GetBValue(Result)));

  Result := scdGetRealColor(Result);
end;

function scdGetOfficeXPDownedColor: COLORREF;
begin
  Result := scdGetRealColor(scdGetLightColor(11, 9, 73));
end;

function scdGetOfficeXPDownedSelColor: COLORREF;
begin
  Result := scdGetRealColor(scdGetLightColor(14, 44, 40));
end;

function scdGetOfficeXPSelColor: COLORREF;
begin
  Result := scdGetRealColor(scdGetLightColor(-2, 30, 72));
end;

{$IFDEF SCDE_DELPHI4_AND_EARLY}
procedure scdFreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

function scdAnsiSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;

function scdSameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
        CALL    CompareText
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;
{$ENDIF}

function scdWinOSUsesGradientCaption: Boolean;
begin
  if not SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @Result, 0) then
    Result := False;
end;

procedure scdFillGradientRectVertical(ACanvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor; ColorCount: Integer; Reverse: Boolean);
var
  R: TRect;
  I, H: Integer;
  YDif, ThisRGB, RGB1, RGB2: array[0..2] of DWord;
  C1, C2, Tmp: LongInt;
  ThisColor: TColor;
begin
  if (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  H := R.Bottom - R.Top;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode    := pmCopy;
    Pen.Style   := psSolid;
  end;

  if (H = 1) or (StartColor = EndColor) then
  begin
    with ACanvas do
    begin
      Brush.Color := StartColor;
      FillRect(ARect);
    end;

    Exit;
  end;

  C1 := ColorToRGB(StartColor);
  C2 := ColorToRGB(EndColor);

  if Reverse then
  begin
    Tmp := C1;
    C1  := C2;
    C2  := Tmp;
  end;

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  if ColorCount > H - 1 then
    ColorCount := H - 1;

  if ColorCount > 255 then ColorCount := 255;

  YDif[0] := RGB1[0] - RGB2[0];
  YDif[1] := RGB1[1] - RGB2[1];
  YDif[2] := RGB1[2] - RGB2[2];

  R.Top := ARect.Top;
  R.Bottom := R.Top;

  for I := 0 to ColorCount do
  begin
    Inc(R.Bottom, MulDiv(I, H-1, ColorCount) - MulDiv(I-1, H-1, ColorCount));
    if R.Bottom = R.Top then Inc(R.Bottom);

    try
      IntersectRect(R, R, ARect);
      if IsRectEmpty(R) then Continue;

      ThisRGB[0] := RGB1[0] - DWord(Muldiv(I, YDif[0], ColorCount));
      ThisRGB[1] := RGB1[1] - DWord(Muldiv(I, YDif[1], ColorCount));
      ThisRGB[2] := RGB1[2] - DWord(Muldiv(I, YDif[2], ColorCount));

      with ACanvas do
      begin
        ThisColor := TColor(RGB(ThisRGB[0], ThisRGB[1], ThisRGB[2]));

        if (R.Right - R.Left = 1) and (R.Bottom - R.Top = 1) then
          Pixels[R.Left, R.Top] := ThisColor
        else begin
          Brush.Color := ThisColor;
          PatBlt(Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PatCopy);
        end;
      end;
    finally
      R.Top := R.Bottom;
    end;
  end;
end;

procedure scdFillGradientRectHorizontal(ACanvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor; ColorCount: Integer; Reverse: Boolean);
var
  R: TRect;
  I, W: Integer;
  XDif, ThisRGB, RGB1, RGB2: array[0..2] of DWord;
  C1, C2, Tmp: LongInt;
  ThisColor: TColor;
begin
  if (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  W := R.Right - R.Left;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode    := pmCopy;
    Pen.Style   := psSolid;
  end;

  if (W = 1) or (StartColor = EndColor) then
  begin
    with ACanvas do
    begin
      Brush.Color := StartColor;
      FillRect(ARect);
    end;

    Exit;
  end;

  C1 := ColorToRGB(StartColor);
  C2 := ColorToRGB(EndColor);

  if Reverse then
  begin
    Tmp := C1;
    C1  := C2;
    C2  := Tmp;
  end;

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  if ColorCount > W - 1 then
    ColorCount := W - 1;

  if ColorCount > 255 then ColorCount := 255;

  XDif[0] := RGB1[0] - RGB2[0];
  XDif[1] := RGB1[1] - RGB2[1];
  XDif[2] := RGB1[2] - RGB2[2];

  R.Left  := ARect.Left;
  R.Right := R.Left;

  for I := 0 to ColorCount do
  begin
    Inc(R.Right, MulDiv(I, W-1, ColorCount) - MulDiv(I-1, W-1, ColorCount));
    if R.Right = R.Left then Inc(R.Right);

    try
      IntersectRect(R, R, ARect);
      if IsRectEmpty(R) then Continue;

      ThisRGB[0] := RGB1[0] - DWord(Muldiv(I, XDif[0], ColorCount));
      ThisRGB[1] := RGB1[1] - DWord(Muldiv(I, XDif[1], ColorCount));
      ThisRGB[2] := RGB1[2] - DWord(Muldiv(I, XDif[2], ColorCount));

      with ACanvas do
      begin
        ThisColor := TColor(RGB(ThisRGB[0], ThisRGB[1], ThisRGB[2]));

        if (R.Right - R.Left = 1) and (R.Bottom - R.Top = 1) then
          Pixels[R.Left, R.Top] := ThisColor
        else begin
          Brush.Color := ThisColor;
          PatBlt(Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PatCopy);
        end;
      end;
    finally
      R.Left := R.Right;
    end;
  end;
end;

procedure scdFillGradientRect(ACanvas: TCanvas; ARect: TRect; ColorCount: Integer;
  Orientation: TSCDeGradient; StartColor, EndColor: TColor);
begin
  if (Orientation = scdgNone) or
    (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  case Orientation of
    scdgLeftToRight, scdgRightToLeft:
    begin
      scdFillGradientRectHorizontal(ACanvas, ARect, StartColor, EndColor,
        ColorCount, Orientation = scdgRightToLeft);
    end;
    scdgTopToBottom, scdgBottomToTop:
    begin
      scdFillGradientRectVertical(ACanvas, ARect, StartColor, EndColor,
        ColorCount, Orientation = scdgBottomToTop);
    end;
  end;
end;

procedure scdDrawGradient(ACanvas: TCanvas; R: TRect;
  Orientation: TSCDeGradient; StartColor, EndColor: TColor);
var
  ARect: TRect;
  Os: Integer;
  TVs: array[0..1] of TRIVERTEX;
  GradRect: GRADIENT_RECT;
  AStartColor, AEndColor: TColor;
  StartColorRef, EndColorRef: TColorRef;
begin
  if (Orientation = scdgNone) or
    (ACanvas = nil) or IsRectEmpty(R) then Exit;

  ARect := R;
  Os := scdCurrentOperatingSystem;
  if (Os = scdOsUnknown) or (Os = scdOsWin95) or (Os = scdOsWinNT) then
  begin
    scdFillGradientRect(ACanvas, ARect, 255, Orientation,
      StartColor, EndColor);
    Exit;
  end;

  if GradientLibrary <= 0 then
  begin
    scdFillGradientRect(ACanvas, ARect, 255, Orientation,
      StartColor, EndColor);
    Exit;
  end;

  @GradientFill := GetProcAddress(GradientLibrary, PChar('GradientFill'));
  if @GradientFill = nil then
  begin
    scdFillGradientRect(ACanvas, ARect, 255, Orientation,
      StartColor, EndColor);
    Exit;
  end;

  case Orientation of
    scdgLeftToRight, scdgTopToBottom:
      begin
        AStartColor := StartColor;
        AEndColor := EndColor;
      end;
    scdgRightToLeft, scdgBottomToTop:
      begin
        AStartColor := EndColor;
        AEndColor := StartColor;
      end;
    else begin
      AStartColor := StartColor;
      AEndColor := EndColor;
    end;
  end;

  StartColorRef := TColorRef(ColorToRGB(AStartColor));
  EndColorRef := TColorRef(ColorToRGB(AEndColor));

  with TVs[0] do
  begin
    x := ARect.Left;
    y := ARect.Top;
    Green := GetGValue(StartColorRef) shl 8;
    Blue := GetBValue(StartColorRef) shl 8;
    Red := GetRValue(StartColorRef) shl 8;
  end;

  with TVs[1] do
  begin
    x := ARect.Right;
    y := ARect.Bottom;
    Green := GetGValue(EndColorRef) shl 8;
    Blue := GetBValue(EndColorRef) shl 8;
    Red := GetRValue(EndColorRef) shl 8;
  end;

  with GradRect do
  begin
    UpperLeft := 0;
    LowerRight := 1;
  end;

  try
    with ACanvas do
    begin
      Brush.Color := AEndColor;
      ARect := R;
      FillRect(ARect);
      if (Orientation = scdgLeftToRight) or (Orientation = scdgRightToLeft) then
        GradientFill(ACanvas.Handle, LongInt(@TVs), 2, LongInt(@GradRect), 1, GRADIENT_FILL_RECT_H)
      else
        GradientFill(ACanvas.Handle, LongInt(@TVs), 2, LongInt(@GradRect), 1, GRADIENT_FILL_RECT_V);
    end;
  finally
    // FreeLibrary(H);
  end;
end;

procedure scdDrawXPFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; IsDown, IsHot: Boolean);
var
  CR: TRect;
  GradOrient: TSCDeGradient;
  BtmColor, TopColor,
  CornerColor1, CornerColor2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  CR := R;

  GradOrient := scdgTopToBottom;
  if Kind = scdskVertical then
    GradOrient := scdgLeftToRight;

  // outter sunken frame
  if BkCl = clNone then BkCl := clBtnFace;

  TopColor := scdBlendedColor(BkCl, 16, 16, 16, False);
  BtmColor := scdBlendedColor(BkCl, 48, 48, 48, True);

  scdDrawGradient(C, R, GradOrient, TopColor, BtmColor);

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := BkCl;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 1, R.Top);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right, R.Top);

      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 1, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;

  InflateRect(R, -1, -1);

  CornerColor1 := TopColor;
  CornerColor2 := BtmColor;

  // outter frame
  TopColor := scdBlendedColor(Cl, 144, 144, 144, False);
  BtmColor := scdBlendedColor(TopColor, 16, 16, 16, True);

  scdDrawGradient(C, R, GradOrient, TopColor, BtmColor);

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := CornerColor1;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 1, R.Top);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right, R.Top);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 1, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;

  InflateRect(R, -1, -1);

  CornerColor1 := scdBlendedColor(TopColor, 64, 64, 64, True);
  CornerColor2 := scdBlendedColor(BtmColor, 64, 64, 64, True);

  // face
  if IsDown and IsHot then
  begin
    TopColor := scdBlendedColor(Cl, 24, 24, 24, True);
    BtmColor := scdBlendedColor(Cl, 48, 48, 48, True);
  end else
  begin
    TopColor := scdBlendedColor(Cl, 48, 48, 48, True);
    BtmColor := scdBlendedColor(Cl, 24, 24, 24, False);
  end;

  scdDrawGradient(C, R, GradOrient, TopColor, BtmColor);

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := TopColor;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);

      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Bottom);
    end;

  R := CR;
  InflateRect(R, -1, -1);

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := CornerColor1;

      MoveTo(R.Left + 1, R.Top);
      LineTo(R.Left - 1, R.Top + 2);
      MoveTo(R.Right - 2, R.Top);
      LineTo(R.Right, R.Top + 2);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 2);
      LineTo(R.Left + 2, R.Bottom);
      MoveTo(R.Right - 1, R.Bottom - 2);
      LineTo(R.Right - 3, R.Bottom);

      InflateRect(R, -1, -1);

      CornerColor1 := scdBlendedColor(CornerColor1, 32, 32, 32, False);
      CornerColor2 := scdBlendedColor(CornerColor2, 32, 32, 32, False);

      Pen.Color := CornerColor1;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 1, R.Top);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right, R.Top);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 1, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;

  R := CR;  
  with C do
  begin
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := BkCl;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Top + 1);

    MoveTo(R.Right - 1, R.Top);
    LineTo(R.Right - 1, R.Top + 1);

    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 2);

    MoveTo(R.Right - 1, R.Bottom - 1);
    LineTo(R.Right - 1, R.Bottom - 2);
  end;
end;

procedure scdDrawXPFace2(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; IsThumb, IsDown, IsHot: Boolean);
var
  CR: TRect;
  GradOrient: TSCDeGradient;
  BtmColor, TopColor,
  CornerColor1, CornerColor2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  CR := R;

  if IsThumb then
  begin
    GradOrient := scdgTopToBottom;
    if Kind = scdskVertical then
      GradOrient := scdgLeftToRight;
  end else
  begin
    GradOrient := scdgLeftToRight;
    if Kind = scdskVertical then
      GradOrient := scdgTopToBottom;
  end;

  // outter frame
  if BkCl = clNone then BkCl := clBtnFace;

  TopColor := scdBlendedColor(BkCl, 16, 16, 16, False);
  BtmColor := scdBlendedColor(BkCl, 48, 48, 48, True);

  CornerColor1 := TopColor;
  CornerColor2 := BtmColor;

  TopColor := scdBlendedColor(Cl, 144, 144, 144, False);
  BtmColor := scdBlendedColor(TopColor, 16, 16, 16, True);

  scdDrawGradient(C, R, GradOrient, TopColor, BtmColor);

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := CornerColor1;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 1, R.Top);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right, R.Top);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 1, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;

  InflateRect(R, -1, -1);

  CornerColor1 := scdBlendedColor(TopColor, 64, 64, 64, True);
  CornerColor2 := scdBlendedColor(BtmColor, 64, 64, 64, True);

  // face
  if IsDown and IsHot then
  begin
    TopColor := scdBlendedColor(Cl, 24, 24, 24, True);
    BtmColor := scdBlendedColor(Cl, 48, 48, 48, True);
  end else
  begin
    TopColor := scdBlendedColor(Cl, 48, 48, 48, True);
    BtmColor := scdBlendedColor(Cl, 24, 24, 24, False);
  end;

  scdDrawGradient(C, R, GradOrient, TopColor, BtmColor);

  if not IsRectEmpty(R) and ((IsThumb and (Kind = scdskHorizontal)) or
    (not IsThumb and (Kind = scdskVertical))) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := TopColor;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);

      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Bottom);
    end;

  R := CR;

  if not IsRectEmpty(R) then
    with C do
    begin
      Pen.Mode  := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 1;

      Pen.Color := CornerColor1;

      MoveTo(R.Left + 1, R.Top);
      LineTo(R.Left - 1, R.Top + 2);
      MoveTo(R.Right - 2, R.Top);
      LineTo(R.Right, R.Top + 2);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 2);
      LineTo(R.Left + 2, R.Bottom);
      MoveTo(R.Right - 1, R.Bottom - 2);
      LineTo(R.Right - 3, R.Bottom);

      InflateRect(R, -1, -1);

      CornerColor1 := scdBlendedColor(CornerColor1, 32, 32, 32, False);
      CornerColor2 := scdBlendedColor(CornerColor2, 32, 32, 32, False);

      Pen.Color := CornerColor1;

      MoveTo(R.Left, R.Top);
      LineTo(R.Left + 1, R.Top);
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right, R.Top);

      Pen.Color := CornerColor2;

      MoveTo(R.Left, R.Bottom - 1);
      LineTo(R.Left + 1, R.Bottom - 1);
      MoveTo(R.Right - 1, R.Bottom - 1);
      LineTo(R.Right, R.Bottom - 1);
    end;

  R := CR;  
  with C do
  begin
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := BkCl;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Top + 1);

    MoveTo(R.Right - 1, R.Top);
    LineTo(R.Right - 1, R.Top + 1);

    MoveTo(R.Left, R.Bottom - 1);
    LineTo(R.Left, R.Bottom - 2);

    MoveTo(R.Right - 1, R.Bottom - 1);
    LineTo(R.Right - 1, R.Bottom - 2);
  end;
end;

procedure scdDraw3DFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot: Boolean);
var
  R2: TRect;
  C1, C2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  if (not Reverse and (Kind = scdskHorizontal)) or
    (Reverse and (Kind = scdskVertical)) then
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 64);
    C2 := SCDECommon.scdBlendColor(Cl, -64);

    if IsDown and IsHot then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C2;

        FillRect(R);
      end;
    end else
    begin
      R2 := R;
      R2.Bottom := R2.Top + Round((R2.Bottom - R2.Top) / 2);

      scdDrawGradient(C, R2, scdgTopToBottom, C1, Cl);

      R2 := R;
      R2.Top := R2.Bottom - Round((R2.Bottom - R2.Top) / 2);

      scdDrawGradient(C, R2, scdgTopToBottom, Cl, C2);
    end;

    R2 := R;
    scdFrame3D(C, R2, C1, C2, 1, 0);

    C1 := SCDECommon.scdBlendColor(Cl, 48);
    C2 := SCDECommon.scdBlendColor(Cl, -48);

    scdFrame3D(C, R2, C1, C2, 1, 0);
  end else
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 64);
    C2 := SCDECommon.scdBlendColor(Cl, -64);

    if IsDown and IsHot then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C2;

        FillRect(R);
      end;
    end else
    begin
      R2 := R;
      R2.Right := R2.Left + ((R2.Right - R2.Left) div 2);

      scdDrawGradient(C, R2, scdgLeftToRight, C1, Cl);

      R2 := R;
      R2.Left := R2.Right - ((R2.Right - R2.Left) div 2);

      scdDrawGradient(C, R2, scdgLeftToRight, Cl, C2);
    end;

    R2 := R;
    scdFrame3D(C, R2, C1, C2, 1, 0);

    C1 := SCDECommon.scdBlendColor(Cl, 48);
    C2 := SCDECommon.scdBlendColor(Cl, -48);

    scdFrame3D(C, R2, C1, C2, 1, 0);
  end;
end;

procedure scdDrawNewFace(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot: Boolean);
var
  R2: TRect;
  C1, C2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  if (not Reverse and (Kind = scdskHorizontal)) or
    (Reverse and (Kind = scdskVertical)) then
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 48);
    C2 := SCDECommon.scdBlendColor(Cl, -48);

    if IsDown and IsHot then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C2;

        FillRect(R);
      end;
    end else
    begin
      R2 := R;
      R2.Bottom := R2.Top + Round((R2.Bottom - R2.Top) / 2);

      scdDrawGradient(C, R2, scdgTopToBottom, C1, Cl);

      R2 := R;
      R2.Top := R2.Bottom - Round((R2.Bottom - R2.Top) / 2);

      scdDrawGradient(C, R2, scdgTopToBottom, Cl, C2);
    end;

    R2 := R;
    scdFrame3D(C, R2, C1, C2, 1, 0);

    InflateRect(R2, 1, 1);
    C1 := SCDECommon.scdBlendColor(Cl, -96);

    scdFrame3D(C, R2, C1, C1, 1, 0);
  end else
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 48);
    C2 := SCDECommon.scdBlendColor(Cl, -48);

    if IsDown and IsHot then
    begin
      with C do
      begin
        Brush.Style := bsSolid;
        Brush.Color := C2;

        FillRect(R);
      end;
    end else
    begin
      R2 := R;
      R2.Right := R2.Left + ((R2.Right - R2.Left) div 2);

      scdDrawGradient(C, R2, scdgLeftToRight, C1, Cl);

      R2 := R;
      R2.Left := R2.Right - ((R2.Right - R2.Left) div 2);

      scdDrawGradient(C, R2, scdgLeftToRight, Cl, C2);
    end;
    
    R2 := R;
    scdFrame3D(C, R2, C1, C2, 1, 0);

    InflateRect(R2, 1, 1);
    C1 := SCDECommon.scdBlendColor(Cl, -96);

    scdFrame3D(C, R2, C1, C1, 1, 0);
  end;
end;

procedure scdDrawSports2Face(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl: TColor; IsDown, IsHot: Boolean);
var
  CR: TRect;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Cl;

    FillRect(R);
  end;

  CR := R;

  if IsDown and IsHot then
    scdFrame3D(C, CR, scdGet3DDkShadowOf(Cl), scdGet3DDkShadowOf(Cl), 1, 0, True)
  else  
    scdFrame3D(C, CR, scdGetBtnHighlightOf(Cl), scdGet3DDkShadowOf(Cl), 1, 0, True);

  InflateRect(CR, -1, -1);

  with C do
  begin
    Pen.Style := psSolid;
    Pen.Mode  := pmCopy;
    Pen.Width := 1;
    Pen.Color := scdGetBtnHighlightOf(Cl);

    Rectangle(CR.Left, CR.Top, CR.Right, CR.Bottom);

    Inc(CR.Left);
    Inc(CR.Top);

    Rectangle(CR.Left, CR.Top, CR.Right, CR.Bottom);

    OffsetRect(CR, -1, -1);
    Pen.Color := scdGetBtnShadowOf(Cl);

    Rectangle(CR.Left, CR.Top, CR.Right, CR.Bottom);
  end;
end;

procedure scdDrawOffice12Face(Kind: TSCDeScrollbarKind; C: TCanvas; R: TRect;
  Cl, BkCl: TColor; Reverse, IsDown, IsHot, HasFrame, Rounded: Boolean);
var
  R1, R2: TRect;
  C1, C2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  R1 := R;
  InflateRect(R1, -1, -1);
  
  C1 := SCDECommon.scdBlendColor(Cl, 48);
  C2 := SCDECommon.scdBlendColor(Cl, -24);

  if IsDown then
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 32);
    C2 := SCDECommon.scdBlendColor(Cl, -48);
  end else
  if IsHot then
  begin
    C1 := SCDECommon.scdBlendColor(Cl, 32);
    C2 := SCDECommon.scdBlendColor(Cl, -36);
  end;

  if (not Reverse and (Kind = scdskHorizontal)) or
    (Reverse and (Kind = scdskVertical)) then
  begin
    R2 := R1;
    R2.Bottom := R2.Top + Round((R2.Bottom - R2.Top) / 2);

    scdDrawGradient(C, R2, scdgTopToBottom, C1, Cl);

    R2 := R1;
    R2.Top := R2.Bottom - Round((R2.Bottom - R2.Top) / 2);

    scdDrawGradient(C, R2, scdgTopToBottom, C2, C1);
  end else
  begin
    R2 := R1;
    R2.Right := R2.Left + ((R2.Right - R2.Left) div 2);

    scdDrawGradient(C, R2, scdgLeftToRight, C1, Cl);

    R2 := R1;
    R2.Left := R2.Right - ((R2.Right - R2.Left) div 2);

    scdDrawGradient(C, R2, scdgLeftToRight, C2, C1);
  end;

  if HasFrame then
  begin
    R2 := R;
    InflateRect(R2, -1, -1);

    C1 := SCDECommon.scdBlendColor(Cl, 24);
    scdFrame3D(C, R2, C1, Cl, 1, 0);

    R2 := R;
    C1 := SCDECommon.scdBlendColor(Cl, -86);

    if Rounded then
      scdFrame3D(C, R2, C1, C1, 1, 2)
    else scdFrame3D(C, R2, C1, C1, 1, 0);

    if Rounded then
    begin
      InflateRect(R2, 1, 1);
      Dec(R2.Right);
      Dec(R2.Bottom);

      C2 := scdMixColors(C1, C.Pixels[R2.Left, R2.Top], 60);
      C.Pixels[R2.Left, R2.Top] := C2;

      C2 := scdMixColors(C1, C.Pixels[R2.Right, R2.Top], 60);
      C.Pixels[R2.Right, R2.Top] := C2;

      C2 := scdMixColors(C1, C.Pixels[R2.Right, R2.Bottom], 60);
      C.Pixels[R2.Right, R2.Bottom] := C2;

      C2 := scdMixColors(C1, C.Pixels[R2.Left, R2.Bottom], 60);
      C.Pixels[R2.Left, R2.Bottom] := C2;
    end;
  end;
end;

function scdGetParentForm(Control: TControl): TCustomForm;
begin
  Result := nil;
  while (Control <> nil) and (Control.Parent <> nil) do
  begin
    Control := Control.Parent;

    if Control is TCustomForm then
    begin
      Result := TCustomForm(Control);
      Exit;
    end;
  end;  
end;

procedure scdInitiateCaptionGradientColors;
begin
  {$IFDEF SCDE_DELPHI5_AND_EARLY}
  if scdWinOSUsesGradientCaption then
  begin
    clActiveCaptionGradient := TColor(GetSysColor(COLOR_GRADIENTACTIVECAPTION));
    clInactiveCaptionGradient := TColor(GetSysColor(COLOR_GRADIENTINACTIVECAPTION));
  end else
  begin
    clActiveCaptionGradient := TColor(GetSysColor(COLOR_ACTIVECAPTION));
    clInactiveCaptionGradient := TColor(GetSysColor(COLOR_INACTIVECAPTION));
  end;
  {$ENDIF}
end;

procedure scdFillTriGradient(ACanvas: TCanvas; ARect: TRect; Color1, Color2,
  Color3: TColor; ColorCount: Integer);
var
  R: TRect;
  I, J, W, H,
  XCount, YCount: Integer;
  XDif, YDif, endRGB,
  ThisRGB, RGB1, RGB2, RGB3: array[0..2] of DWord;
  C1, C2, C3: LongInt;
  ThisColor: TColor;
begin
  if (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
  end;

  if ((W = 1) and (H = 1)) or
    ((Color1 = Color2) and (Color1 = Color3)) then
  begin
    with ACanvas do
    begin
      Brush.Color := Color1;
      FillRect(ARect);
    end;

    Exit;
  end;

  C1 := ColorToRGB(Color1); // <-- left_top color
  C2 := ColorToRGB(Color2); // <-- right_top color
  C3 := ColorToRGB(Color3); // <-- right_bottom color

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  RGB3[0] := GetRValue(C3);
  RGB3[1] := GetGValue(C3);
  RGB3[2] := GetBValue(C3);

  XCount := ColorCount;
  if XCount > W - 1 then XCount := W - 1;

  YCount := ColorCount;
  if YCount > H - 1 then YCount := H - 1;

  if XCount > 255 then XCount := 255;
  if YCount > 255 then YCount := 255;

  YDif[0] := RGB2[0] - RGB3[0];
  YDif[1] := RGB2[1] - RGB3[1];
  YDif[2] := RGB2[2] - RGB3[2];

  R.Bottom := ARect.Top;
  for J := 0 to YCount do
  begin
    R.Left  := ARect.Left;
    R.Right := R.Left;

    Inc(R.Bottom, MulDiv(J, H-1, YCount) - MulDiv(J-1, H-1, YCount));
    if R.Bottom = R.Top then Inc(R.Bottom);

    endRGB[0] := RGB2[0] - DWord(Muldiv(J, YDif[0], YCount));
    endRGB[1] := RGB2[1] - DWord(Muldiv(J, YDif[1], YCount));
    endRGB[2] := RGB2[2] - DWord(Muldiv(J, YDif[2], YCount));

    XDif[0] := RGB1[0] - endRGB[0];
    XDif[1] := RGB1[1] - endRGB[1];
    XDif[2] := RGB1[2] - endRGB[2];

    try
      for I := 0 to XCount do
      begin
        Inc(R.Right, MulDiv(I, W-1, XCount) - MulDiv(I-1, W-1, XCount));
        if R.Right = R.Left then Inc(R.Right);

        try
          IntersectRect(R, R, ARect);
          if IsRectEmpty(R) then Continue;

          ThisRGB[0] := RGB1[0] - DWord(Muldiv(I, XDif[0], XCount));
          ThisRGB[1] := RGB1[1] - DWord(Muldiv(I, XDif[1], XCount));
          ThisRGB[2] := RGB1[2] - DWord(Muldiv(I, XDif[2], XCount));

          with ACanvas do
          begin
            ThisColor := TColor(RGB(ThisRGB[0], ThisRGB[1], ThisRGB[2]));

            if (R.Right - R.Left = 1) and (R.Bottom - R.Top = 1) then
              Pixels[R.Left, R.Top] := ThisColor
            else begin
              Brush.Color := ThisColor;
              PatBlt(Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PatCopy);
            end;
          end;
        finally
          R.Left := R.Right;
        end;
      end;
    finally
      R.Top := R.Bottom;
    end;
  end;
end;

procedure scdDrawTriGradient(ACanvas: TCanvas; R: TRect; Color1, Color2, Color3: TColor);
var
  Os: Integer;
  verts: array[0..2] of TRIVERTEX;
  GradTri: GRADIENT_TRIANGLE;
  C1, C2, C3: LongInt;
begin
  if (ACanvas = nil) or IsRectEmpty(R) then Exit;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
  end;

  if ((R.Right - R.Left = 1) and (R.Bottom - R.Top = 1)) or
    ((Color1 = Color2) and (Color1 = Color3)) then
  begin
    with ACanvas do
    begin
      Brush.Color := Color1;
      FillRect(R);
    end;

    Exit;
  end;

  Os := scdCurrentOperatingSystem;
  if (Os = scdOsUnknown) or (Os = scdOsWin95) or (Os = scdOsWinNT) then
  begin
    scdFillTriGradient(ACanvas, R, Color1, Color2, Color3, 255);
    Exit;
  end;

  if GradientLibrary <= 0 then
  begin
    scdFillTriGradient(ACanvas, R, Color1, Color2, Color3, 255);
    Exit;
  end;

  @GradientFill := GetProcAddress(GradientLibrary, PChar('GradientFill'));
  if @GradientFill = nil then
  begin
    scdFillTriGradient(ACanvas, R, Color1, Color2, Color3, 255);
    Exit;
  end;

  C1 := ColorToRGB(Color1);
  C2 := ColorToRGB(Color2);
  C3 := ColorToRGB(Color3);

  verts[0].x := R.Left;
  verts[0].y := R.Bottom;

  verts[1].x := R.Right;
  verts[1].y := R.Top;

  verts[2].x := R.Right;
  verts[2].y := R.Bottom;

  verts[0].Red   := GetRValue(C1) shl 8;
  verts[0].Green := GetGValue(C1) shl 8;
  verts[0].Blue  := GetBValue(C1) shl 8;

  verts[1].Red   := GetRValue(C2) shl 8;
  verts[1].Green := GetGValue(C2) shl 8;
  verts[1].Blue  := GetBValue(C2) shl 8;

  verts[2].Red   := GetRValue(C3) shl 8;
  verts[2].Green := GetGValue(C3) shl 8;
  verts[2].Blue  := GetBValue(C3) shl 8;

  GradTri.Vertex1 := 0;
  GradTri.Vertex2 := 1;
  GradTri.Vertex3 := 2;

  GradientFill(ACanvas.Handle, LongInt(@verts), 3, LongInt(@GradTri), 1, GRADIENT_FILL_TRIANGLE);

  verts[0].x := R.Left;
  verts[0].y := R.Top;

  verts[1].x := R.Right;
  verts[1].y := R.Top;

  verts[2].x := R.Left;
  verts[2].y := R.Bottom;

  verts[2].Red   := verts[0].Red;
  verts[2].Green := verts[0].Green;
  verts[2].Blue  := verts[0].Blue;

  GradientFill(ACanvas.Handle, LongInt(@verts), 3, LongInt(@GradTri), 1, GRADIENT_FILL_TRIANGLE);
end;

procedure CbFastFillTriGradient(ACanvas: TCanvas; ARect: TRect; Color1, Color2, Color3: TColor);
var
  R : TRect;
  I, J, W, H,
  RectTop, EndColor : Integer;
  C1, C2, C3 : LongInt;
  RGB1, RGB2, RGB3 : array[0..2] of Byte;
  XDif, YDif : array[0..2] of Integer;
begin
  if (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  with ACanvas do
  begin
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
  end;

  C1 := ColorToRGB(Color1);
  C2 := ColorToRGB(Color2);
  C3 := ColorToRGB(Color3);

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  YDif[0] := GetRValue(C3) - RGB2[0];
  YDif[1] := GetGValue(C3) - RGB2[1];
  YDif[2] := GetBValue(C3) - RGB2[2];

  RGB3[0] := RGB1[0];
  RGB3[1] := RGB1[1];
  RGB3[2] := RGB1[2];

  for I := 0 to 64 do
  begin
    EndColor := ColorToRGB(RGB(RGB2[0] + MulDiv(4*I, YDif[0], 255),
                               RGB2[1] + MulDiv(4*I, YDif[1], 255),
                               RGB2[2] + MulDiv(4*I, YDif[2], 255)));

    XDif[0] := GetRValue(EndColor) - RGB3[0];
    XDif[1] := GetGValue(EndColor) - RGB3[1];
    XDif[2] := GetBValue(EndColor) - RGB3[2];

    RectTop := MulDiv(4*I, H, 255);
    for J := 0 to 64 do
    begin
      R := Rect(MulDiv(4*J, W, 255), RectTop, MulDiv(4*(J + 1), W, 255),
                RectTop + MulDiv(5, H, 255));
      
      OffsetRect(R, ARect.Left, ARect.Top);
      IntersectRect(R, R, ARect);

      if IsRectEmpty(R) then Continue;

      with ACanvas do
      begin
        Brush.Color := RGB(RGB3[0] + MulDiv(4*J, XDif[0], 255),
                           RGB3[1] + MulDiv(4*J, XDif[1], 255),
                           RGB3[2] + MulDiv(4*J, XDif[2], 255));
        FillRect(R);
      end;
    end;
  end;
end;

procedure scdFillFourGradient(ACanvas: TCanvas; ARect: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor; ColorCount: Integer);
var
  R: TRect;
  I, J, W, H,
  XCount, YCount: Integer;
  XDif, LeftDif, RightDif,
  StartRGB, endRGB, ThisRGB,
  LT_RGB, RT_RGB, LB_RGB, RB_RGB: array[0..2] of DWord;
  LT_C, RT_C, LB_C, RB_C: LongInt;
  ThisColor: TColor;
begin
  if (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
  end;

  if ((W = 1) and (H = 1)) or
    ((LeftTopColor = LeftBottomColor) and
     (LeftTopColor = RightTopColor) and (RightTopColor = RightBottomColor)) then
  begin
    with ACanvas do
    begin
      Brush.Color := LeftTopColor;
      FillRect(ARect);
    end;

    Exit;
  end;

  LT_C := ColorToRGB(LeftTopColor);
  RT_C := ColorToRGB(RightTopColor);
  LB_C := ColorToRGB(LeftBottomColor);
  RB_C := ColorToRGB(RightBottomColor);

  LT_RGB[0] := GetRValue(LT_C);
  LT_RGB[1] := GetGValue(LT_C);
  LT_RGB[2] := GetBValue(LT_C);

  RT_RGB[0] := GetRValue(RT_C);
  RT_RGB[1] := GetGValue(RT_C);
  RT_RGB[2] := GetBValue(RT_C);

  LB_RGB[0] := GetRValue(LB_C);
  LB_RGB[1] := GetGValue(LB_C);
  LB_RGB[2] := GetBValue(LB_C);

  RB_RGB[0] := GetRValue(RB_C);
  RB_RGB[1] := GetGValue(RB_C);
  RB_RGB[2] := GetBValue(RB_C);

  LeftDif[0] := LT_RGB[0] - LB_RGB[0];
  LeftDif[1] := LT_RGB[1] - LB_RGB[1];
  LeftDif[2] := LT_RGB[2] - LB_RGB[2];

  RightDif[0] := RT_RGB[0] - RB_RGB[0];
  RightDif[1] := RT_RGB[1] - RB_RGB[1];
  RightDif[2] := RT_RGB[2] - RB_RGB[2];

  XCount := ColorCount;
  if XCount > W - 1 then XCount := W - 1;

  YCount := ColorCount;
  if YCount > H - 1 then YCount := H - 1;

  if XCount > 255 then XCount := 255;
  if YCount > 255 then YCount := 255;

  R.Bottom := ARect.Top;
  for J := 0 to YCount do
  begin
    R.Left  := ARect.Left;
    R.Right := R.Left;

    Inc(R.Bottom, MulDiv(J, H-1, YCount) - MulDiv(J-1, H-1, YCount));
    if R.Bottom = R.Top then Inc(R.Bottom);

    StartRGB[0] := LT_RGB[0] - DWord(Muldiv(J, LeftDif[0], YCount));
    StartRGB[1] := LT_RGB[1] - DWord(Muldiv(J, LeftDif[1], YCount));
    StartRGB[2] := LT_RGB[2] - DWord(Muldiv(J, LeftDif[2], YCount));

    endRGB[0] := RT_RGB[0] - DWord(Muldiv(J, RightDif[0], YCount));
    endRGB[1] := RT_RGB[1] - DWord(Muldiv(J, RightDif[1], YCount));
    endRGB[2] := RT_RGB[2] - DWord(Muldiv(J, RightDif[2], YCount));

    XDif[0] := StartRGB[0] - endRGB[0];
    XDif[1] := StartRGB[1] - endRGB[1];
    XDif[2] := StartRGB[2] - endRGB[2];

    try
      for I := 0 to XCount do
      begin
        Inc(R.Right, MulDiv(I, W-1, XCount) - MulDiv(I-1, W-1, XCount));
        if R.Right = R.Left then Inc(R.Right);

        try
          IntersectRect(R, R, ARect);
          if IsRectEmpty(R) then Continue;

          ThisRGB[0] := StartRGB[0] - DWord(Muldiv(I, XDif[0], XCount));
          ThisRGB[1] := StartRGB[1] - DWord(Muldiv(I, XDif[1], XCount));
          ThisRGB[2] := StartRGB[2] - DWord(Muldiv(I, XDif[2], XCount));

          with ACanvas do
          begin
            ThisColor := TColor(RGB(ThisRGB[0], ThisRGB[1], ThisRGB[2]));

            if (R.Right - R.Left = 1) and (R.Bottom - R.Top = 1) then
              Pixels[R.Left, R.Top] := ThisColor
            else begin
              Brush.Color := ThisColor;
              PatBlt(Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, PatCopy);
            end;
          end;
        finally
          R.Left := R.Right;
        end;
      end;
    finally
      R.Top := R.Bottom;
    end;
  end;
end;

procedure scdDrawFourGradient(ACanvas: TCanvas; R: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor);
var
  Os: Integer;
  verts: array[0..4] of TRIVERTEX;
  GradTri:array[0..1] of GRADIENT_TRIANGLE;
  LT_C, RT_C, LB_C, RB_C: LongInt;
begin
  if (ACanvas = nil) or IsRectEmpty(R) then Exit;

  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
  end;

  if ((R.Right - R.Left = 1) and (R.Bottom - R.Top = 1)) or
     ((LeftTopColor = LeftBottomColor) and
      (LeftTopColor = RightTopColor) and (RightTopColor = RightBottomColor)) then
  begin
    with ACanvas do
    begin
      Brush.Color := LeftTopColor;
      FillRect(R);
    end;

    Exit;
  end;

  Os := scdCurrentOperatingSystem;
  if (Os = scdOsUnknown) or (Os = scdOsWin95) or (Os = scdOsWinNT) then
  begin
    scdFillFourGradient(ACanvas, R, LeftTopColor, RightTopColor,
      LeftBottomColor, RightBottomColor, 255);
    Exit;
  end;

  if GradientLibrary <= 0 then
  begin
    scdFillFourGradient(ACanvas, R, LeftTopColor, RightTopColor,
      LeftBottomColor, RightBottomColor, 255);
    Exit;
  end;

  @GradientFill := GetProcAddress(GradientLibrary, PChar('GradientFill'));
  if @GradientFill = nil then
  begin
    scdFillFourGradient(ACanvas, R, LeftTopColor, RightTopColor,
      LeftBottomColor, RightBottomColor, 255);
    Exit;
  end;

  LT_C := ColorToRGB(LeftTopColor);
  RT_C := ColorToRGB(RightTopColor);
  LB_C := ColorToRGB(LeftBottomColor);
  RB_C := ColorToRGB(RightBottomColor);

  verts[0].x     := R.Left;
  verts[0].y     := R.Top;
  verts[0].Red   := GetRValue(LT_C) shl 8;
  verts[0].Green := GetGValue(LT_C) shl 8;
  verts[0].Blue  := GetBValue(LT_C) shl 8;
  verts[0].Alpha := $0000;

  verts[1].x     := R.Right;
  verts[1].y     := R.Top;
  verts[1].Red   := GetRValue(RT_C) shl 8;
  verts[1].Green := GetGValue(RT_C) shl 8;
  verts[1].Blue  := GetBValue(RT_C) shl 8;
  verts[1].Alpha := $0000;

  verts[2].x     := R.Right;
  verts[2].y     := R.Bottom;
  verts[2].Red   := GetRValue(RB_C) shl 8;
  verts[2].Green := GetGValue(RB_C) shl 8;
  verts[2].Blue  := GetBValue(RB_C) shl 8;
  verts[2].Alpha := $0000;

  verts[3].x     := R.Left;
  verts[3].y     := R.Bottom;
  verts[3].Red   := GetRValue(LB_C) shl 8;
  verts[3].Green := GetGValue(LB_C) shl 8;
  verts[3].Blue  := GetBValue(LB_C) shl 8;
  verts[3].Alpha := $0000;

  GradTri[0].Vertex1 := 0;
  GradTri[0].Vertex2 := 1;
  GradTri[0].Vertex3 := 3;

  GradTri[1].Vertex1 := 1;
  GradTri[1].Vertex2 := 2;
  GradTri[1].Vertex3 := 3;

  GradientFill(ACanvas.Handle, LongInt(@verts), 4, LongInt(@GradTri), 2, GRADIENT_FILL_TRIANGLE);
end;

procedure scdFillVerticalRainbow(ACanvas: TCanvas; ARect: TRect; Reverse: Boolean);
var
  I, SecCount, H: Integer;
  StColor, EndColor: TColor;
  R: TRect;
begin
  SecCount := Length(scdRainbowColors) - 1;
  if (SecCount <= 1) or (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;

  H := ARect.Bottom - ARect.Top;

  R.Bottom := R.Top;

  for I := 1 to SecCount do
  begin
    R.Top  := ARect.Top + Muldiv(I-1, H, SecCount);
    R.Bottom := ARect.Top + Muldiv(I, H, SecCount);

    if Reverse then
    begin
      StColor  := scdRainbowColors[High(scdRainbowColors) - (I - 1)];
      EndColor := scdRainbowColors[High(scdRainbowColors) - I];
    end else
    begin
      StColor  := scdRainbowColors[Low(scdRainbowColors) + I - 1];
      EndColor := scdRainbowColors[Low(scdRainbowColors) + I];
    end;

    scdFillGradientRectVertical(ACanvas, R, StColor, EndColor, 255, False);
  end;
end;

procedure scdFillHorizontalRainbow(ACanvas: TCanvas; ARect: TRect; Reverse: Boolean);
var
  I, SecCount, W: Integer;
  StColor, EndColor: TColor;
  R: TRect;
begin
  SecCount := Length(scdRainbowColors) - 1;
  if (SecCount <= 1) or (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  R.Right := R.Left;

  W := ARect.Right - ARect.Left;

  for I := 1 to SecCount do
  begin
    R.Left  := ARect.Left + Muldiv(I-1, W, SecCount);
    R.Right := ARect.Left + Muldiv(I, W, SecCount);

    if Reverse then
    begin
      StColor  := scdRainbowColors[High(scdRainbowColors) - (I - 1)];
      EndColor := scdRainbowColors[High(scdRainbowColors) - I];
    end else
    begin
      StColor  := scdRainbowColors[Low(scdRainbowColors) + I - 1];
      EndColor := scdRainbowColors[Low(scdRainbowColors) + I];
    end;

    scdFillGradientRectHorizontal(ACanvas, R, StColor, EndColor, 255, False);
  end;
end;

procedure scdFillVerticalExRainbow(ACanvas: TCanvas; ARect: TRect; ExColor: TColor;
  VertReverse, HorzReverse: Boolean);
var
  I, SecCount, H: Integer;
  StColor, EndColor: TColor;
  R: TRect;
begin
  SecCount := Length(scdRainbowColors) - 1;
  if (SecCount <= 1) or (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;

  H := ARect.Bottom - ARect.Top;

  R.Bottom := R.Top;

  for I := 1 to SecCount do
  begin
    R.Top  := ARect.Top + Muldiv(I-1, H, SecCount);
    R.Bottom := ARect.Top + Muldiv(I, H, SecCount);

    if VertReverse then
    begin
      StColor  := scdRainbowColors[High(scdRainbowColors) - (I - 1)];
      EndColor := scdRainbowColors[High(scdRainbowColors) - I];
    end else
    begin
      StColor  := scdRainbowColors[Low(scdRainbowColors) + I - 1];
      EndColor := scdRainbowColors[Low(scdRainbowColors) + I];
    end;

    if HorzReverse then
      scdFillFourGradient(ACanvas, R, ExColor, ExColor, StColor, EndColor, 255)
    else
      scdFillFourGradient(ACanvas, R, StColor, EndColor, ExColor, ExColor, 255);
  end;
end;

procedure scdFillHorizontalExRainbow(ACanvas: TCanvas; ARect: TRect; ExColor: TColor;
  VertReverse, HorzReverse: Boolean);
var
  I, SecCount, W: Integer;
  StColor, EndColor: TColor;
  R: TRect;
begin
  SecCount := Length(scdRainbowColors) - 1;
  if (SecCount <= 1) or (ACanvas = nil) or IsRectEmpty(ARect) then Exit;

  R := ARect;
  R.Right := R.Left;

  W := ARect.Right - ARect.Left;

  for I := 1 to SecCount do
  begin
    R.Left  := ARect.Left + Muldiv(I-1, W, SecCount);
    R.Right := ARect.Left + Muldiv(I, W, SecCount);

    if HorzReverse then
    begin
      StColor  := scdRainbowColors[High(scdRainbowColors) - (I - 1)];
      EndColor := scdRainbowColors[High(scdRainbowColors) - I];
    end else
    begin
      StColor  := scdRainbowColors[Low(scdRainbowColors) + I - 1];
      EndColor := scdRainbowColors[Low(scdRainbowColors) + I];
    end;

    if VertReverse then
      scdFillFourGradient(ACanvas, R, ExColor, ExColor, StColor, EndColor, 255)
    else
      scdFillFourGradient(ACanvas, R, StColor, EndColor, ExColor, ExColor, 255);
  end;
end;

function scdVerticalGradientColorOf(P: TPoint; R: TRect; StartColor, EndColor: TColor;
  ColorCount: Integer; Reverse: Boolean): TColor;
var
  J, Y, H: Integer;
  YDif, RGB1, RGB2: array[0..2] of DWord;
  C1, C2, Tmp: LongInt;
begin
  Result := StartColor;

  H := R.Bottom - R.Top;
  if (H <= 1) or (StartColor = EndColor) or not PtInRect(R, P) then Exit;

  C1 := ColorToRGB(StartColor);
  C2 := ColorToRGB(EndColor);

  if Reverse then
  begin
    Tmp := C1;
    C1  := C2;
    C2  := Tmp;
  end;

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  if ColorCount > H - 1 then
    ColorCount := H - 1;

  if ColorCount > 255 then ColorCount := 255;

  YDif[0] := RGB1[0] - RGB2[0];
  YDif[1] := RGB1[1] - RGB2[1];
  YDif[2] := RGB1[2] - RGB2[2];

  Y := P.Y - R.Top;
  J := Muldiv(Y, ColorCount, H);

  RGB2[0] := RGB1[0] - DWord(Muldiv(J, YDif[0], ColorCount));
  RGB2[1] := RGB1[1] - DWord(Muldiv(J, YDif[1], ColorCount));
  RGB2[2] := RGB1[2] - DWord(Muldiv(J, YDif[2], ColorCount));

  Result := TColor(RGB(RGB2[0], RGB2[1], RGB2[2]));
end;

function scdHorizontalGradientColorOf(P: TPoint; R: TRect; StartColor, EndColor: TColor;
  ColorCount: Integer; Reverse: Boolean): TColor;
var
  I, X, W: Integer;
  XDif, RGB1, RGB2: array[0..2] of DWord;
  C1, C2, Tmp: LongInt;
begin
  Result := StartColor;
  if Reverse then Result := EndColor;

  W := R.Right - R.Left;
  if (W <= 1) or (StartColor = EndColor) or (P.Y < R.Top) then Exit;

  if P.Y > R.Bottom then
  begin
    Result := EndColor;
    if Reverse then Result := StartColor;

    Exit;
  end;

  C1 := ColorToRGB(StartColor);
  C2 := ColorToRGB(EndColor);

  if Reverse then
  begin
    Tmp := C1;
    C1  := C2;
    C2  := Tmp;
  end;

  RGB1[0] := GetRValue(C1);
  RGB1[1] := GetGValue(C1);
  RGB1[2] := GetBValue(C1);

  RGB2[0] := GetRValue(C2);
  RGB2[1] := GetGValue(C2);
  RGB2[2] := GetBValue(C2);

  if ColorCount > W - 1 then
    ColorCount := W - 1;

  if ColorCount > 255 then ColorCount := 255;

  XDif[0] := RGB1[0] - RGB2[0];
  XDif[1] := RGB1[1] - RGB2[1];
  XDif[2] := RGB1[2] - RGB2[2];

  X := P.x - R.Left;
  I := Muldiv(X, ColorCount, W);

  RGB2[0] := RGB1[0] - DWord(Muldiv(I, XDif[0], ColorCount));
  RGB2[1] := RGB1[1] - DWord(Muldiv(I, XDif[1], ColorCount));
  RGB2[2] := RGB1[2] - DWord(Muldiv(I, XDif[2], ColorCount));

  Result := TColor(RGB(RGB2[0], RGB2[1], RGB2[2]));
end;

function scdGradientFourColorOf(P: TPoint; R: TRect; LeftTopColor, RightTopColor,
  LeftBottomColor, RightBottomColor: TColor; ColorCount: Integer): TColor;
var
  I, J, W, H,
  X, Y, XCount, YCount: Integer;
  XDif, LeftDif, RightDif,
  StartRGB, endRGB, ThisRGB,
  LT_RGB, RT_RGB, LB_RGB, RB_RGB: array[0..2] of DWord;
  LT_C, RT_C, LB_C, RB_C: LongInt;
begin
  Result := LeftTopColor;

  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W <= 0) or (H <= 0) or ((W = 1) and (H = 1)) or
    ((LeftTopColor = RightTopColor) and (LeftBottomColor = RightBottomColor) and
     (LeftTopColor = LeftBottomColor)) or not PtInRect(R, P) then Exit;

  LT_C := ColorToRGB(LeftTopColor);
  RT_C := ColorToRGB(RightTopColor);
  LB_C := ColorToRGB(LeftBottomColor);
  RB_C := ColorToRGB(RightBottomColor);

  LT_RGB[0] := GetRValue(LT_C);
  LT_RGB[1] := GetGValue(LT_C);
  LT_RGB[2] := GetBValue(LT_C);

  RT_RGB[0] := GetRValue(RT_C);
  RT_RGB[1] := GetGValue(RT_C);
  RT_RGB[2] := GetBValue(RT_C);

  LB_RGB[0] := GetRValue(LB_C);
  LB_RGB[1] := GetGValue(LB_C);
  LB_RGB[2] := GetBValue(LB_C);

  RB_RGB[0] := GetRValue(RB_C);
  RB_RGB[1] := GetGValue(RB_C);
  RB_RGB[2] := GetBValue(RB_C);

  LeftDif[0] := LT_RGB[0] - LB_RGB[0];
  LeftDif[1] := LT_RGB[1] - LB_RGB[1];
  LeftDif[2] := LT_RGB[2] - LB_RGB[2];

  RightDif[0] := RT_RGB[0] - RB_RGB[0];
  RightDif[1] := RT_RGB[1] - RB_RGB[1];
  RightDif[2] := RT_RGB[2] - RB_RGB[2];

  XCount := ColorCount;
  if XCount > W - 1 then XCount := W - 1;

  YCount := ColorCount;
  if YCount > H - 1 then YCount := H - 1;

  if XCount > 255 then XCount := 255;
  if YCount > 255 then YCount := 255;

  X := P.X - R.Left;
  I := Muldiv(X, XCount, W);

  Y := P.Y - R.Top;
  J := Muldiv(Y, YCount, H);

  StartRGB[0] := LT_RGB[0] - DWord(Muldiv(J, LeftDif[0], YCount));
  StartRGB[1] := LT_RGB[1] - DWord(Muldiv(J, LeftDif[1], YCount));
  StartRGB[2] := LT_RGB[2] - DWord(Muldiv(J, LeftDif[2], YCount));

  endRGB[0] := RT_RGB[0] - DWord(Muldiv(J, RightDif[0], YCount));
  endRGB[1] := RT_RGB[1] - DWord(Muldiv(J, RightDif[1], YCount));
  endRGB[2] := RT_RGB[2] - DWord(Muldiv(J, RightDif[2], YCount));

  XDif[0] := StartRGB[0] - endRGB[0];
  XDif[1] := StartRGB[1] - endRGB[1];
  XDif[2] := StartRGB[2] - endRGB[2];

  ThisRGB[0] := StartRGB[0] - DWord(Muldiv(I, XDif[0], XCount));
  ThisRGB[1] := StartRGB[1] - DWord(Muldiv(I, XDif[1], XCount));
  ThisRGB[2] := StartRGB[2] - DWord(Muldiv(I, XDif[2], XCount));

  Result := TColor(RGB(ThisRGB[0], ThisRGB[1], ThisRGB[2]));
end;

procedure scdDrawFocusRect(C: TCanvas; R: TRect; LineColor: TColor);
var
  B: Boolean;
  I, X, Y: Integer;
begin
  Dec(R.Right); Dec(R.Bottom);
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  with C do
  begin
    Pen.Color := LineColor;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Mode  := pmNotMerge;

    X := R.Left;
    Y := R.Top;

    OffsetRect(R, -R.Left, -R.Top);

    B := True;
    for I := 0 to R.Right-1 do
    begin
      if B then
      begin
        MoveTo(I + X, Y);
        LineTo(I + X, Y + 1);
      end;

      B := not B;
    end;

    for I := 0 to R.Bottom-1 do
    begin
      if B then
      begin
        MoveTo(X + R.Right,     Y + I);
        LineTo(X + R.Right - 1, Y + I + 1);
      end;

      B := not B;
    end;

    for I := R.Right downto 1 do
    begin
      if B then
      begin
        MoveTo(I + X, Y + R.Bottom);
        LineTo(I + X, Y + R.Bottom - 1);
      end;

      B := not B;
    end;

    for I := R.Bottom downto 1 do
    begin
      if B then
      begin
        MoveTo(X,     Y + I);
        LineTo(X + 1, Y + I);
      end;

      B := not B;
    end;

    Pen.Mode := pmCopy;
  end;
end;

procedure scdFrame3D(C: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor;
  LineWidth, Rounded: Integer; UseNoneColor: Boolean);
var
  R: TRect;
  RD: Integer;
  TopPoints, BtmPoints: array of TPoint;
  PenStyle: TPenStyle;
  PenWidth: Integer;
  PenColor: TColor;
  PenMode: TPenMode;
begin
  if (C = nil) or IsRectEmpty(ARect) then
    Exit;

  R := ARect;
  InflateRect(ARect, -1, -1);

  Dec(R.Bottom);
  Dec(R.Right);

  if R.Right < R.Left then R.Right := R.Left;
  if R.Bottom < R.Top then R.Bottom := R.Top;

  with C do
  begin
    PenColor := Pen.Color;
    PenStyle := Pen.Style;
    PenMode  := Pen.Mode;
    PenWidth := Pen.Width;

    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    
    try
      if IsRectEmpty(R) then
      begin
        Pen.Width := 1;
        Pen.Color := BottomColor;
        PolyLine([Point(R.Left, R.Top), Point(R.Right, R.Bottom)]);
        Exit;
      end;

      RD := Rounded div 2;
      if RD > 0 then
      begin
        if RD > R.Bottom - R.Top then
          RD := R.Bottom - R.Top;
        if RD > R.Right - R.Left then
          RD := R.Right - R.Left;
      end;

      SetLength(TopPoints, 5);

      TopPoints[0].x := R.Left + RD;
      TopPoints[0].y := R.Bottom;
      TopPoints[1].x := R.Left;
      TopPoints[1].y := R.Bottom - RD;
      TopPoints[2].x := R.Left;
      TopPoints[2].y := R.Top + RD;
      TopPoints[3].x := R.Left + RD;
      TopPoints[3].y := R.Top;
      TopPoints[4].x := R.Right - RD;
      TopPoints[4].y := R.Top;

      SetLength(BtmPoints, 5);

      BtmPoints[0].x := R.Right - RD;
      BtmPoints[0].y := R.Top;
      BtmPoints[1].x := R.Right;
      BtmPoints[1].y := R.Top + RD;
      BtmPoints[2].x := R.Right;
      BtmPoints[2].y := R.Bottom - RD;
      BtmPoints[3].x := R.Right - RD;
      BtmPoints[3].y := R.Bottom;
      BtmPoints[4].x := R.Left + RD - 1;
      BtmPoints[4].y := R.Bottom;

      Pen.Width := LineWidth;
      PenStyle  := psInsideFrame;

      if (TopColor <> clNone) or UseNoneColor then
      begin
        Pen.Color := TopColor;
        PolyLine(TopPoints);
      end;

      if (BottomColor <> clNone) or UseNoneColor then
      begin
        Pen.Color := BottomColor;
        PolyLine(BtmPoints);
      end;  
    finally
      Pen.Color := PenColor;
      Pen.Style := PenStyle;
      Pen.Width := PenWidth;
      Pen.Mode  := PenMode;
    end;
  end;
end;

procedure scdRoundRect(C: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor;
  LineWidth: Integer);
var
  I: Integer;
  OldPenColor: TColor;
  Sx, Sy, Ex, Ey: Integer;
begin
  if (C = nil) or IsRectEmpty(ARect) then
    Exit;

  with C do
  begin
    OldPenColor := Pen.Color;

    Sx := ARect.Left;
    Sy := ARect.Bottom;
    Ex := ARect.Right;
    Ey := ARect.Top;

    for I := 0 to LineWidth-1 do
    begin
      Pen.Color := BottomColor;
      Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        Sx, Sy, Ex, Ey);

      Pen.Color := TopColor;
      Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        Ex, Ey, Sx, Sy);

      InflateRect(ARect, -1, -1);
    end;
    Pen.Color := OldPenColor;
  end;
end;

procedure scdFillCorners(C: TCanvas; ARect: TRect; AColor: TColor; Rounded: Integer);
var
  h, rh: HRGN;
  OldBrushColor: TColor;
begin
  if (C = nil) or IsRectEmpty(ARect) then
    Exit;

  with C do
  begin
    OldBrushColor := Brush.Color;
    Brush.Color := AColor;

    h := 0;
    rh := 0;
    try
      h := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      rh := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right+1,
        ARect.Bottom + 1, Rounded, Rounded);

      CombineRgn(h, h, rh, RGN_DifF);
      PaintRgn(Handle, h);
    finally
      if h <> 0 then DeleteObject(h);
      if rh <> 0 then DeleteObject(rh);
    end;

    Brush.Color := OldBrushColor;
  end;
end;

procedure scdFillWithColors(C: TCanvas; R: TRect; AColor, WithColor: TColor);
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  with C do
  begin
    if AColor = WithColor then
      Brush.Color := AColor
    else
      Brush.Bitmap := AllocPatternBitmap(AColor, WithColor);
      
    FillRect(R);
    Brush.Bitmap := nil;
  end;
end;

procedure scdDrawBevelEx(DC: HDC; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges);

  procedure Draw3DFrame(R: TRect; TCl, BCl: TColor; P: TPen);
  begin
    if R.Right > R.Left then Dec(R.Right);
    if R.Bottom > R.Top then Dec(R.Bottom);

    P.Color := TCl;
    SelectObject(DC, P.Handle);

    if scdbeTop in Edges then
    begin
      Windows.MoveToEx(DC, R.Right, R.Top, nil);
      Windows.LineTo(DC, R.Left, R.Top);
    end;  

    if scdbeLeft in Edges then
    begin
      Windows.MoveToEx(DC, R.Left, R.Top, nil);
      Windows.LineTo(DC, R.Left, R.Bottom);
    end;

    P.Color := BCl;
    SelectObject(DC, P.Handle);

    if scdbeBottom in Edges then
    begin
      Windows.MoveToEx(DC, R.Left, R.Bottom, nil);
      Windows.LineTo(DC, R.Right, R.Bottom);
    end;

    if scdbeRight in Edges then
    begin
      Windows.MoveToEx(DC, R.Right, R.Bottom, nil);
      Windows.LineTo(DC, R.Right, R.Top);
    end;  
  end;

  procedure DrawFrameRounded(R: TRect; Cl: TColor; P: TPen; AFillCorner: Boolean;
    RoundBy: Word = 2);
  var
    BR: TRect;
  begin
    BR := R;
    if R.Right > R.Left then Dec(R.Right);
    if R.Bottom > R.Top then Dec(R.Bottom);

    if (BR.Right < BR.Left + 2*RoundBy) or (BR.Bottom < BR.Top + 2*RoundBy) then
    begin
      Draw3DFrame(BR, Cl, Cl, P);
      Exit;
    end;

    P.Color := Cl;
    SelectObject(DC, P.Handle);

    if scdbeTop in Edges then
    begin
      if scdbeRight in Edges then
        Windows.MoveToEx(DC, R.Right - RoundBy, R.Top, nil)
      else
        Windows.MoveToEx(DC, R.Right, R.Top, nil);

      if scdbeLeft in Edges then
      begin
        Windows.LineTo(DC, R.Left + RoundBy, R.Top);
        Windows.LineTo(DC, R.Left, R.Top + RoundBy);
      end else
        Windows.LineTo(DC, R.Left - 1, R.Top);
    end;

    if scdbeLeft in Edges then
    begin
      if scdbeTop in Edges then
        Windows.MoveToEx(DC, R.Left, R.Top + RoundBy, nil)
      else
        Windows.MoveToEx(DC, R.Left, R.Top, nil);

      if scdbeBottom in Edges then
      begin
        Windows.LineTo(DC, R.Left, R.Bottom - RoundBy);
        Windows.LineTo(DC, R.Left + RoundBy, R.Bottom);
      end else
        Windows.LineTo(DC, R.Left, R.Bottom + 1);
    end;

    if scdbeBottom in Edges then
    begin
      if scdbeLeft in Edges then
        Windows.MoveToEx(DC, R.Left + RoundBy, R.Bottom, nil)
      else  
        Windows.MoveToEx(DC, R.Left, R.Bottom, nil);

      if scdbeRight in Edges then
      begin
        Windows.LineTo(DC, R.Right - RoundBy, R.Bottom);
        Windows.LineTo(DC, R.Right, R.Bottom - RoundBy);
      end else
        Windows.LineTo(DC, R.Right + 1, R.Bottom);
    end;

    if scdbeRight in Edges then
    begin
      if scdbeBottom in Edges then
        Windows.MoveToEx(DC, R.Right, R.Bottom - RoundBy, nil)
      else
        Windows.MoveToEx(DC, R.Right, R.Bottom, nil);

      if scdbeTop in Edges then
      begin
        Windows.LineTo(DC, R.Right, R.Top + RoundBy);
        Windows.LineTo(DC, R.Right - RoundBy, R.Top);
      end else
        Windows.LineTo(DC, R.Right, R.Top - 1);
    end;

    if AFillCorner and (RoundBy = 2) and (Edges <> []) and (ParentColor <> clNone) then
    begin
      P.Color := ParentColor;
      SelectObject(DC, P.Handle);

      if (scdbeTop in Edges) and (scdbeLeft in Edges) then
      begin
        Windows.MoveToEx(DC, R.Left + 1, R.Top, nil);

        Windows.LineTo(DC, R.Left, R.Top);
        Windows.LineTo(DC, R.Left, R.Top + 1);
        Windows.LineTo(DC, R.Left + 1, R.Top);
      end;

      if (scdbeLeft in Edges) and (scdbeBottom in Edges) then
      begin
        Windows.MoveToEx(DC, R.Left, R.Bottom - 1, nil);

        Windows.LineTo(DC, R.Left, R.Bottom);
        Windows.LineTo(DC, R.Left + 1, R.Bottom);
        Windows.LineTo(DC, R.Left, R.Bottom - 1);
      end;

      if (scdbeBottom in Edges) and (scdbeRight in Edges) then
      begin
        Windows.MoveToEx(DC, R.Right - 1, R.Bottom, nil);

        Windows.LineTo(DC, R.Right, R.Bottom);
        Windows.LineTo(DC, R.Right, R.Bottom - 1);
        Windows.LineTo(DC, R.Right - 1, R.Bottom);
      end;

      if (scdbeRight in Edges) and (scdbeTop in Edges) then
      begin
        Windows.MoveToEx(DC, R.Right, R.Top + 1, nil);

        Windows.LineTo(DC, R.Right, R.Top);
        Windows.LineTo(DC, R.Right - 1, R.Top);
        Windows.LineTo(DC, R.Right, R.Top + 1);
      end;
    end;
  end;
  
var
  TopC, BtmC: TColor;
  P: TPen;
  OldPen: HPen;
begin
  if (DC = 0) or (B = scdcbNone) or IsRectEmpty(R) or (Edges = []) then
    Exit;

  OldPen := 0;
  P := TPen.Create;
  try
    P.Width := 1;
    P.Style := psSolid;

    SetROP2(DC, R2_COPYPEN);
    OldPen := SelectObject(DC, P.Handle);

    case B of
      scdcbRaised:
      begin
        TopC := scdGetBtnHighlightOf(C);
        BtmC := scdGetBtnShadowOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcbLowered:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcb3DRaised:
      begin
        TopC := scdGetBtnHighlightOf(C);
        BtmC := scdGet3DDkShadowOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
        InflateRect(R, -1, -1);

        TopC := scdGetBtnFaceOf(C);
        BtmC := scdGetBtnShadowOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcb3DLowered:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
        InflateRect(R, -1, -1);

        TopC := scdGet3DDkShadowOf(C);
        BtmC := scdGetBtnFaceOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcbBumped:
      begin
        TopC := scdGetBtnHighlightOf(C);
        BtmC := scdGetBtnShadowOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
        InflateRect(R, -1, -1);

        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcbEtched:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Dec(R.Right); Dec(R.Bottom);
        OffsetRect(R, 1, 1);

        Draw3DFrame(R, BtmC, BtmC, P);
        OffsetRect(R, -1, -1);

        Draw3DFrame(R, TopC, TopC, P);
      end;
      scdcbFlat:
      begin
        Draw3DFrame(R, C, C, P);
      end;
      scdcbFlatBold:
      begin
        Draw3DFrame(R, C, C, P);

        InflateRect(R, -1, -1);
        Draw3DFrame(R, C, C, P);
      end;
      scdcbFlatRounded:
      begin
        DrawFrameRounded(R, C, P, FillCorner, 2);
      end;
      scdcbFlatBoldRounded:
      begin
        DrawFrameRounded(R, C, P, FillCorner, 2);
        InflateRect(R, -1, -1);

        DrawFrameRounded(R, C, P, False, 1);
        DrawFrameRounded(R, C, P, False, 2);
      end;
      scdcbColor:
      begin
        Draw3DFrame(R, C, C, P);
      end;
      scdcbMacLowered:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
        InflateRect(R, -1, -1);

        TopC := scdBlendedColor(C, 212, 212, 212, False);

        Draw3DFrame(R, TopC, TopC, P);
      end;
      scdcbMacRaised:
      begin
        TopC := scdBlendedColor(C, 212, 212, 212, False);
        Draw3DFrame(R, TopC, TopC, P);
        InflateRect(R, -1, -1);

        TopC := scdGetBtnHighlightOf(C);
        BtmC := scdGetBtnShadowOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
      end;
      scdcbMetal:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, BtmC, P);
        InflateRect(R, -1, -1);

        TopC := scdGetBtnFaceOf(C);
        Draw3DFrame(R, TopC, TopC, P);
      end;
      scdcbSoftLowered:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, C, BtmC, P);
        InflateRect(R, -1, -1);
        Draw3DFrame(R, TopC, TopC, P);
      end;
      scdcbSoftRaised:
      begin
        TopC := scdGetBtnShadowOf(C);
        BtmC := scdGetBtnHighlightOf(C);

        Draw3DFrame(R, TopC, TopC, P);
        InflateRect(R, -1, -1);
        Draw3DFrame(R, BtmC, C, P);
      end;
    end;
  finally
    if OldPen <> 0 then SelectObject(DC, OldPen);
    P.Free;
  end;
end;

procedure scdDrawEdgeEx(DC: HDC; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges);

  procedure Draw3DFrame(R: TRect; TCl, BCl: TColor; P: TPen);
  begin
    if R.Right > R.Left then Dec(R.Right);
    if R.Bottom > R.Top then Dec(R.Bottom);

    P.Color := TCl;
    SelectObject(DC, P.Handle);

    if scdbeTop in Edges then
    begin
      Windows.MoveToEx(DC, R.Right, R.Top, nil);
      Windows.LineTo(DC, R.Left, R.Top);
    end;

    if scdbeLeft in Edges then
    begin
      Windows.MoveToEx(DC, R.Left, R.Top, nil);
      Windows.LineTo(DC, R.Left, R.Bottom);
    end;

    P.Color := BCl;
    SelectObject(DC, P.Handle);

    if scdbeBottom in Edges then
    begin
      Windows.MoveToEx(DC, R.Left, R.Bottom, nil);
      Windows.LineTo(DC, R.Right, R.Bottom);
    end;

    if scdbeRight in Edges then
    begin
      Windows.MoveToEx(DC, R.Right, R.Bottom, nil);
      Windows.LineTo(DC, R.Right, R.Top);
    end;  
  end;

  procedure DrawFrameRounded(R: TRect; Cl: TColor; P: TPen; AFillCorner: Boolean;
    RoundBy: Word = 2);
  var
    BR: TRect;
  begin
    BR := R;
    if R.Right > R.Left then Dec(R.Right);
    if R.Bottom > R.Top then Dec(R.Bottom);

    if (BR.Right < BR.Left + 2*RoundBy) or (BR.Bottom < BR.Top + 2*RoundBy) then
    begin
      Draw3DFrame(BR, Cl, Cl, P);
      Exit;
    end;

    P.Color := Cl;
    SelectObject(DC, P.Handle);

    if scdbeTop in Edges then
    begin
      if scdbeRight in Edges then
        Windows.MoveToEx(DC, R.Right - RoundBy, R.Top, nil)
      else
        Windows.MoveToEx(DC, R.Right, R.Top, nil);

      if scdbeLeft in Edges then
      begin
        Windows.LineTo(DC, R.Left + RoundBy, R.Top);
        Windows.LineTo(DC, R.Left, R.Top + RoundBy);
      end else
        Windows.LineTo(DC, R.Left - 1, R.Top);
    end;

    if scdbeLeft in Edges then
    begin
      if scdbeTop in Edges then
        Windows.MoveToEx(DC, R.Left, R.Top + RoundBy, nil)
      else
        Windows.MoveToEx(DC, R.Left, R.Top, nil);

      if scdbeBottom in Edges then
      begin
        Windows.LineTo(DC, R.Left, R.Bottom - RoundBy);
        Windows.LineTo(DC, R.Left + RoundBy, R.Bottom);
      end else
        Windows.LineTo(DC, R.Left, R.Bottom + 1);
    end;

    if scdbeBottom in Edges then
    begin
      if scdbeLeft in Edges then
        Windows.MoveToEx(DC, R.Left + RoundBy, R.Bottom, nil)
      else  
        Windows.MoveToEx(DC, R.Left, R.Bottom, nil);

      if scdbeRight in Edges then
      begin
        Windows.LineTo(DC, R.Right - RoundBy, R.Bottom);
        Windows.LineTo(DC, R.Right, R.Bottom - RoundBy);
      end else
        Windows.LineTo(DC, R.Right + 1, R.Bottom);
    end;

    if scdbeRight in Edges then
    begin
      if scdbeBottom in Edges then
        Windows.MoveToEx(DC, R.Right, R.Bottom - RoundBy, nil)
      else
        Windows.MoveToEx(DC, R.Right, R.Bottom, nil);

      if scdbeTop in Edges then
      begin
        Windows.LineTo(DC, R.Right, R.Top + RoundBy);
        Windows.LineTo(DC, R.Right - RoundBy, R.Top);
      end else
        Windows.LineTo(DC, R.Right, R.Top - 1);
    end;

    if AFillCorner and (RoundBy = 2) and (Edges <> []) and (ParentColor <> clNone) then
    begin
      P.Color := ParentColor;
      SelectObject(DC, P.Handle);

      if (scdbeTop in Edges) and (scdbeLeft in Edges) then
      begin
        Windows.MoveToEx(DC, R.Left + 1, R.Top, nil);

        Windows.LineTo(DC, R.Left, R.Top);
        Windows.LineTo(DC, R.Left, R.Top + 1);
        Windows.LineTo(DC, R.Left + 1, R.Top);
      end;

      if (scdbeLeft in Edges) and (scdbeBottom in Edges) then
      begin
        Windows.MoveToEx(DC, R.Left, R.Bottom - 1, nil);

        Windows.LineTo(DC, R.Left, R.Bottom);
        Windows.LineTo(DC, R.Left + 1, R.Bottom);
        Windows.LineTo(DC, R.Left, R.Bottom - 1);
      end;

      if (scdbeBottom in Edges) and (scdbeRight in Edges) then
      begin
        Windows.MoveToEx(DC, R.Right - 1, R.Bottom, nil);

        Windows.LineTo(DC, R.Right, R.Bottom);
        Windows.LineTo(DC, R.Right, R.Bottom - 1);
        Windows.LineTo(DC, R.Right - 1, R.Bottom);
      end;

      if (scdbeRight in Edges) and (scdbeTop in Edges) then
      begin
        Windows.MoveToEx(DC, R.Right, R.Top + 1, nil);

        Windows.LineTo(DC, R.Right, R.Top);
        Windows.LineTo(DC, R.Right - 1, R.Top);
        Windows.LineTo(DC, R.Right, R.Top + 1);
      end;
    end;
  end;
  
var
  P: TPen;
  OldPen: HPen;
begin
  if (DC = 0) or (B = scdcbNone) or IsRectEmpty(R) or (Edges = []) then
    Exit;

  OldPen := 0;
  P := TPen.Create;
  try
    P.Width := 1;
    P.Style := psSolid;

    SetROP2(DC, R2_COPYPEN);
    OldPen := SelectObject(DC, P.Handle);

    case B of
      scdcb3DRaised:
      begin
        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_ADJUST or BF_RECT);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_ADJUST or BF_RECT);
      end;
      scdcb3DLowered:
      begin
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
        DrawEdge(DC, R, BDR_SUNKENINNER, BF_ADJUST or BF_RECT);
      end;
      scdcbBumped:
      begin
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_ADJUST or BF_RECT);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
      end;
      scdcbEtched:
        DrawEdge(DC, R, EDGE_ETCHED, BF_ADJUST or BF_RECT);
      scdcbRaised:
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_ADJUST or BF_RECT);
      scdcbLowered:
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
      scdcbFlat, scdcbColor:
      begin
        Draw3DFrame(R, C, C, P);
      end;
      scdcbFlatBold:
      begin
        Draw3DFrame(R, C, C, P);
        InflateRect(R, -1, -1);
        Draw3DFrame(R, C, C, P);
      end;
      scdcbFlatRounded:
      begin
        DrawFrameRounded(R, C, P, FillCorner, 2);
      end;
      scdcbFlatBoldRounded:
      begin
        DrawFrameRounded(R, C, P, FillCorner, 2);
        InflateRect(R, -1, -1);

        DrawFrameRounded(R, C, P, False, 1);
        DrawFrameRounded(R, C, P, False, 2);
      end;
      scdcbMacLowered:
      begin
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_ADJUST or BF_RECT);
        Draw3DFrame(R, clWindowFrame, clWindowFrame, P);
      end;
      scdcbMacRaised:
      begin
        Draw3DFrame(R, clWindowFrame, clWindowFrame, P);
        InflateRect(R, -1, -1);

        DrawEdge(DC, R, BDR_RAISEDINNER, BF_ADJUST or BF_RECT);
      end;
      scdcbMetal:
      begin
        Draw3DFrame(R, clBtnShadow, clBtnHighlight, P);

        InflateRect(R, -1, -1);
        Draw3DFrame(R, clBtnFace, clBtnFace, P);
      end;
      scdcbSoftLowered:
      begin
        Draw3DFrame(R, clBtnFace, clBtnHighlight, P);

        InflateRect(R, -1, -1);
        Draw3DFrame(R, clBtnShadow, clBtnShadow, P);
      end;
      scdcbSoftRaised:
      begin
        Draw3DFrame(R, clBtnShadow, clBtnShadow, P);

        InflateRect(R, -1, -1);
        Draw3DFrame(R, clBtnHighlight, clBtnFace, P);
      end;
    end;
  finally
    if OldPen <> 0 then SelectObject(DC, OldPen);
    P.Free;
  end;
end;

procedure scdDrawEdge(ACanvas: TCanvas; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges);
begin
  if (ACanvas <> nil) and (B <> scdcbNone) and (Edges <> []) then
    scdDrawEdgeEx(ACanvas.Handle, R, C, ParentColor, FillCorner, B, Edges);
end;

procedure scdDrawBevel(ACanvas: TCanvas; R: TRect; C, ParentColor: TColor;
  FillCorner: Boolean; B: TSCDeBorder; Edges: TSCDeBorderEdges);
begin
  if (ACanvas <> nil) and (B <> scdcbNone) and (Edges <> []) then
    scdDrawBevelEx(ACanvas.Handle, R, C, ParentColor, FillCorner, B, Edges);
end;

procedure scdHSVtoRGB(const H, S, V: Integer; var R, G, B: Byte);
const
  divisor: Integer = 255*60;
var
  f, hTemp,
  p, q, t, VS: Integer;
begin
  if S = 0 then  // achromatic:  shades of gray
  begin
    R := V;
    G := V;
    B := V;
  end else
  begin   // chromatic color
    if H = 360 then hTemp := 0
    else hTemp := H;

    f     := hTemp mod 60; // f is IN [0, 59]
    hTemp := hTemp div 60; // h is now IN [0..6]

    VS := V*S;
    p := V - VS div 255;                 // p = v * (1 - s)
    q := V - (VS*f) div divisor;         // q = v * (1 - s*f)
    t := V - (VS*(60 - f)) div divisor;  // t = v * (1 - s * (1 - f))

    case hTemp of
      0:
      begin
        R := V;
        G := t;
        B := p;
      end;
      1:
      begin
        R := q;
        G := V;
        B := p;
      end;
      2:
      begin
        R := p;
        G := V;
        B := t;
      end;
      3:
      begin
        R := p;
        G := q;
        B := V;
      end;
      4:
      begin
        R := t;
        G := p;
        B := V;
      end;
      5:
      begin
        R := V;
        G := p;
        B := q;
      end;
      else begin // should never happen; avoid compiler warning
        R := 0;
        G := 0;
        B := 0;
      end;
    end;
  end;
end;

procedure scRGBToHSV(const R, G, B: Byte; var H, S, V: Integer);
var
  Delta, Min: Integer;
begin
  Min := MinIntValue([R, G, B]);
  V   := MaxIntValue([R, G, B]);

  Delta := V - Min;

  // Calculate saturation: saturation is 0 if r, g and b are all 0
  if V = 0 then S := 0
  else S := MulDiv(Delta, 255, V);

  if S = 0 then
    H := 0 // Achromatic:  When s = 0, h is undefined but assigned the value 0
  else begin // Chromatic
    if R = V then  // degrees -- between yellow and magenta
      H := MulDiv(G - B, 60, Delta)
    else if G = V then // between cyan and yellow
      H := 120 + MulDiv(B - R, 60, Delta)
    else if B = V then // between magenta and cyan
      H := 240 + MulDiv(R - G, 60, Delta);

    if H < 0 then H := H + 360;
  end;
end;

procedure scdHSLtoRGB(const H, S, L: Word; var R, G, B: Byte);
var
  HR, SR, LR,
  RR, GR, BR,
  Val, M, SV,
  Fract, VSF, Mid1, Mid2: Real;
  HI: Integer;
begin
  HR := H / 239;
  SR := S / 240;
  LR := L / 240;

  if LR < 0.5 then
    Val := LR * (1.0 + SR)
  else
    Val := LR + SR - LR * SR;

  if Val <= 0 then
  begin
    RR := 0.0;
    GR := 0.0;
    BR := 0.0;
  end else
  begin
    M := (2 * LR) - Val;
    SV := (Val - M) / Val;

    HR := HR * 6.0;
    HI := Trunc(HR);
    Fract := HR - HI;

    VSF := Val * SV * Fract;
    Mid1 := M + VSF;
    Mid2 := Val - VSF;

    case HI of
      0: begin
        RR := Val;
        GR := Mid1;
        BR := M;
      end;
      1: begin
        RR := Mid2;
        GR := Val;
        BR := M;
      end;
      2: begin
        RR := M;
        GR := Val;
        BR := Mid1;
      end;
      3: begin
        RR := M;
        GR := Mid2;
        BR := Val;
      end;
      4: begin
        RR := Mid1;
        GR := M;
        BR := Val;
      end;
      5: begin
        RR := Val;
        GR := M;
        BR := Mid2;
      end;
      else begin
        RR := Val;
        GR := Mid1;
        BR := M;
      end;
    end;
  end;

  if RR > 1.0 then RR := 1.0;
  if GR > 1.0 then GR := 1.0;
  if BR > 1.0 then BR := 1.0;

  R := Round(RR * 255);
  G := Round(GR * 255);
  B := Round(BR * 255);
end;

procedure scRGBToHSL(const R, G, B: Byte; var H, S, L: Word);
var
  MnMxDif, Cmax, Cmin,
  RR, GR, BR,
  TmpH, TmpS, TmpL: Real;
begin
  RR := GetRValue(R) / 255;
  GR := GetGValue(G) / 255;
  BR := GetBValue(B) / 255;

  CMax := MaxValue([RR, GR, BR]);
  CMin := MinValue([RR, GR, BR]);

  TmpL := (Cmax + Cmin) / 2.0;

  if Cmax = Cmin then
  begin
    TmpS := 0;
    TmpH := 0;
  end else
  begin
    MnMxDif := Cmax - Cmin;

    if TmpL < 0.5 then
      TmpS := MnMxDif / (Cmax + Cmin)
    else
      TmpS := MnMxDif / (2.0 - (Cmax + Cmin));

    if RR = Cmax then
      TmpH := (GR - BR) / MnMxDif
    else if GR = Cmax then
      TmpH := 2.0 + (BR - RR) / MnMxDif
    else
      TmpH := 4.0 + (RR - GR) / MnMxDif;

    TmpH := TmpH / 6;
    if TmpH < 0 then TmpH := TmpH + 1;
  end;

  H := Round(240 * TmpH);
  H := H mod 240;

  S := Round(240 * TmpS);
  L := Round(240 * TmpL);
end;

procedure scRGBToCMY(const R, G, B: Byte; var C, M, Y: Byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
end;

procedure scdHSBtoRGB(const H, S, Br: Integer; var R, G, B: Byte);
var
  Sat, Val: Integer;
begin
  Sat := Muldiv(S, 255, 100);
  Val := Muldiv(Br, 255, 100);

  scdHSVtoRGB(H, Sat, Val, R, G, B);
end;

procedure scRGBToHSB(const R, G, B: Byte; var H, S, Br: Integer);
begin
  scRGBToHSV(R, G, B, H, S, Br);

  S  := Muldiv(S, 100, 255);
  Br := Muldiv(Br, 100, 255);
end;

procedure scRGBToCMYK(const R, G, B: Byte; var C, M, Y, K: Byte);
begin
  scRGBToCMY(R, G, B, C, M, Y);
  K := MinIntValue([C, M, Y]);

  C := C - K;
  M := M - K;
  Y := Y - K;
end;

procedure scdCMYKToRGB(C, M, Y, K: Byte; var R, G, B : Byte);
begin
  if (Integer(C) + Integer(K)) < 255 then R := 255 - (C + K) else R := 0;
  if (Integer(M) + Integer(K)) < 255 then G := 255 - (M + K) else G := 0;
  if (Integer(Y) + Integer(K)) < 255 then B := 255 - (Y + K) else B := 0;
end;

procedure scdColorToHSV(const Color: TColor; var H, S, V: Integer);
var
  C: LongInt;
begin
  C := ColorToRGB(Color);
  scRGBToHSV(GetRValue(C), GetGValue(C), GetBValue(C), H, S, V);
end;

function scdHSVtoColor(const H, S, V: Integer): TColor;
var
  R, G, B: Byte;
begin
  scdHSVtoRGB(H, S, V, R, G, B);
  Result := RGB(R, G, B);
end;

function scdHSLtoColor(const H, S, L: Word): TColor;
var
  R, G, B: Byte;
begin
  scdHSLtoRGB(H, S, L, R, G, B);
  Result := RGB(R, G, B);
end;

procedure scdColorToHSL(const Color: TColor; var H, S, L: Word);
var
  C: LongInt;
begin
  C := ColorToRGB(Color);
  scRGBToHSL(GetRValue(C), GetGValue(C), GetBValue(C), H, S, L);
end;

function scdHSBtoColor(const H, S, Br: Integer): TColor;
var
  R, G, B: Byte;
begin
  scdHSBtoRGB(H, S, Br, R, G, B);
  Result := RGB(R, G, B);
end;

procedure scdColorToHSB(const Color: TColor; var H, S, Br: Integer);
var
  C: LongInt;
begin
  C := ColorToRGB(Color);
  scRGBToHSB(GetRValue(C), GetGValue(C), GetBValue(C), H, S, Br);
end;

function scdCMYKToColor(const C, M, Y, K: Byte): TColor;
var
  R, G, B: Byte;
begin
  scdCMYKToRGB(C, M, Y, K, R, G, B);
  Result := RGB(R, G, B);
end;

procedure scdColorToCMYK(const Color: TColor; var C, M, Y, K: Byte);
var
  RGBVal: LongInt;
begin
  RGBVal := ColorToRGB(Color);
  scRGBToCMYK(GetRValue(RGBVal), GetGValue(RGBVal), GetBValue(RGBVal),
    C, M, Y, K);
end;

procedure scdDrawDownSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor; FaceColor: TColor);
var
  R, R2: TRect;
  Pts1: array[0..4] of TPoint;
  Pts2: array[0..2] of TPoint;
  Pts3: array[0..3] of TPoint;
  Pts6: array[0..6] of TPoint;
  Indent, Tail: Integer;
begin
  if (H > 0) and (W > 0) and (ACanvas <> nil) then
  begin
    case Style of
      sctsNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        if FrameColor = clNone then
          FrameColor := clHighlight;

        W := W div 2;

        if H < W then H := W;

        if Blind then
        begin
          with ACanvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := FaceColor;
            Pen.Mode    := pmCopy;
            Pen.Style   := psSolid;

            R := Rect(P.x - W, P.y, P.x + W, P.y + H);
            InflateRect(R, 0, -1);
            FillRect(R);

            InflateRect(R, 0, 1);

            Pen.Color   := scdGetBtnShadowOf(FaceColor);
            MoveTo(R.Left + 1,  R.Top);
            LineTo(R.Right, R.Top);

            MoveTo(R.Left, R.Top + 1);
            LineTo(R.Left, R.Bottom - 1);


            Pen.Color := scdGet3DDkShadowOf(FaceColor);
            MoveTo(R.Right, R.Top + 1);
            LineTo(R.Right, R.Bottom - 1);

            MoveTo(R.Left + 1,  R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);


            InflateRect(R, -1, -1);

            R2 := R;
            Inc(R2.Right);
            scdFrame3D(ACanvas, R2, scdGetBtnHighlightOf(FaceColor),
              scdGetBtnShadowOf(FaceColor), 1, 0);

            Dec(R2.Right);
            Pen.Color := scdBlendedColor(FaceColor, 48, 48, 48, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);


            R2 := R;
            R2.Bottom := R2.Top + 3;

            Brush.Color := scdBlendedColor(FrameColor, 24, 24, 24, True);
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left, R2.Top);
            LineTo(R2.Left, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);

            R2 := R;
            R2.Top := R2.Bottom - 3;

            Brush.Color := FrameColor;
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left, R2.Top);
            LineTo(R2.Left, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);
          end;

          Exit;
        end;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y + H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y + H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y + 1;
          Pts3[1].x := P.x + W - 1;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + W - 1;
          Pts3[2].y := P.y + H - 1;
          Pts3[3].x := P.x - W;
          Pts3[3].y := P.y + H - 1;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y + 3;
          Pts2[1].x := P.x - W + 1;
          Pts2[1].y := P.y + W + 2;
          Pts2[2].x := P.x - W + 1;
          Pts2[2].y := P.y + H - 1;
          PolyLine(Pts2);

          // Pen.Color   := scdBlendedColor(clBtnFace, 48, 48, 48, False);
          Pen.Color   := scdBlendedColor(FaceColor, 48, 48, 48, False);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y + 2;
          Pts2[1].x := P.x + W - 2;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x + W - 2;
          Pts2[2].y := P.y + H - 1;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W + 1;
          Pts2[0].y := P.y + W;
          Pts2[1].x := P.x;
          Pts2[1].y := P.y + 1;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y + W + 1;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W + 1;
          Pts2[0].y := P.y + W + 1;
          Pts2[1].x := P.x;
          Pts2[1].y := P.y + 2;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y + W + 2;
          PolyLine(Pts2);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - W + 1, P.y + W);
          LineTo(P.x + 1, P.y);

          Pen.Color   := FrameColor;
          MoveTo(P.x + W - 1, P.y + H - 3);
          LineTo(P.x - W,     P.y + H - 3);
          MoveTo(P.x + W - 1, P.y + H - 2);
          LineTo(P.x - W,     P.y + H - 2);
          MoveTo(P.x + W - 1, P.y + H - 1);
          LineTo(P.x - W,     P.y + H - 1);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - W + 1, P.y + H - 3);
          LineTo(P.x - W + 1, P.y + H);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, False);
          MoveTo(P.x + W - 1, P.y + H - 3);
          LineTo(P.x + W - 1, P.y + H);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + W;
          Pts3[2].y := P.y + H;
          Pts3[3].x := P.x - W - 1;
          Pts3[3].y := P.y + H;
          PolyLine(Pts3);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x - W, P.y + H);
          LineTo(P.x - W + 1, P.y + H);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x + W, P.y + H);
          LineTo(P.x + W - 1, P.y + H);
        end;
      end;
      sctsWindows:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnFace;
          Pen.Color   := FaceColor;

          if Blind then
          begin
            R := Rect(P.x - W, P.y, P.x + W + 1, P.y + H);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor),
              scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, FaceColor, scdGetBtnShadowOf(FaceColor), 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y + H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y + H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);

          Pts3[0].x := P.x;
          Pts3[0].y := P.y + 1;
          Pts3[1].x := P.x + W - 1;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + W - 1;
          Pts3[2].y := P.y + H - 1;
          Pts3[3].x := P.x - W;
          Pts3[3].y := P.y + H - 1;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x - W;
          Pts2[2].y := P.y + H;
          PolyLine(Pts2);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + W;
          Pts3[2].y := P.y + H;
          Pts3[3].x := P.x - W - 1;
          Pts3[3].y := P.y + H;
          PolyLine(Pts3);
        end;
      end;
      sctsMetal:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := FaceColor;

          if Blind then
          begin
            R := Rect(P.x - W, P.y, P.x + W + 1, P.y + H);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnShadowOf(FaceColor), scdGetBtnShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, SCDECommon.scdBlendColor(FaceColor, 64), FaceColor, 1, 0);

            Exit;
          end;

          Pen.Color := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y + H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y + H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := SCDECommon.scdBlendColor(FaceColor, 64);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y + 1;
          Pts2[1].x := P.x - W + 1;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x - W + 1;
          Pts2[2].y := P.y + H - 1;
          PolyLine(Pts2);
        end;
      end;
      sctsPSNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := scdGet3DDkShadowOf(FaceColor);

          if Blind then
          begin
            R := Rect(P.x - W, P.y, P.x + W + 1, P.y + H);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGet3DDkShadowOf(FaceColor), scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor), FaceColor, 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y + H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y + H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y + 1;
          Pts2[1].x := P.x - W + 1;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x - W + 1;
          Pts2[2].y := P.y + H;
          PolyLine(Pts2);

          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + W;
          Pts3[2].y := P.y + H;
          Pts3[3].x := P.x - W - 1;
          Pts3[3].y := P.y + H;
          PolyLine(Pts3);
        end;
      end;
      sctsMac, sctsPS:
      begin
        if FaceColor = clNone then
        begin
          if Style = sctsPS then
            FaceColor := clBtnFace
          else FaceColor := clWindowFrame;
        end;

        if (Style = sctsPS) and (FrameColor = clNone) then
          FrameColor := clWindowFrame;

        W := W div 2;

        Dec(P.x);
        Pts2[0].x := P.x;
        Pts2[0].y := P.y;
        Pts2[1].x := P.x + W;
        Pts2[1].y := P.y + H;
        Pts2[2].x := P.x - W;
        Pts2[2].y := P.y + H;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          
          if Style = sctsPS then
            Pen.Color := FrameColor
          else Pen.Color := FaceColor;

          Polygon(Pts2);
        end;
      end;
      sctsArrow, sctsExArrow:
      begin
        if FaceColor = clNone then
          FaceColor := clWindowFrame;

        if FrameColor = clNone then
          FrameColor := clWindowFrame;

        W := W div 2;
        Indent := W div 2;

        if H < W + 2 then H := W + 2;

        Tail := 0;
        if Style = sctsExArrow then
        begin
          if H < W + 4 then H := W + 4;

          Tail := H - (W + 3);
          H := W + 2;
        end;

        Dec(P.x);
        Pts6[0].x := P.x;
        Pts6[0].y := P.y;
        Pts6[1].x := P.x + W;
        Pts6[1].y := P.y + W;
        Pts6[2].x := P.x + Indent;
        Pts6[2].y := P.y + W;
        Pts6[3].x := P.x + Indent;
        Pts6[3].y := P.y + H;
        Pts6[4].x := P.x - Indent;
        Pts6[4].y := P.y + H;
        Pts6[5].x := P.x - Indent;
        Pts6[5].y := P.y + W;
        Pts6[6].x := P.x - W;
        Pts6[6].y := P.y + W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := FrameColor;

          Polygon(Pts6);

          if Style = sctsExArrow then
          begin
            Inc(H, 2);

            Pts3[0].x := P.x - Indent;
            Pts3[0].y := P.y + H;
            Pts3[1].x := P.x + Indent;
            Pts3[1].y := P.y + H;
            Pts3[2].x := P.x + Indent;
            Pts3[2].y := P.y + H + Tail;
            Pts3[3].x := P.x - Indent;
            Pts3[3].y := P.y + H + Tail;

            Polygon(Pts3);
          end;
        end;
      end;
    end;
  end;
end;

procedure scdDrawUpSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor; FaceColor: TColor);
var
  R, R2: TRect;
  Pts1: array[0..4] of TPoint;
  Pts2: array[0..2] of TPoint;
  Pts3: array[0..3] of TPoint;
  Pts6: array[0..6] of TPoint;
  Indent, Tail: Integer;
begin
  if (H > 0) and (W > 0) and (ACanvas <> nil) then
  begin
    case Style of
      sctsNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        if FrameColor = clNone then
          FrameColor := clHighlight;

        W := W div 2;

        if H < W then H := W;

        if Blind then
        begin
          with ACanvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := FaceColor;
            Pen.Mode    := pmCopy;
            Pen.Style   := psSolid;
            
            R := Rect(P.x - W, P.y - H, P.x + W, P.y);
            InflateRect(R, 0, -1);
            FillRect(R);

            InflateRect(R, 0, 1);

            Pen.Color   := scdGetBtnShadowOf(FaceColor);
            MoveTo(R.Left + 1,  R.Top);
            LineTo(R.Right, R.Top);

            MoveTo(R.Left, R.Top + 1);
            LineTo(R.Left, R.Bottom - 1);


            Pen.Color := scdGet3DDkShadowOf(FaceColor);
            MoveTo(R.Right, R.Top + 1);
            LineTo(R.Right, R.Bottom - 1);

            MoveTo(R.Left + 1,  R.Bottom - 1);
            LineTo(R.Right, R.Bottom - 1);


            InflateRect(R, -1, -1);

            R2 := R;
            Inc(R2.Right);
            scdFrame3D(ACanvas, R2, scdGetBtnHighlightOf(FaceColor),
              scdGetBtnShadowOf(FaceColor), 1, 0);

            Dec(R2.Right);
            Pen.Color := scdBlendedColor(FaceColor, 48, 48, 48, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);

            
            R2 := R;
            R2.Bottom := R2.Top + 3;

            Brush.Color := scdBlendedColor(FrameColor, 24, 24, 24, True);
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left, R2.Top);
            LineTo(R2.Left, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);

            R2 := R;
            R2.Top := R2.Bottom - 3;

            Brush.Color := FrameColor;
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left, R2.Top);
            LineTo(R2.Left, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Right, R2.Top);
            LineTo(R2.Right, R2.Bottom);
          end;

          Exit;
        end;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          // Brush.Color := clBtnFace;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnShadow;
          Pen.Color   := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y - H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y - H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color   := scdGetBtnShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y - 1;
          Pts3[1].x := P.x + W - 1;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x + W - 1;
          Pts3[2].y := P.y - H + 1;
          Pts3[3].x := P.x - W;
          Pts3[3].y := P.y - H + 1;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color   := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y - 3;
          Pts2[1].x := P.x - W + 1;
          Pts2[1].y := P.y - W - 2;
          Pts2[2].x := P.x - W + 1;
          Pts2[2].y := P.y - H + 1;
          PolyLine(Pts2);

          // Pen.Color   := scdBlendedColor(clBtnFace, 48, 48, 48, False);
          Pen.Color   := scdBlendedColor(FaceColor, 48, 48, 48, False);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y - 2;
          Pts2[1].x := P.x + W - 2;
          Pts2[1].y := P.y - W;
          Pts2[2].x := P.x + W - 2;
          Pts2[2].y := P.y - H + 1;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W + 1;
          Pts2[0].y := P.y - W;
          Pts2[1].x := P.x;
          Pts2[1].y := P.y - 1;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y - W - 1;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W + 1;
          Pts2[0].y := P.y - W - 1;
          Pts2[1].x := P.x;
          Pts2[1].y := P.y - 2;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y - W - 2;
          PolyLine(Pts2);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - W + 1, P.y - W);
          LineTo(P.x + 1, P.y);

          Pen.Color   := FrameColor;
          MoveTo(P.x + W - 1, P.y - H + 3);
          LineTo(P.x - W,     P.y - H + 3);
          MoveTo(P.x + W - 1, P.y - H + 2);
          LineTo(P.x - W,     P.y - H + 2);
          MoveTo(P.x + W - 1, P.y - H + 1);
          LineTo(P.x - W,     P.y - H + 1);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - W + 1, P.y - H + 3);
          LineTo(P.x - W + 1, P.y - H);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, False);
          MoveTo(P.x + W - 1, P.y - H + 3);
          LineTo(P.x + W - 1, P.y - H);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color   := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x + W;
          Pts3[2].y := P.y - H;
          Pts3[3].x := P.x - W - 1;
          Pts3[3].y := P.y - H;
          PolyLine(Pts3);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x - W, P.y - H);
          LineTo(P.x - W + 1, P.y - H);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x + W, P.y - H);
          LineTo(P.x + W - 1, P.y - H);
        end;
      end;
      sctsWindows:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnFace;
          Pen.Color   := FaceColor;

          if Blind then
          begin
            R := Rect(P.x - W, P.y - H, P.x + W + 1, P.y);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor),
              scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, FaceColor, scdGetBtnShadowOf(FaceColor), 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y - H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y - H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x - W;
          Pts3[2].y := P.y - H;
          Pts3[3].x := P.x + W;
          Pts3[3].y := P.y - H;
          PolyLine(Pts3);

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y - 1;
          Pts2[1].x := P.x + W - 1;
          Pts2[1].y := P.y - W;
          Pts2[2].x := P.x + W - 1;
          Pts2[2].y := P.y - H;
          PolyLine(Pts2);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y - W;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y - H - 1;
          PolyLine(Pts2);
        end;
      end;
      sctsMetal:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;

          if Blind then
          begin
            R := Rect(P.x - W, P.y - H, P.x + W + 1, P.y);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnShadowOf(FaceColor), scdGetBtnShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, SCDECommon.scdBlendColor(FaceColor, 64), FaceColor, 1, 0);

            Exit;
          end;

          Pen.Color := scdGetBtnShadowOf(FaceColor);
          
          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y - H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y - H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := SCDECommon.scdBlendColor(FaceColor, 64);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y - 1;
          Pts3[1].x := P.x - W + 1;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x - W + 1;
          Pts3[2].y := P.y - H + 1;
          Pts3[3].x := P.x + W - 1;
          Pts3[3].y := P.y - H + 1;
          PolyLine(Pts3);
        end;
      end;
      sctsPSNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := scdGet3DDkShadowOf(FaceColor);

          if Blind then
          begin
            R := Rect(P.x - W, P.y - H, P.x + W + 1, P.y);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGet3DDkShadowOf(FaceColor), scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor), FaceColor, 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + W;
          Pts1[2].y := P.y - H;
          Pts1[3].x := P.x - W;
          Pts1[3].y := P.y - H;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y - 1;
          Pts3[1].x := P.x - W + 1;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x - W + 1;
          Pts3[2].y := P.y - H + 1;
          Pts3[3].x := P.x + W;
          Pts3[3].y := P.y - H + 1;
          PolyLine(Pts3);

          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y - W;
          Pts2[2].x := P.x + W;
          Pts2[2].y := P.y - H - 1;
          PolyLine(Pts2);
        end;
      end;
      sctsMac, sctsPS:
      begin
        if FaceColor = clNone then
        begin
          if Style = sctsPS then
            FaceColor := clBtnFace
          else FaceColor := clWindowFrame;
        end;

        if (Style = sctsPS) and (FrameColor = clNone) then
          FrameColor := clWindowFrame;

        W := W div 2;

        Dec(P.x);
        Pts2[0].x := P.x;
        Pts2[0].y := P.y;
        Pts2[1].x := P.x + W;
        Pts2[1].y := P.y - H;
        Pts2[2].x := P.x - W;
        Pts2[2].y := P.y - H;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          
          if Style = sctsPS then
            Pen.Color := FrameColor
          else Pen.Color := FaceColor;

          Polygon(Pts2);
        end;
      end;
      sctsArrow, sctsExArrow:
      begin
        if FaceColor = clNone then
          FaceColor := clWindowFrame;

        if FrameColor = clNone then
          FrameColor := clWindowFrame;

        W := W div 2;
        Indent := W div 2;

        if H < W + 2 then H := W + 2;

        Tail := 0;
        if Style = sctsExArrow then
        begin
          if H < W + 4 then H := W + 4;

          Tail := H - (W + 3);
          H := W + 2;
        end;

        Dec(P.x);
        Pts6[0].x := P.x;
        Pts6[0].y := P.y;
        Pts6[1].x := P.x + W;
        Pts6[1].y := P.y - W;
        Pts6[2].x := P.x + Indent;
        Pts6[2].y := P.y - W;
        Pts6[3].x := P.x + Indent;
        Pts6[3].y := P.y - H;
        Pts6[4].x := P.x - Indent;
        Pts6[4].y := P.y - H;
        Pts6[5].x := P.x - Indent;
        Pts6[5].y := P.y - W;
        Pts6[6].x := P.x - W;
        Pts6[6].y := P.y - W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := FrameColor;

          Polygon(Pts6);

          if Style = sctsExArrow then
          begin
            Inc(H, 2);

            Pts3[0].x := P.x - Indent;
            Pts3[0].y := P.y - H;
            Pts3[1].x := P.x + Indent;
            Pts3[1].y := P.y - H;
            Pts3[2].x := P.x + Indent;
            Pts3[2].y := P.y - (H + Tail);
            Pts3[3].x := P.x - Indent;
            Pts3[3].y := P.y - (H + Tail);

            Polygon(Pts3);
          end;
        end;
      end;
    end;
  end;
end;

procedure scdDrawLeftSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor; FaceColor: TColor);
var
  R, R2: TRect;
  Pts1: array[0..4] of TPoint;
  Pts2: array[0..2] of TPoint;
  Pts3: array[0..3] of TPoint;
  Pts6: array[0..6] of TPoint;
  Indent, Tail: Integer;
begin
  if (H > 0) and (W > 0) and (ACanvas <> nil) then
  begin
    case Style of
      sctsNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        if FrameColor = clNone then
          FrameColor := clHighlight;

        W := W div 2;

        if H < W then H := W;

        if Blind then
        begin
          with ACanvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := FaceColor;
            Pen.Mode    := pmCopy;
            Pen.Style   := psSolid;

            R := Rect(P.x - H, P.y - W, P.x, P.y + W);
            Dec(R.Right);
            InflateRect(R, 0, -1);

            FillRect(R);

            InflateRect(R, 0, 1);

            Pen.Color   := scdGetBtnShadowOf(FaceColor);
            MoveTo(R.Left + 1,  R.Top);
            LineTo(R.Right, R.Top);

            MoveTo(R.Left, R.Top + 1);
            LineTo(R.Left, R.Bottom);


            Pen.Color := scdGet3DDkShadowOf(FaceColor);
            MoveTo(R.Right, R.Top + 1);
            LineTo(R.Right, R.Bottom);

            MoveTo(R.Left + 1,  R.Bottom);
            LineTo(R.Right, R.Bottom);

            
            InflateRect(R, -1, -1);
            Inc(R.Right);

            R2 := R;
            Inc(R2.Bottom);
            scdFrame3D(ACanvas, R2, scdGetBtnHighlightOf(FaceColor),
              scdGetBtnShadowOf(FaceColor), 1, 0);

            Dec(R2.Bottom);
            Pen.Color := scdBlendedColor(FaceColor, 48, 48, 48, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            R2 := R;
            R2.Right := R2.Left + 3;

            Brush.Color := scdBlendedColor(FrameColor, 24, 24, 24, True);
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left,  R2.Top);
            LineTo(R2.Right, R2.Top);

            R2 := R;
            R2.Left := R2.Right - 3;

            Brush.Color := FrameColor;
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left,  R2.Top);
            LineTo(R2.Right, R2.Top);
          end;

          Exit;
        end;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          // Brush.Color := clBtnFace;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnShadow;
          Pen.Color   := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x - W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x - H;
          Pts1[2].y := P.y + W;
          Pts1[3].x := P.x - H;
          Pts1[3].y := P.y - W;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts3[0].x := P.x - 1;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y + W - 1;
          Pts3[2].x := P.x - H + 1;
          Pts3[2].y := P.y + W - 1;
          Pts3[3].x := P.x - H + 1;
          Pts3[3].y := P.y - W;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x - 3;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W - 2;
          Pts2[1].y := P.y - W + 1;
          Pts2[2].x := P.x - H + 1;
          Pts2[2].y := P.y - W + 1;
          PolyLine(Pts2);

          // Pen.Color   := scdBlendedColor(clBtnFace, 48, 48, 48, False);
          Pen.Color := scdBlendedColor(FaceColor, 48, 48, 48, False);
          Pts2[0].x := P.x - 2;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W;
          Pts2[1].y := P.y + W - 2;
          Pts2[2].x := P.x - H + 1;
          Pts2[2].y := P.y + W - 2;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W;
          Pts2[0].y := P.y - W + 1;
          Pts2[1].x := P.x - 1;
          Pts2[1].y := P.y;
          Pts2[2].x := P.x - W - 1;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x - W - 1;
          Pts2[0].y := P.y - W + 1;
          Pts2[1].x := P.x - 2;
          Pts2[1].y := P.y;
          Pts2[2].x := P.x - W - 2;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - W, P.y - W + 1);
          LineTo(P.x,     P.y + 1);

          Pen.Color   := FrameColor;
          MoveTo(P.x - H + 3, P.y + W - 1);
          LineTo(P.x - H + 3, P.y - W);
          MoveTo(P.x - H + 2, P.y + W - 1);
          LineTo(P.x - H + 2, P.y - W);
          MoveTo(P.x - H + 1, P.y + W - 1);
          LineTo(P.x - H + 1, P.y - W);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x - H + 3, P.y - W + 1);
          LineTo(P.x - H,     P.y - W + 1);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, False);
          MoveTo(P.x - H + 3, P.y + W - 1);
          LineTo(P.x - H,     P.y + W - 1);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x - H;
          Pts3[2].y := P.y + W;
          Pts3[3].x := P.x - H;
          Pts3[3].y := P.y - W - 1;
          PolyLine(Pts3);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x - H, P.y - W);
          LineTo(P.x - H, P.y - W + 1);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x - H, P.y + W);
          LineTo(P.x - H, P.y + W - 1);
        end;
      end;
      sctsWindows:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnFace;
          Pen.Color   := FaceColor;

          if Blind then
          begin
            R := Rect(P.x - H, P.y - W, P.x, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor),
              scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, FaceColor, scdGetBtnShadowOf(FaceColor), 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x - W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x - H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x - H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y - W;
          Pts3[2].x := P.x - H;
          Pts3[2].y := P.y - W;
          Pts3[3].x := P.x - H;
          Pts3[3].y := P.y + W;
          PolyLine(Pts3);

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts2[0].x := P.x - 1;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W;
          Pts2[1].y := P.y + W - 1;
          Pts2[2].x := P.x - H;
          Pts2[2].y := P.y + W - 1;
          PolyLine(Pts2);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x - H - 1;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);
        end;
      end;
      sctsMetal:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;

          if Blind then
          begin
            R := Rect(P.x - H, P.y - W, P.x, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnShadowOf(FaceColor), scdGetBtnShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, SCDECommon.scdBlendColor(FaceColor, 64), FaceColor, 1, 0);

            Exit;
          end;

          Pen.Color := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x - W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x - H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x - H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := SCDECommon.scdBlendColor(FaceColor, 64);
          Pts3[0].x := P.x - 1;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y - W + 1;
          Pts3[2].x := P.x - H + 1;
          Pts3[2].y := P.y - W + 1;
          Pts3[3].x := P.x - H + 1;
          Pts3[3].y := P.y + W - 1;
          PolyLine(Pts3);
        end;
      end;
      sctsPSNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := scdGet3DDkShadowOf(FaceColor);

          if Blind then
          begin
            R := Rect(P.x - H, P.y - W, P.x, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGet3DDkShadowOf(FaceColor), scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor), FaceColor, 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x - W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x - H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x - H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x - W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts3[0].x := P.x - 1;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x - W;
          Pts3[1].y := P.y - W + 1;
          Pts3[2].x := P.x - H + 1;
          Pts3[2].y := P.y - W + 1;
          Pts3[3].x := P.x - H + 1;
          Pts3[3].y := P.y + W;
          PolyLine(Pts3);

          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x - W;
          Pts2[1].y := P.y + W;
          Pts2[2].x := P.x - H - 1;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);
        end;
      end;
      sctsMac, sctsPS:
      begin
        if FaceColor = clNone then
        begin
          if Style = sctsPS then
            FaceColor := clBtnFace
          else FaceColor := clWindowFrame;
        end;

        if (Style = sctsPS) and (FrameColor = clNone) then
          FrameColor := clWindowFrame;

        W := W div 2;

        Dec(P.y);
        Pts2[0].x := P.x;
        Pts2[0].y := P.y;
        Pts2[1].x := P.x - H;
        Pts2[1].y := P.y + W;
        Pts2[2].x := P.x - H;
        Pts2[2].y := P.y - W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;

          if Style = sctsPS then
            Pen.Color := FrameColor
          else Pen.Color := FaceColor;

          Polygon(Pts2);
        end;
      end;
      sctsArrow, sctsExArrow:
      begin
        if FaceColor = clNone then
          FaceColor := clWindowFrame;

        if FrameColor = clNone then
          FrameColor := clWindowFrame;

        W := W div 2;
        Indent := W div 2;

        if H < W + 2 then H := W + 2;

        Tail := 0;
        if Style = sctsExArrow then
        begin
          if H < W + 4 then H := W + 4;

          Tail := H - (W + 3);
          H := W + 2;
        end;

        Dec(P.y);
        Pts6[0].x := P.x;
        Pts6[0].y := P.y;
        Pts6[1].x := P.x - W;
        Pts6[1].y := P.y + W;
        Pts6[2].x := P.x - W;
        Pts6[2].y := P.y + Indent;
        Pts6[3].x := P.x - H;
        Pts6[3].y := P.y + Indent;
        Pts6[4].x := P.x - H;
        Pts6[4].y := P.y - Indent;
        Pts6[5].x := P.x - W;
        Pts6[5].y := P.y - Indent;
        Pts6[6].x := P.x - W;
        Pts6[6].y := P.y - W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := FrameColor;

          Polygon(Pts6);

          if Style = sctsExArrow then
          begin
            Inc(H, 2);

            Pts3[0].x := P.x - H;
            Pts3[0].y := P.y - Indent;
            Pts3[1].x := P.x - H;
            Pts3[1].y := P.y + Indent;
            Pts3[2].x := P.x - (H + Tail);
            Pts3[2].y := P.y + Indent;
            Pts3[3].x := P.x - (H + Tail);
            Pts3[3].y := P.y - Indent;

            Polygon(Pts3);
          end;
        end;
      end;
    end;
  end;
end;

procedure scdDrawRightSlider(ACanvas: TCanvas; P: TPoint; W, H: Integer;
  Style: TSCDeThumbStyle; Blind: Boolean; FrameColor: TColor; FaceColor: TColor);
var
  R, R2: TRect;
  Pts1: array[0..4] of TPoint;
  Pts2: array[0..2] of TPoint;
  Pts3: array[0..3] of TPoint;
  Pts6: array[0..6] of TPoint;
  Indent, Tail: Integer;
begin
  if (H > 0) and (W > 0) and (ACanvas <> nil) then
  begin
    case Style of
      sctsNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        if FrameColor = clNone then
          FrameColor := clHighlight;

        W := W div 2;

        if H < W then H := W;

        if Blind then
        begin
          with ACanvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := FaceColor;
            Pen.Mode    := pmCopy;
            Pen.Style   := psSolid;

            R := Rect(P.x, P.y - W, P.x + H, P.y + W);
            Dec(R.Right);
            InflateRect(R, 0, -1);

            FillRect(R);

            InflateRect(R, 0, 1);

            Pen.Color   := scdGetBtnShadowOf(FaceColor);
            MoveTo(R.Left + 1,  R.Top);
            LineTo(R.Right, R.Top);

            MoveTo(R.Left, R.Top + 1);
            LineTo(R.Left, R.Bottom);


            Pen.Color := scdGet3DDkShadowOf(FaceColor);
            MoveTo(R.Right, R.Top + 1);
            LineTo(R.Right, R.Bottom);

            MoveTo(R.Left + 1,  R.Bottom);
            LineTo(R.Right, R.Bottom);


            InflateRect(R, -1, -1);
            Inc(R.Right);

            R2 := R;
            Inc(R2.Bottom);
            scdFrame3D(ACanvas, R2, scdGetBtnHighlightOf(FaceColor),
              scdGetBtnShadowOf(FaceColor), 1, 0);

            Dec(R2.Bottom);
            Pen.Color := scdBlendedColor(FaceColor, 48, 48, 48, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            R2 := R;
            R2.Right := R2.Left + 3;

            Brush.Color := scdBlendedColor(FrameColor, 24, 24, 24, True);
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left,  R2.Top);
            LineTo(R2.Right, R2.Top);

            R2 := R;
            R2.Left := R2.Right - 3;

            Brush.Color := FrameColor;
            FillRect(R2);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, False);
            MoveTo(R2.Left,  R2.Bottom);
            LineTo(R2.Right, R2.Bottom);

            Pen.Color := scdBlendedColor(FrameColor, 42, 36, 16, True);
            MoveTo(R2.Left,  R2.Top);
            LineTo(R2.Right, R2.Top);
          end;

          Exit;
        end;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          // Brush.Color := clBtnFace;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnShadow;
          Pen.Color   := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y + W;
          Pts1[2].x := P.x + H;
          Pts1[2].y := P.y + W;
          Pts1[3].x := P.x + H;
          Pts1[3].y := P.y - W;
          Pts1[4].x := P.x + W;
          Pts1[4].y := P.y - W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts3[0].x := P.x + 1;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W - 1;
          Pts3[2].x := P.x + H - 1;
          Pts3[2].y := P.y + W - 1;
          Pts3[3].x := P.x + H - 1;
          Pts3[3].y := P.y - W;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x + 3;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W + 2;
          Pts2[1].y := P.y - W + 1;
          Pts2[2].x := P.x + H - 1;
          Pts2[2].y := P.y - W + 1;
          PolyLine(Pts2);

          // Pen.Color   := scdBlendedColor(clBtnFace, 48, 48, 48, False);
          Pen.Color   := scdBlendedColor(FaceColor, 48, 48, 48, False);
          Pts2[0].x := P.x + 2;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y + W - 2;
          Pts2[2].x := P.x + H - 1;
          Pts2[2].y := P.y + W - 2;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x + W;
          Pts2[0].y := P.y - W + 1;
          Pts2[1].x := P.x + 1;
          Pts2[1].y := P.y;
          Pts2[2].x := P.x + W + 1;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);

          Pen.Color   := FrameColor;
          Pts2[0].x := P.x + W + 1;
          Pts2[0].y := P.y - W + 1;
          Pts2[1].x := P.x + 2;
          Pts2[1].y := P.y;
          Pts2[2].x := P.x + W + 2;
          Pts2[2].y := P.y + W;
          PolyLine(Pts2);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x + W, P.y - W + 1);
          LineTo(P.x,     P.y + 1);

          Pen.Color   := FrameColor;
          MoveTo(P.x + H - 3, P.y + W - 1);
          LineTo(P.x + H - 3, P.y - W);
          MoveTo(P.x + H - 2, P.y + W - 1);
          LineTo(P.x + H - 2, P.y - W);
          MoveTo(P.x + H - 1, P.y + W - 1);
          LineTo(P.x + H - 1, P.y - W);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, True);
          MoveTo(P.x + H - 3, P.y - W + 1);
          LineTo(P.x + H,     P.y - W + 1);

          Pen.Color   := scdBlendedColor(FrameColor, 42, 36, 16, False);
          MoveTo(P.x + H - 3, P.y + W - 1);
          LineTo(P.x + H,     P.y + W - 1);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + H;
          Pts3[2].y := P.y + W;
          Pts3[3].x := P.x + H;
          Pts3[3].y := P.y - W - 1;
          PolyLine(Pts3);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x + H, P.y - W);
          LineTo(P.x + H, P.y - W + 1);

          // Pen.Color   := clBtnFace;
          Pen.Color := FaceColor;
          MoveTo(P.x + H, P.y + W);
          LineTo(P.x + H, P.y + W - 1);
        end;
      end;
      sctsWindows:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          // Pen.Color   := clBtnFace;
          Pen.Color   := FaceColor;

          if Blind then
          begin
            R := Rect(P.x, P.y - W, P.x + H, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor),
              scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, FaceColor, scdGetBtnShadowOf(FaceColor), 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x + H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x + W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          // Pen.Color   := clBtnShadow;
          Pen.Color := scdGetBtnShadowOf(FaceColor);
          Pts3[0].x := P.x + 1;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W - 1;
          Pts3[2].x := P.x + H - 1;
          Pts3[2].y := P.y + W - 1;
          Pts3[3].x := P.x + H - 1;
          Pts3[3].y := P.y - W;
          PolyLine(Pts3);

          // Pen.Color   := clBtnHighlight;
          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y - W;
          Pts2[2].x := P.x + H;
          Pts2[2].y := P.y - W;
          PolyLine(Pts2);

          // Pen.Color   := cl3DDkShadow;
          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + H;
          Pts3[2].y := P.y + W;
          Pts3[3].x := P.x + H;
          Pts3[3].y := P.y - W - 1;
          PolyLine(Pts3);
        end;
      end;
      sctsMetal:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;

          if Blind then
          begin
            R := Rect(P.x, P.y - W, P.x + H, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGetBtnShadowOf(FaceColor), scdGetBtnShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, SCDECommon.scdBlendColor(FaceColor, 64), FaceColor, 1, 0);

            Exit;
          end;

          Pen.Color := scdGetBtnShadowOf(FaceColor);

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x + H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x + W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := SCDECommon.scdBlendColor(FaceColor, 64);
          Pts2[0].x := P.x + 1;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y - W + 1;
          Pts2[2].x := P.x + H - 1;
          Pts2[2].y := P.y - W + 1;
          PolyLine(Pts2);
        end;
      end;
      sctsPSNew:
      begin
        if FaceColor = clNone then
          FaceColor := clBtnFace;

        W := W div 2;

        if H < W then H := W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := scdGet3DDkShadowOf(FaceColor);

          if Blind then
          begin
            R := Rect(P.x, P.y - W, P.x + H, P.y + W + 1);

            FillRect(R);

            scdFrame3D(ACanvas, R, scdGet3DDkShadowOf(FaceColor), scdGet3DDkShadowOf(FaceColor), 1, 0);
            scdFrame3D(ACanvas, R, scdGetBtnHighlightOf(FaceColor), FaceColor, 1, 0);

            Exit;
          end;

          Pts1[0].x := P.x;
          Pts1[0].y := P.y;
          Pts1[1].x := P.x + W;
          Pts1[1].y := P.y - W;
          Pts1[2].x := P.x + H;
          Pts1[2].y := P.y - W;
          Pts1[3].x := P.x + H;
          Pts1[3].y := P.y + W;
          Pts1[4].x := P.x + W;
          Pts1[4].y := P.y + W;
          Polygon(Pts1);

          Brush.Style := bsClear;

          Pen.Color := scdGetBtnHighlightOf(FaceColor);
          Pts2[0].x := P.x + 1;
          Pts2[0].y := P.y;
          Pts2[1].x := P.x + W;
          Pts2[1].y := P.y - W + 1;
          Pts2[2].x := P.x + H - 1;
          Pts2[2].y := P.y - W + 1;
          PolyLine(Pts2);

          Pen.Color := scdGet3DDkShadowOf(FaceColor);
          Pts3[0].x := P.x;
          Pts3[0].y := P.y;
          Pts3[1].x := P.x + W;
          Pts3[1].y := P.y + W;
          Pts3[2].x := P.x + H;
          Pts3[2].y := P.y + W;
          Pts3[3].x := P.x + H;
          Pts3[3].y := P.y - W - 1;
          PolyLine(Pts3);
        end;
      end;
      sctsMac, sctsPS:
      begin
        if FaceColor = clNone then
        begin
          if Style = sctsPS then
            FaceColor := clBtnFace
          else FaceColor := clWindowFrame;
        end;

        if (Style = sctsPS) and (FrameColor = clNone) then
          FrameColor := clWindowFrame;

        W := W div 2;

        Dec(P.y);
        Pts2[0].x := P.x;
        Pts2[0].y := P.y;
        Pts2[1].x := P.x + H;
        Pts2[1].y := P.y + W;
        Pts2[2].x := P.x + H;
        Pts2[2].y := P.y - W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          
          if Style = sctsPS then
            Pen.Color := FrameColor
          else Pen.Color := FaceColor;

          Polygon(Pts2);
        end;
      end;
      sctsArrow, sctsExArrow:
      begin
        if FaceColor = clNone then
          FaceColor := clWindowFrame;

        if FrameColor = clNone then
          FrameColor := clWindowFrame;

        W := W div 2;
        Indent := W div 2;

        if H < W + 2 then H := W + 2;

        Tail := 0;
        if Style = sctsExArrow then
        begin
          if H < W + 4 then H := W + 4;

          Tail := H - (W + 3);
          H := W + 2;
        end;

        Dec(P.y);
        Pts6[0].x := P.x;
        Pts6[0].y := P.y;
        Pts6[1].x := P.x + W;
        Pts6[1].y := P.y + W;
        Pts6[2].x := P.x + W;
        Pts6[2].y := P.y + Indent;
        Pts6[3].x := P.x + H;
        Pts6[3].y := P.y + Indent;
        Pts6[4].x := P.x + H;
        Pts6[4].y := P.y - Indent;
        Pts6[5].x := P.x + W;
        Pts6[5].y := P.y - Indent;
        Pts6[6].x := P.x + W;
        Pts6[6].y := P.y - W;

        with ACanvas do
        begin
          Brush.Style := bsSolid;
          Brush.Color := FaceColor;
          Pen.Mode    := pmCopy;
          Pen.Style   := psSolid;
          Pen.Color   := FrameColor;

          Polygon(Pts6);

          if Style = sctsExArrow then
          begin
            Inc(H, 2);

            Pts3[0].x := P.x + H;
            Pts3[0].y := P.y - Indent;
            Pts3[1].x := P.x + H;
            Pts3[1].y := P.y + Indent;
            Pts3[2].x := P.x + H + Tail;
            Pts3[2].y := P.y + Indent;
            Pts3[3].x := P.x + H + Tail;
            Pts3[3].y := P.y - Indent;

            Polygon(Pts3);
          end;
        end;
      end;
    end;
  end;
end;


procedure scdDrawXPButton(C: TCanvas; R: TRect; Cl, BkCl: TColor;
  IsDown, IsHot: Boolean);
var
  X, I: Integer;
  R1: TRect;
  FCl, C1: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  FCl := Cl;
  if IsDown then
    FCl := SCDECommon.scdBlendColor(FCl, -24)
  else
  if IsHot then
    FCl := SCDECommon.scdBlendColor(FCl, 16);

  R1 := R;
  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FCl;
    FillRect(R1);

    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;

    C1 := BkCl;
    if BkCl = clNone then
      C1 := scdGetBtnHighlightOf(Cl);
      
    scdFrame3D(C, R1, C1, C1, 1, 0);

    C1 := SCDECommon.scdBlendColor(Cl, 32);
    scdFrame3D(C, R1, C1, C1, 1, 0);
    InflateRect(R1, 1, 1);

    C1 := SCDECommon.scdBlendColor(Cl, -16);
    scdFrame3D(C, R1, C1, C1, 1, 0);

    if IsDown then
      C1 := SCDECommon.scdBlendColor(FCl, -24)
    else
      C1 := SCDECommon.scdBlendColor(FCl, 16);

    Pen.Color := C1;
    MoveTo(R1.Left,  R1.Top);
    LineTo(R1.Right - 2, R1.Top);

    if IsDown then
    begin
      Pen.Color := SCDECommon.scdBlendColor(C1, 12);

      if R1.Top + 1 < R1.Bottom then
      begin
        MoveTo(R1.Left,  R1.Top + 1);
        LineTo(R1.Right - 2, R1.Top + 1);

        MoveTo(R1.Left,  R1.Top + 2);
        LineTo(R1.Right - 2, R1.Top + 2);
      end;  
    end;

    for I := 0 to 2 do
    begin
      Pen.Color := SCDECommon.scdBlendColor(FCl, -12 + 4*I);

      X := R1.Right - 2*I;
      if X < R1.Left then Break;

      MoveTo(X, R1.Top + 1);
      LineTo(X, R1.Bottom);

      Dec(X);
      if X < R1.Left then Break;

      MoveTo(X, R1.Top + 1);
      LineTo(X, R1.Bottom);
    end;

    if IsDown then
      for I := 0 to 2 do
      begin
        Pen.Color := SCDECommon.scdBlendColor(FCl, -18 + 6*I);

        X := R1.Left + 3*I;
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);

        Inc(X);
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);

        Inc(X);
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);
      end;
  end;

  C1 := scdGetBtnHighlightOf(Cl);
  C1 := SCDECommon.scdBlendColor(C1, -16);

  R1 := R;
  scdFrame3D(C, R1, C1, C1, 1, 0);
end;

procedure scdDrawXPThumb(C: TCanvas; R: TRect; Cl: TColor;
  IsDown, IsHot: Boolean);
var
  X, I: Integer;
  R1: TRect;
  FCl, C1, C2: TColor;
begin
  if (C = nil) or IsRectEmpty(R) then
    Exit;

  FCl := Cl;
  if IsDown then
    FCl := SCDECommon.scdBlendColor(FCl, -24)
  else
  if IsHot then
    FCl := SCDECommon.scdBlendColor(FCl, 16);

  R1 := R;
  with C do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FCl;
    FillRect(R1);

    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;

    C1 := scdGetBtnHighlightOf(Cl);
    scdFrame3D(C, R1, C1, C1, 1, 0);
    scdFrame3D(C, R1, C1, C1, 1, 0);

    C1 := SCDECommon.scdBlendColor(Cl, 32);
    scdFrame3D(C, R1, C1, C1, 1, 0);
    InflateRect(R1, 1, 1);

    C1 := SCDECommon.scdBlendColor(Cl, -16);
    scdFrame3D(C, R1, C1, C1, 1, 2);

    if IsDown then
      C1 := SCDECommon.scdBlendColor(FCl, -24)
    else
      C1 := SCDECommon.scdBlendColor(FCl, 16);

    Pen.Color := C1;
    MoveTo(R1.Left,  R1.Top);
    LineTo(R1.Right - 2, R1.Top);

    if IsDown then
    begin
      Pen.Color := SCDECommon.scdBlendColor(C1, 12);

      if R1.Top + 1 < R1.Bottom then
      begin
        MoveTo(R1.Left,  R1.Top + 1);
        LineTo(R1.Right - 2, R1.Top + 1);

        MoveTo(R1.Left,  R1.Top + 2);
        LineTo(R1.Right - 2, R1.Top + 2);
      end;  
    end;

    for I := 0 to 2 do
    begin
      Pen.Color := SCDECommon.scdBlendColor(FCl, -12 + 4*I);

      X := R1.Right - 2*I;
      if X < R1.Left then Break;

      MoveTo(X, R1.Top + 1);
      LineTo(X, R1.Bottom);

      Dec(X);
      if X < R1.Left then Break;

      MoveTo(X, R1.Top + 1);
      LineTo(X, R1.Bottom);
    end;

    if IsDown then
      for I := 0 to 2 do
      begin
        Pen.Color := SCDECommon.scdBlendColor(FCl, -18 + 6*I);

        X := R1.Left + 3*I;
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);

        Inc(X);
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);

        Inc(X);
        if X > R1.Right then Break;

        MoveTo(X, R1.Top + 1);
        LineTo(X, R1.Bottom);
      end;
  end;

  C1 := scdGetBtnHighlightOf(Cl);
  C1 := SCDECommon.scdBlendColor(C1, -16);

  C2 := scdGetBtnShadowOf(Cl);
  C2 := SCDECommon.scdBlendColor(C2, 16);

  R1 := R;
  scdFrame3D(C, R1, C1, C2, 1, 4);

  with C do
  begin
    Pen.Mode  := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;

    Pen.Color := C1;
    MoveTo(R.Left, R.Top + 1);
    LineTo(R.Left, R.Bottom - 1);

    C1 := scdGetBtnHighlightOf(Cl);
    Pen.Color := C1;

    MoveTo(R.Left,  R.Top);
    LineTo(R.Right, R.Top);

    Pen.Color := C2;
    MoveTo(R.Left + 1, R.Bottom - 2);
    LineTo(R.Left + 3, R.Bottom);

    Pen.Color := C1;
    MoveTo(R.Right - 3, R.Top);
    LineTo(R.Right, R.Top + 3);
  end;
end;


// Shotcut methods

function scdShortCut(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := 0;
  if WordRec(Key).Hi <> 0 then Exit;
  Result := Key;
  if ssShift in Shift then Inc(Result, scShift);
  if ssCtrl in Shift then Inc(Result, scCtrl);
  if ssAlt in Shift then Inc(Result, scAlt);
end;

procedure scdShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  Key := ShortCut and not (scShift + scCtrl + scAlt);
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift, ssShift);
  if ShortCut and scCtrl <> 0 then Include(Shift, ssCtrl);
  if ShortCut and scAlt <> 0 then Include(Shift, ssAlt);
end;

type
  TSCDeKeyCap = (scdkcBkSp, scdkcTab, scdkcEsc, scdkcEnter, scdkcSpace,
    scdkcPgUp, scdkcPgDn, scdkcEnd, scdkcHome, scdkcLeft, scdkcUp,
    scdkcRight, scdkcDown, scdkcIns, scdkcDel, scdkcShift, scdkcCtrl,
    scdkcAlt);

var
  scdKeyCaps: array[TSCDeKeyCap] of string = (
    scdskBkSp, scdskTab, scdskEsc,  scdskEnter, scdskSpace, scdskPgUp,
    scdskPgDn, scdskEnd, scdskHome, scdskLeft,  scdskUp,    scdskRight,
    scdskDown, scdskIns, scdskDel,  scdskShift, scdskCtrl,  scdskAlt);

function scdGetSpecialKeyName(ShortCut: TShortCut): string;
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
begin
  Result := '';
  ScanCode := MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    Result := KeyName;
  end;
end;

function scdTextToShortCut(Text: string): TShortCut;

  function CompareFront(var Text: string; const Front: string): Boolean;
  begin
    Result := False;
    if (Length(Text) >= Length(Front)) and
      (AnsiStrLIComp(PChar(Text), PChar(Front), Length(Front)) = 0) then
    begin
      Result := True;
      Delete(Text, 1, Length(Front));
    end;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    if CompareFront(Text, scdKeyCaps[scdkcShift]) then Shift := Shift or scShift
    else if CompareFront(Text, '^') then Shift := Shift or scCtrl
    else if CompareFront(Text, scdKeyCaps[scdkcCtrl]) then Shift := Shift or scCtrl
    else if CompareFront(Text, scdKeyCaps[scdkcAlt]) then Shift := Shift or scAlt
    else Break;
  end;
  if Text = '' then Exit;
  for Key := $08 to $255 do { Copy range from table in ShortCutToText }
    if AnsiCompareText(Text, scdShortCutToText(Key)) = 0 then
    begin
      Result := Key or Shift;
      Exit;
    end;
end;

function scdShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09:
      Name := scdKeyCaps[TSCDeKeyCap(Ord(scdkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name := scdKeyCaps[scdkcEnter];
    $1B: Name := scdKeyCaps[scdkcEsc];
    $20..$28:
      Name := scdKeyCaps[TSCDeKeyCap(Ord(scdkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E:
      Name := scdKeyCaps[TSCDeKeyCap(Ord(scdkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name := scdGetSpecialKeyName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scCtrl <> 0 then Result := Result + scdKeyCaps[scdkcCtrl];
    if ShortCut and scShift <> 0 then Result := Result + scdKeyCaps[scdkcShift];
    if ShortCut and scAlt <> 0 then Result := Result + scdKeyCaps[scdkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

function  scdShiftStateToKeys(Shift: TShiftState): Word;
begin
  Result := 0;
  if ssShift in Shift then Result := Result or SCDE_SHIFT;
  if ssCtrl in Shift then Result := Result or SCDE_CONTROL;
  if ssLeft in Shift then Result := Result or SCDE_LBUTTON;
  if ssRight in Shift then Result := Result or SCDE_RBUTTON;
  if ssMiddle in Shift then Result := Result or SCDE_MBUTTON;
  if ssAlt in Shift then Result := Result or SCDE_MENU;
end;

function scdKeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function scdKeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and AltMask <> 0 then Include(Result, ssAlt);
end;

function scdKeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
end;

function scdDblClickTime: DWord;
begin
  Result := GetDoubleClickTime;
end;

function scdKeyboardDelay: Integer;
var
  D: DWord;
begin
  SystemParametersInfo(SPI_GETKEYBOARDDELAY, 0, @D, 0);
  Result := 200*(D + 1);
end;

function scdKeyBoardSpeed: Integer;
var
  D: DWord;
begin
  SystemParametersInfo(SPI_GETKEYBOARDSPEED, 0, @D, 0);
  Result := Round(1000 / (2.3*(D + 1)));
end;

procedure scdKillMessage(Wnd: HWnd; Msg: Integer);
var
  Ms: TMsg;
begin
  Ms.Message := 0;
  if PeekMessage(Ms, Wnd, Msg, Msg, PM_REMOVE) and (Ms.Message = WM_QUIT) then
    PostQuitMessage(Ms.Wparam);
end;

function scdVertScrollbarWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
end;

function scdVertScrollButtonHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYVSCROLL);
end;

function scdHorzScrollbarHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYHSCROLL);
end;

function scdHorzScrollButtonWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXHSCROLL);
end;

procedure scdApplicationCancelHint;
begin
  Application.CancelHint;
end;

function scdApplicationHintHidePause: Integer;
begin
  Result := Application.HintHidePause;
end;

function scdComponentToString(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result := StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

function scdStringToComponent(Value: string): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(nil);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

function scdIsHex(S: string): Boolean;
begin
  Result := False;
  if Length(S) > 0 then
  begin
    if S[1] <> '$' then S := '$' + S;
    Result := scdIsInteger(S);
  end;
end;

function scdIsFloat(const S: string): Boolean;
var
  V: Extended;
begin
  Result := scdIsFloat(S, V);
end;

function scdIsFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function scdIsFloat(const S: string; Currency: Boolean): Boolean;
var
  Value: Extended;
begin
  if Currency then
    Result := TextToFloat(PChar(S), Value, fvCurrency)
  else
    Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function scdIsFloat2(const S: string; out Value: Extended): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function scdStrToFloatDef(const S: String; Def: Extended): Extended;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Def;
end;

function scdIsInteger(const S: string): Boolean;
var
  V: Integer;
begin
  Result := scdIsInteger(S, V);
end;

function scdIsInteger(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function scdIsInt64(const S: string; out Value: Int64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function scdArrangeDecimalSeperator(S: String; IsCurrency: Boolean; DecimalPlaces: Integer): String;
var
  Str: String;
  DecimalChar: Char;
  P, I, Len: Integer;
begin
  Result := S;

  DecimalChar := DecimalSeparator;
  // if IsCurrency then DecimalChar := scCurDecimalSeparator;

  P := Pos(DecimalChar, S);

  if P = 0 then
  begin
    if DecimalPlaces > 0 then
      Result := Result + DecimalChar + StringOfChar('0', DecimalPlaces);

    Exit;
  end;

  if DecimalPlaces > 20 then DecimalPlaces := 20;

  Str := Copy(Result, P + 1, Length(Result) - P);
  Len := Length(Str);

  Result := Copy(Result, 1, P - 1);

  if DecimalPlaces < 0 then
  begin
    if Len > 20 then Str := Copy(Str, 1, 20);
    Result := Result + DecimalChar + Str;

    Exit;
  end;

  if Len = DecimalPlaces then
  begin
    if Len > 0 then
      Result := Result + DecimalChar + Str;

    Exit;
  end;

  if DecimalPlaces = 0 then
  begin
    P := 0;
    for I := Len downto 1 do
    begin
      if Str[I] <> '0' then
        Break;

      Inc(P);
    end;

    Delete(Str, Len - P + 1, P);
    Len := Length(Str);
  end;

  if Len > DecimalPlaces then
    Str := Copy(Str, 1, DecimalPlaces)
  else if Len < DecimalPlaces then
    Str := Str + StringOfChar('0', DecimalPlaces - Len);

  if Str <> '' then
    Result := Result + DecimalChar + Str;
end;

function scdFloatToStr(Value: Extended; IsCurrency: Boolean): String;
begin
  Result := scdArrangeDecimalSeperator(FloatToStr(Value), IsCurrency);
end;

function scdCurrencyToStr(Value: Extended; UseThousandsSeperator: Boolean;
  DecimalPlaces: Byte): String;
begin
  Result := FloatToStr(Value);
  if not UseThousandsSeperator then
    Result := StringReplace(Result, ThousandSeparator, '', [rfReplaceAll])
  else
    Result := scdArrangeDecimalSeperator(FloatToStr(Value), True, DecimalPlaces);
end;

function scdVarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

procedure SetInitializeValues;
{$IFDEF SCDE_DELPHI6_UP}
var
  S: String;
{$ENDIF}
begin
  try
    scdCurrentOperatingSystem := scdGetWinOperatingSystem;
  except
    scdCurrentOperatingSystem := scdOsUnknown;
  end;

  try
    scdInitiateCaptionGradientColors;
    GradientLibrary := LoadLibrary(PChar('msimg32.dll'));
  except
  end;

{$IFDEF SCDE_DELPHI6_UP}
  scdDefaultLCID := GetThreadLocale;
  scdSystemLCID := GetSystemDefaultLCID;

  {$IFDEF MSWINDOWS}
  try
    scdNegativeFormat := 1;
    scdNegativeFormat := StrToIntDef(GetLocaleStr(scdDefaultLCID, LOCALE_INEGNUMBER, '0'), 1);
  except
    scdNegativeFormat := 1;
  end;

  try
    scdNegativeSign := '-';
    S := GetLocaleStr(scdDefaultLCID, LOCALE_SNEGATIVESIGN, '-');
    if Length(S) > 0 then scdNegativeSign := S[1];
  except
    scdNegativeSign := '-';
  end;

  try
    scdNegativeCurFormat := 1;
    scdNegativeCurFormat := StrToIntDef(GetLocaleStr(scdDefaultLCID, LOCALE_INEGCURR, '0'), 1);
  except
    scdNegativeCurFormat := 1;
  end;

  try
    scdSysDecimalSeparator := '.';
    S := GetLocaleChar(scdSystemLCID, LOCALE_SDECIMAL, '.');
    if Length(S) = 0 then
      scdSysDecimalSeparator := '.'
    else
      scdSysDecimalSeparator := S[1];
  except
    scdSysDecimalSeparator := '.';
  end;

  try
    scdCurThousandSeparator := ',';
    S := GetLocaleChar(scdDefaultLCID, LOCALE_SMONTHOUSANDSEP, ',');
    if Length(S) = 0 then
      scdCurThousandSeparator := ','
    else
      scdCurThousandSeparator := S[1];
  except
    scdCurThousandSeparator := ',';
  end;

  try
    scdCurDecimalSeparator := '.';
    S := GetLocaleChar(scdDefaultLCID, LOCALE_SMONDECIMALSEP, '.');
    if Length(S) = 0 then
      scdCurDecimalSeparator := '.'
    else
      scdCurDecimalSeparator := S[1];
  except
    scdCurDecimalSeparator := '.';
  end;
  {$ENDIF}
{$ENDIF}
end;

procedure scdRaiseLastOSError;
var
  LastError: Integer;
  Error: ESCOSError;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := ESCOSError.Create(Format(SSCDE_OSError, [LastError,
      SysErrorMessage(LastError)]))
  else
    Error := ESCOSError.Create(SSCDE_UnkOSError);
  Error.ErrorCode := LastError;
  raise Error;
end;

initialization
  SetInitializeValues;

finalization
  if GradientLibrary > 0 then
    FreeLibrary(GradientLibrary);

end.



