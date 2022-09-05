unit SpTBXDefaultSkins;

{==============================================================================
Version 2.4.8

The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

History:
15 April 2013 - version 2.4.8
  - No changes.

7 February 2012 - version 2.4.7
  - Minor bug fixes.
  - Added support for Delphi XE2.
  - Added support for 64 bit Delphi compiler.

25 June 2011 - version 2.4.6
  - No changes.

12 March 2010 - version 2.4.5
  - No changes.

2 December 2009 - version 2.4.4
  - No changes.

13 September 2009 - version 2.4.3
  - No changes.

8 May 2009 - version 2.4.2
  - No changes.

15 March 2009 - version 2.4.1
  - No changes.

17 January 2009 - version 2.4
  - No changes.

26 September 2008 - version 2.3
  - No changes.

29 July 2008 - version 2.2
  - Fixed incorrect TRGBQuadArray declaration, thanks to Arvid
    for reporting this.
  - Fixed incorrect gradients in Office 2003 skins, thanks to
    David for reporting this.
  - Fixed Vista painting bug, the menu checkboxes are stretched
    by the themes API, this doesn't happen on XP, thanks to
    Arvid for reporting this.

26 June 2008 - version 2.1
  - New skin added: Aluminum, thanks to Pete for his contribution.

3 May 2008 - version 2.0
  - No changes.

2 April 2008 - version 1.9.5
  - No changes.

3 February 2008 - version 1.9.4
  - No changes.

19 January 2008 - version 1.9.3
  - New skin added: Human.
  
26 December 2007 - version 1.9.2
  - No changes.

1 December 2007 - version 1.9.1
  - No changes.

20 November 2007 - version 1.9
  - Initial release.

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, SpTBXSkins;

type
  { Skins }

  TSpTBXAluminumColors = array[0..3] of TColor;

  TSpTBXAluminumSkin = class(TSpTBXSkinOptions)
  private
    procedure SetDefaultColorScheme(const Value: TSpTBXLunaScheme);
  protected
    FColors: TSpTBXAluminumColors;
    FDefaultColorScheme: TSpTBXLunaScheme;
    FLightMetalColor: TColor;
    FDarkMetalColor: TColor;
    FRoughness: Integer;
    procedure FillColors; virtual;
    function GetBrushMetalColor(Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType): TColor; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FillOptions; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders: Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []); override;
    property LightMetalColor: TColor read FLightMetalColor write FLightMetalColor;
    property DarkMetalColor: TColor read FDarkMetalColor write FDarkMetalColor;
    property DefaultColorScheme: TSpTBXLunaScheme read FDefaultColorScheme write SetDefaultColorScheme default lusUnknown;
  end;

  TSpTBXAthenSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
  end;

  TSpTBXDreamSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
  end;

  TSpTBXEosSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
  end;

  TSpTBXHumanSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders: Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []); override;
  end;

  TSpTBXLeopardSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
    procedure PaintBackground(ACanvas: TCanvas; ARect: TRect; Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background, Borders: Boolean; Vertical: Boolean = False; ForceRectBorders: TAnchors = []); override;
  end;

  TSpTBXXitoSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
  end;

  { Office Skins }

  TSpTBXOfficeXPSkin = class(TSpTBXSkinOptions)
  public
    procedure FillOptions; override;
  end;

  TSpTBXOffice2003Colors = array[0..22] of TColor;

  TSpTBXOffice2003Skin = class(TSpTBXSkinOptions)
  private
    procedure SetDefaultColorScheme(const Value: TSpTBXLunaScheme);
  protected
    FColors: TSpTBXOffice2003Colors;
    FDefaultColorScheme: TSpTBXLunaScheme;
    procedure FillColors; virtual;
  public
    constructor Create; override;
    procedure FillOptions; override;
    property DefaultColorScheme: TSpTBXLunaScheme read FDefaultColorScheme write SetDefaultColorScheme default lusUnknown;
  end;

  TSpTBXOffice2007Colors = array[0..17] of TColor;

  TSpTBXOffice2007Skin = class(TSpTBXSkinOptions)
  protected
    FColors: TSpTBXOffice2007Colors;
    procedure FillColors; virtual; abstract;
  public
    procedure FillOptions; override;
  end;

  TSpTBXOffice2007BlueSkin = class(TSpTBXOffice2007Skin)
  protected
    procedure FillColors; override;
  end;

  TSpTBXOffice2007BlackSkin = class(TSpTBXOffice2007Skin)
  protected
    procedure FillColors; override;
  end;

  TSpTBXOffice2007SilverSkin = class(TSpTBXOffice2007Skin)
  protected
    procedure FillColors; override;
  end;

implementation

uses
  Math;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Brushed Fill }

const
  THREAD_CACHE_SIZE = 16;
  NUM_TEMPLATES = 8;
  MIN_TEMPLATE_SIZE = 100;
  MAX_TEMPLATE_SIZE = 200;
  NUM_RANDTHREADS = 1024;

type
  PRGBQuad = ^TRGBQuad;
  TRGBQuad = Integer;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;
  TThreadCacheItem = record
    BaseColor: TColorRef;
    Roughness: Integer;
    Bitmaps: array [0..NUM_TEMPLATES - 1] of HBITMAP;
  end;

var
  ThreadTemplates: array of array of Integer;
  RandThreadIndex: array of Integer;
  RandThreadPositions: array of Integer;
  ThreadCache: array of TThreadCacheItem;
  NextCacheEntry: Integer = 0;

function GetBGR(C: TColorRef): Cardinal;
asm
        MOV     ECX,EAX         // this function swaps R and B bytes in ABGR
        SHR     EAX,16
        XCHG    AL,CL
        MOV     AH,$FF          // and writes $FF into A component }
        SHL     EAX,16
        MOV     AX,CX
end;

procedure InitializeBrushedFill;
const
  Pi = 3.14159265358987;
var
  TemplateIndex, Size, I, V, V1, V2: Integer;
  T, R12, R13, R14, R21, R22, R23, R24: Single;
begin

  SetLength(ThreadTemplates, NUM_TEMPLATES);
  SetLength(RandThreadIndex, NUM_RANDTHREADS);
  SetLength(RandThreadPositions, NUM_RANDTHREADS);

  { Make thread templates }
  for TemplateIndex := 0 to NUM_TEMPLATES - 1 do begin
    Size := (MIN_TEMPLATE_SIZE + Random(MAX_TEMPLATE_SIZE - MIN_TEMPLATE_SIZE + 1)) div 2;
    SetLength(ThreadTemplates[TemplateIndex], Size * 2);
    R12 := Random * 2 * Pi;
    R13 := Random * 2 * Pi;
    R14 := Random * 2 * Pi;
    R21 := Random * 2 * Pi;
    R22 := Random * 2 * Pi;
    R23 := Random * 2 * Pi;
    R24 := Random * 2 * Pi;
    for I := 0 to Size - 1 do begin
      T := 2 * Pi * I / Size;
      V1 := Round(150 * Sin(T) + 100 * Sin(2 * T + R12) + 50 * Sin(3 * T + R13) + 20 * Sin(4 * T + R14));
      if V1 > 255 then V1 := 255;
      if V1 < -255 then V1 := -255;

      V2 := Round(150 * Sin(T + R21) + 100 * Sin(2 * T + R22) + 50 * Sin(3 * T + R23) + 20 * Sin(4 * T + R24));
      if V2 > 255 then V2 := 255;
      if V2 < -255 then V2 := -255;

      if Abs(V2 - V1) > 300 then begin
        V := (V1 + V2) div 2;
        V1 := V - 150;
        V2 := V + 150;
      end;

      ThreadTemplates[TemplateIndex][I * 2] := Min(V1, V2);
      ThreadTemplates[TemplateIndex][I * 2 + 1] := Max(V1, V2);
    end;
  end;

  { Initialize Rand arrays }
  for I := 0 to NUM_RANDTHREADS- 1 do begin
    RandThreadIndex[I] := Random(NUM_TEMPLATES);
    V1 := Random(Length(ThreadTemplates[RandThreadIndex[I]])) and not $1;
    if Odd(I) then Inc(V1);
    RandThreadPositions[I] := V1;
  end;
end;

procedure ClearCacheItem(var CacheItem: TThreadCacheItem);
var
  I: Integer;
begin
  with CacheItem do begin
    BaseColor := $FFFFFFFF;
    Roughness := -1;
    for I := NUM_TEMPLATES- 1 downto 0 do
      if Bitmaps[I] <> 0 then begin
        DeleteObject(Bitmaps[I]);
        Bitmaps[I] := 0;
      end;
  end;
end;

procedure ResetBrushedFillCache;
var
  I: Integer;
begin
  { Should be called each time the screen parameters change }
  for I := High(ThreadCache) downto 0 do ClearCacheItem(ThreadCache[I]);
end;

procedure FinalizeBrushedFill;
begin
  ResetBrushedFillCache;
  SetLength(ThreadCache, 0);
  ThreadCache := nil;
  SetLength(RandThreadPositions, 0);
  RandThreadPositions := nil;
  SetLength(RandThreadIndex, 0);
  RandThreadIndex := nil;
  SetLength(ThreadTemplates, 0, 0);
  ThreadTemplates := nil;
end;

procedure MakeCacheItem(var CacheItem: TThreadCacheItem; Color: TColorRef; Roughness: Integer);
var
  TemplateIndex, Size, I, V: Integer;
  CR, CG, CB: Integer;
  R, G, B: Integer;
  ScreenDC: HDC;
  BMI: TBitmapInfo;
  Bits: PRGBQuadArray;
  DIBSection: HBITMAP;
  DIBDC, CacheDC: HDC;
begin
  ScreenDC := GetDC(0);
  FillChar(BMI, SizeOf(TBitmapInfo), 0);
  with BMI.bmiHeader do begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biCompression := BI_RGB;
    biWidth := MAX_TEMPLATE_SIZE;
    biHeight := -1;
    biBitCount := 32;
  end;
  DIBSection := CreateDIBSection(0, BMI, DIB_RGB_COLORS, Pointer(Bits), 0, 0);
  DIBDC := CreateCompatibleDC(0);
  SelectObject(DIBDC, DIBSection);
  CacheDC := CreateCompatibleDC(0);

  CR := Color shl 8 and $FF00;
  CG := Color and $FF00;
  CB := Color shr 8 and $FF00;
  try
    for TemplateIndex := 0 to NUM_TEMPLATES - 1 do begin
      CacheItem.BaseColor := Color;
      CacheItem.Roughness := Roughness;
      Size := Length(ThreadTemplates[TemplateIndex]);

      if CacheItem.Bitmaps[TemplateIndex] = 0 then
        CacheItem.Bitmaps[TemplateIndex] := CreateCompatibleBitmap(ScreenDC, Size, 1);
      SelectObject(CacheDC, CacheItem.Bitmaps[TemplateIndex]);

      for I := 0 to Size - 1 do begin
        V := ThreadTemplates[TemplateIndex][I];
        R := CR + V * Roughness;
        G := CG + V * Roughness;
        B := CB + V * Roughness;
        if R < 0 then R := 0;
        if G < 0 then G := 0;
        if B < 0 then B := 0;
        if R > $EF00 then R := $EF00;
        if G > $EF00 then G := $EF00;
        if B > $EF00 then B := $EF00;
        Bits^[I] := (R and $FF00 + (G and $FF00) shl 8 + (B and $FF00) shl 16) shr 8;
      end;

      BitBlt(CacheDC, 0, 0, Size, 1, DIBDC, 0, 0, SRCCOPY);
    end;
  finally
    DeleteDC(CacheDC);
    DeleteDC(DIBDC);
    DeleteObject(DIBSection);
    ReleaseDC(0, ScreenDC);
  end;
end;

function FindCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := High(ThreadCache);
  while Result >= 0 do
    if (ThreadCache[Result].BaseColor = Color) and (ThreadCache[Result].Roughness = Roughness) then
      Exit
    else
      Dec(Result);
end;

function GetCacheItem(Color: TColorRef; Roughness: Integer): Integer;
begin
  Result := FindCacheItem(Color, Roughness);
  if Result >= 0 then
    Exit
  else begin
    Result := NextCacheEntry;
    if Result > High(ThreadCache) then begin
      SetLength(ThreadCache, Result+ 1);
      ClearCacheItem(ThreadCache[Result]);
    end;
    MakeCacheItem(ThreadCache[Result], Color, Roughness);
    NextCacheEntry := (NextCacheEntry + 1) mod THREAD_CACHE_SIZE;
  end;
end;

procedure BrushedFill(DC: HDC; Origin: PPoint; ARect: TRect; Color: TColor; Roughness: Integer);
const
  ZeroOrigin: TPoint = (X: 0; Y: 0);
var
  CR: TColorRef;
  X, Y: Integer;
  CacheIndex: Integer;
  TemplateIndex: Integer;
  CacheDC: HDC;
  Size: Integer;
  BoxR: TRect;
begin
  if (Color = clNone) or not RectVisible(DC, ARect) then Exit;
  CR := GetBGR(ColorToRGB(Color));
  if Origin = nil then Origin := @ZeroOrigin;
  if ThreadTemplates = nil then InitializeBrushedFill;
  CacheIndex := GetCacheItem(CR, Roughness);
  GetClipBox(DC, BoxR);
  IntersectRect(ARect, ARect, BoxR);
  SaveDC(DC);
  IntersectClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  CacheDC := CreateCompatibleDC(0);
  for Y := ARect.Top to ARect.Bottom - 1 do begin
    TemplateIndex := RandThreadIndex[(65536 + Y - Origin.Y) mod NUM_RANDTHREADS];
    Size := Length(ThreadTemplates[TemplateIndex]);
    X := -RandThreadPositions[(65536 + Y - Origin.Y) mod NUM_RANDTHREADS] + Origin.X;
    SelectObject(CacheDC, ThreadCache[CacheIndex].Bitmaps[TemplateIndex]);
    while X < ARect.Right do begin
      if X + Size >= ARect.Left then BitBlt(DC, X, Y, Size, 1, CacheDC, 0, 0, SRCCOPY);
      Inc(X, Size);
    end;
  end;
  DeleteDC(CacheDC);
  RestoreDC(DC, -1);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXAluminumSkin }

constructor TSpTBXAluminumSkin.Create;
begin
  FRoughness := 12;
  FDefaultColorScheme := lusUnknown;
  inherited;
end;

destructor TSpTBXAluminumSkin.Destroy;
begin
  FinalizeBrushedFill;
  inherited;
end;

procedure TSpTBXAluminumSkin.FillColors;
const
  AluminumColors: array[lusBlue..lusGreen] of TSpTBXAluminumColors = (
    ($C56A31,               // 0: Item borders
     $E0D7D1,               // 1: Item Checked
     $E2B598,               // 2: Item Pushed
     $EED2C1),              // 3: Item HotTrack

    ($BFB4B2,               // 0: Item borders
     $DDDCDC,               // 1: Item Checked
     $DFDAD9,               // 2: Item Pushed
     $ECE9E8),              // 3: Item HotTrack

    ($70A093,               // 0: Item borders
     $D8DCDB,               // 1: Item Checked
     $B8D0C9,               // 2: Item Pushed
     $D4E3DF)               // 3: Item HotTrack
  );
var
  Luna: TSpTBXLunaScheme;
begin
  if FDefaultColorScheme <> lusUnknown then
    Luna := FDefaultColorScheme
  else begin
    Luna := SpGetLunaScheme;
    if Luna = lusUnknown then Luna := lusMetallic;
  end;

  FColors := AluminumColors[Luna];
  FRoughness := 12;
  FLightMetalColor := $C7C7C7;
  FDarkMetalColor := $DFDFDF;
end;

procedure TSpTBXAluminumSkin.FillOptions;
begin
  SkinName := 'Aluminum';

  //---- Colors ----//

  FillColors;

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone); // Dummy, uses BrushedFill

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $F9F9F9, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $C7C7C7, $C7C7C7, clWhite, clWhite);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(1, clWhite, $D8D8D8, clNone, clNone);
  Options(skncDockablePanelTitleBar, sknsNormal).Borders.Fill(0, clWhite, $9F9F9F, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone); // Dummy, uses BrushedFill
  Options(skncPanel, sknsNormal).Borders.Fill(2, FColors[0], FColors[0], clWhite, clWhite);

  Options(skncPopup, sknsNormal).Body.Fill(0, $F9F9F9, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $7F7F7F, $7F7F7F, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone); // Dummy, uses BrushedFill

  Options(skncSplitter, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone); // Dummy, uses BrushedFill

  Options(skncToolbar, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone); // Dummy, uses BrushedFill
  Options(skncToolbar, sknsNormal).Borders.Fill(2, clWhite, $7F7F7F, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar); // Dummy, uses BrushedFill

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, clWhite, $D8D8D8, clNone, clNone);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $9F9F9F, clWhite, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(0, FColors[3], clNone, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(0, FColors[2], clNone, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(0, FColors[1], clNone, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(0, FColors[2], clNone, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  
  Options(skncOpenToolbarItem, sknsNormal).Body.Fill(0, $ECECEC, clNone, clNone, clNone);
  Options(skncOpenToolbarItem, sknsNormal).Borders.Fill(0, $7F7F7F, $7F7F7F, clNone, clNone);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);

  Options(skncButton, sknsNormal).Body.Fill(1, $EFEFEF, $D1D1D1, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(1, $9F9F9F, $9F9F9F, $F7F7F7, $C7C7C7);
  Options(skncButton, sknsDisabled).Body.Fill(1, $EFEFEF, $D1D1D1, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(1, $9F9F9F, $9F9F9F, $F7F7F7, $C7C7C7);
  Options(skncButton, sknsHotTrack).Body.Fill(1, $F2F2F2, $DDDDDD, clNone, clNone);
  Options(skncButton, sknsHotTrack).Borders.Fill(1, $BABABA, $BABABA, $F8F8F8, $D6D6D6);
  Options(skncButton, sknsPushed).Body.Fill(1, $D1D1D1, $EFEFEF, clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(1, $9F9F9F, $9F9F9F, $C7C7C7, $F7F7F7);
  Options(skncButton, sknsChecked).Body.Fill(1, $D1D1D1, $EFEFEF, clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(1, $9F9F9F, $9F9F9F, $C7C7C7, $F7F7F7);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(1, $D1D1D1, $EFEFEF, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(1, $9F9F9F, $9F9F9F, $C7C7C7, $F7F7F7);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Borders.Fill(1, $9F9F9F, $9F9F9F, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(1, $99A8AC, $99A8AC, clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $9F9F9F, $9F9F9F);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, $9F9F9F, $9F9F9F);

  CopyOptions(skncButton, skncEditButton);
  Options(skncEditButton, sknsNormal).Reset;

  //---- Tabs ----//
  CopyOptions(skncToolbarItem, skncTab);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, FColors[1], clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, FColors[0], FColors[0], clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, FColors[2], clNone, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(0, FColors[3], clNone, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, FColors[3], clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(0, FColors[1], clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(1, FColors[0], FColors[0], clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(1, $EFEFEF, $D1D1D1, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $F7F7F7, $C7C7C7, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(1, $F2F2F2, $DDDDDD, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $F8F8F8, $D6D6D6, clNone, clNone);
  Options(skncHeader, sknsPushed).Body.Fill(1, $D1D1D1, $EFEFEF, clNone, clNone);
  Options(skncHeader, sknsPushed).Borders.Fill(0, $C7C7C7, $F7F7F7, clNone, clNone);
end;

function TSpTBXAluminumSkin.GetBrushMetalColor(Component: TSpTBXSkinComponentsType;
  State: TSpTBXSkinStatesType): TColor;
begin
  Result := clNone;
  if Component in [skncDock, skncPanel, skncTabToolbar, skncStatusBar, skncSplitter] then
    Result := FLightMetalColor
  else
    if Component in [skncToolbar, skncMenuBar] then
      Result := FDarkMetalColor;
end;

procedure TSpTBXAluminumSkin.PaintBackground(ACanvas: TCanvas; ARect: TRect;
  Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType;
  Background, Borders, Vertical: Boolean; ForceRectBorders: TAnchors);
var
  BackgroundRect: TRect;
  Op: TSpTBXSkinOptionCategory;
  BrushColor: TColor;
begin
  BrushColor := GetBrushMetalColor(Component, State);

  if BrushColor = clNone then
    inherited
  else begin
    Op := Options(Component, State);

    if Op.Borders.IsEmpty then
      Borders := False;
    if Op.Body.IsEmpty then
      Background := False;

    if Background then begin
      BackgroundRect := ARect;
      if Borders then
        InflateRect(BackgroundRect, -1, -1);
      BrushedFill(ACanvas.Handle, nil, BackgroundRect, BrushColor, FRoughness);
    end;

    if Borders then
      SpPaintSkinBorders(ACanvas, ARect, Op, ForceRectBorders);
  end;
end;

procedure TSpTBXAluminumSkin.SetDefaultColorScheme(const Value: TSpTBXLunaScheme);
begin
  if FDefaultColorScheme <> Value then begin
    FDefaultColorScheme := Value;
    Reset;
    FillOptions;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXAthenSkin }

procedure TSpTBXAthenSkin.FillOptions;
begin
  SkinName := 'Athen';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $E0E0E0, $E0E0E0, clWhite, clWhite);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);

  Options(skncPanel, sknsNormal).Body.Fill(1, $FBF9A0, clWhite, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $D0D0D0, $D0D0D0, $F8F8F8, $F8F8F8);

  Options(skncPopup, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);

  Options(skncSplitter, sknsNormal).Body.Fill(1, $D8D8D8, $F0F0F0, clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncToolbar, sknsNormal).Borders.Fill(1, clWhite, $B0B0B0, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(1, $DAB370, $DAB370, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(1, $D0D0D0, $B0B0B0, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(1, $DAB370, $DAB370, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(3, $FAECDE, $E0B080, $EDDE73, $FFFFBA);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(1, $DAB370, $DAB370, clNone, clNone);

  Options(skncOpenToolbarItem, sknsNormal).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncOpenToolbarItem, sknsNormal).Borders.Fill(1, $D0D0D0, $B0B0B0, clNone, clNone);

  Options(skncMenuBarItem, sknsHotTrack).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncMenuBarItem, sknsPushed).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncMenuBarItem, sknsChecked).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Body.Fill(3, $FAECDE, $E0B080, $EDDE73, $FFFFBA);

  CopyOptions(skncToolbarItem, skncMenuItem);
  Options(skncMenuItem, sknsHotTrack).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncMenuItem, sknsHotTrack).Borders.Fill(1, $C0C0C0, $C0C0C0, clNone, clNone);

  CopyOptions(skncToolbarItem, skncButton);
  Options(skncButton, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncButton, sknsNormal).Borders.Fill(1, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncButton, sknsDisabled).Borders.Fill(1, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(30);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(30);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-30);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-30);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncCheckBox, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(0, $99A8AC, $99A8AC, clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $D0D0D0, $D0D0D0);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, $DAB370, $DAB370);

  CopyOptions(skncToolbarItem, skncEditButton);

  //---- Tabs ----//
  Options(skncTab, sknsHotTrack).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncTab, sknsHotTrack).Borders.Fill(2, $DAB370, $DAB370, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncTab, sknsChecked).Borders.Fill(2, $DAB370, $DAB370, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(3, $FAECDE, $E0B080, $EDDE73, $FFFFBA);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $DAB370, $DAB370, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(1, $FBF9A0, clWhite, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, $DAB370, $DAB370, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(1, $D0D0D0, $B0B0B0, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $DAB370, $DAB370, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(1, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(1, $DAB370, $DAB370, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(3, $F8F8F8, $D8D8D8, $F0F0F0, $F8F8F8);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(3, $FFF8F0, $E0B080, $F0E870, $FFFFB0);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $F7E4D0, $DAB370, clNone, clNone);
  Options(skncHeader, sknsPushed).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncHeader, sknsPushed).Borders.Fill(0, $D0D0D0, $B0B0B0, clNone, clNone);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXDreamSkin }

procedure TSpTBXDreamSkin.FillOptions;
begin
  SkinName := 'Dream';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $E0E0E0, $E0E0E0, clWhite, clWhite);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(1, $D1F9FF, clWhite, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $808080, $808080, $B0D4DF, clWhite);

  Options(skncPopup, sknsNormal).Body.Fill(0, $F9F9F9, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(1, $DDD9D2, clWhite, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);
  Options(skncToolbar, sknsNormal).Borders.Fill(2, clWhite, $B0B0B0, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(1, clWhite, $E5E2DB, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(1, $909090, $909090, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(1, $909090, $909090, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(1, $D2F9FF, $F0FDFF, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(1, $808080, $808080, $B0D4DF, $F0FDFF);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(1, $2695EA, $80E0FF, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(1, $808080, $808080, $60B0E8, $80E0FF);

  Options(skncOpenToolbarItem, sknsNormal).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncOpenToolbarItem, sknsNormal).Borders.Fill(1, $909090, $909090, clNone, clNone);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);

  CopyOptions(skncToolbarItem, skncButton);
  Options(skncButton, sknsNormal).Body.Fill(1, clWhite, $DCD8D0, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(1, $909090, $909090, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(1, clWhite, $DCD8D0, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(1, $909090, $909090, clNone, clNone);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Borders.Fill(0, $808080, $808080, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(0, $99A8AC, $99A8AC, clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $D0D0D0, $D0D0D0);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, $909090, $909090);

  CopyOptions(skncToolbarItem, skncEditButton);

  //---- Tabs ----//
  Options(skncTab, sknsHotTrack).Body.Fill(0, $F4F0F0, clNone, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(1, $A0A0A0, $A0A0A0, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(1, $F0FDFF, $D2F9FF, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(1, $808080, $808080, $B0D4DF, $F0FDFF);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(1, $2695EA, $80E0FF, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(1, $808080, $808080, $60B0E8, $80E0FF);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(1, $D2F9FF, $F0FDFF, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, $808080, $808080, $B0D4DF, $F0FDFF);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $909090, $909090, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(1, clWhite, $DDD9D2, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $909090, $909090, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, $F4F0F0, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(1, $909090, $909090, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(1, $D1F9FF, clWhite, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(1, $808080, $808080, $B0D4DF, clWhite);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(1, clWhite, $DCD8D0, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(1, clWhite, $E5E2DB, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEosSkin }

procedure TSpTBXEosSkin.FillOptions;
begin
  SkinName := 'Eos';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, $809090, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $838C8C, $838C8C, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(0, $5A6666, clNone, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncToolbar, sknsNormal).Borders.Fill(0, $838C8C, $5A6666, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(0, $5A6666, clNone, clNone, clNone);
  Options(skncWindowTitleBar, sknsNormal).TextColor := $EEEEEE;  // This overrides the Items text color

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $576363, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $576363, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $869999, clNone, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsNormal).TextColor := $EEEEEE;
  Options(skncToolbarItem, sknsHotTrack).TextColor := $68CAE6;
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(0, $5F6D6D, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).TextColor := clBlack;
  Options(skncToolbarItem, sknsPushed).Body.Fill(0, $7AD2EA, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).TextColor := $EEEEEE;
  Options(skncToolbarItem, sknsChecked).Body.Fill(0, $94A0A0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).TextColor := clBlack;
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(0, $8ED7EC, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncToolbarItem, sknsDisabled).TextColor := $99A8AC;

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);
  Options(skncMenuItem, sknsPushed).TextColor := $68CAE6;
  Options(skncMenuItem, sknsCheckedAndHotTrack).Body.Fill(0, $94A0A0, clNone, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).TextColor := $68CAE6;

  Options(skncButton, sknsNormal).TextColor := $EEEEEE;
  Options(skncButton, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(0, clWhite, $3C4444, $838C8C, $5A6666);
  Options(skncButton, sknsHotTrack).TextColor := $68CAE6;
  Options(skncButton, sknsHotTrack).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsHotTrack).Borders.Fill(0, clWhite, $3C4444, $838C8C, $5A6666);
  Options(skncButton, sknsPushed).TextColor := $EEEEEE;
  Options(skncButton, sknsPushed).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(0, $3C4444, clWhite, $5A6666, $838C8C);
  Options(skncButton, sknsChecked).TextColor := $EEEEEE;
  Options(skncButton, sknsChecked).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(0, $3C4444, clWhite, $5A6666, $838C8C);
  Options(skncButton, sknsCheckedAndHotTrack).TextColor := $68CAE6;
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(0, $3C4444, clWhite, $5A6666, $838C8C);
  Options(skncButton, sknsDisabled).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(0, clWhite, $3C4444, $838C8C, $5A6666);
  Options(skncButton, sknsDisabled).TextColor := $99A8AC;

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(30);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(30);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-30);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-30);

  Options(skncCheckBox, sknsNormal).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);
  Options(skncCheckBox, sknsHotTrack).Body.Fill(0, $5F6D6D, clNone, clNone, clNone);
  Options(skncCheckBox, sknsHotTrack).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Body.Fill(0, $7AD2EA, clNone, clNone, clNone);
  Options(skncCheckBox, sknsPushed).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Body.Fill(0, $94A0A0, clNone, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Body.Fill(0, $8ED7EC, clNone, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Borders.Fill(0, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(0, $99A8AC, $99A8AC, clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $D0D0D0, $D0D0D0);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, $94A0A0, $94A0A0);

  CopyOptions(skncToolbarItem, skncEditButton);
  Options(skncEditButton, sknsNormal).TextColor := clBlack;

  //---- Tabs ----//
  Options(skncTab, sknsNormal).TextColor := $EEEEEE;
  Options(skncTab, sknsHotTrack).TextColor := $68CAE6;
  Options(skncTab, sknsHotTrack).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);
  Options(skncTab, sknsChecked).TextColor := $EEEEEE;
  Options(skncTab, sknsChecked).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).TextColor := $68CAE6;
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(0, $5A6666, $5A6666, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(0, $94A0A0, clNone, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(1, $5A6666, $5A6666, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);
  Options(skncTrackBar, sknsNormal).TextColor := $5A6666;

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, $5A6666, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(1, $94A0A0, $94A0A0, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(0, $809090, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(1, $94A0A0, $94A0A0, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).TextColor := $EEEEEE;
  Options(skncHeader, sknsNormal).Body.Fill(0, $708080, clNone, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $838C8C, $5A6666, clNone, clNone);
  Options(skncHeader, sknsHotTrack).TextColor := $68CAE6;
  Options(skncHeader, sknsHotTrack).Body.Fill(0, $5F6D6D, clNone, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $838C8C, $5A6666, clNone, clNone);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXHumanSkin }

procedure TSpTBXHumanSkin.FillOptions;
begin
  SkinName := 'Human';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $F1F6F9, clNone, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);
  Options(skncDockablePanelTitleBar, sknsNormal).Borders.Fill(0, $B9BCBF, $B9BCBF, clWhite, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, $F1F6F9, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $B7C1CB, $B7C1CB, clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, $F2F5F8, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $91A3B6, $91A3B6, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);
  Options(skncToolbar, sknsNormal).Borders.Fill(0, clWhite, $B7C1CB, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $383A3D, $C7CFD8, $E7EBEF, $E7EBEF);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(5, $6AAAE8, $468CD1, $3A82C8, $488ED3);
  Options(skncWindowTitleBar, sknsNormal).TextColor := clWhite;  // This overrides the Items text color

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $B7C1CB, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $B7C1CB, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $B7C1CB, clWhite, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(2, $1E4B8F, $1E4B8F, $1975FF, $1975FF);
  Options(skncToolbarItem, sknsPushed).Body.Fill(0, $D4DBE0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(2, $606A73, $606A73, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(0, $D4DBE0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(2, $606A73, $606A73, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $1E4B8F, $1E4B8F, $1975FF, $1975FF);

  Options(skncMenuBarItem, sknsHotTrack).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);

  Options(skncMenuItem, sknsHotTrack).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuItem, sknsHotTrack).Borders.Fill(1, $69BAEF, $69BAEF, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuItem, sknsChecked).Borders.Fill(1, $69BAEF, $69BAEF, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Body.Fill(1, $ACDEFE, $7BC5F5, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Fill(1, $69BAEF, $69BAEF, clNone, clNone);

  CopyOptions(skncToolbarItem, skncButton);
  Options(skncButton, sknsNormal).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncButton, sknsNormal).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);
  Options(skncButton, sknsDisabled).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncButton, sknsDisabled).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);

  Options(skncListItem, sknsChecked).Assign(Options(skncMenuBarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncMenuBarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncMenuBarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  Options(skncCheckBox, sknsNormal).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncCheckBox, sknsNormal).Borders.Fill(2, $666666, $666666, $F1F1F1, $F1F1F1);
  Options(skncCheckBox, sknsDisabled).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);
  Options(skncCheckBox, sknsPushed).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncCheckBox, sknsPushed).Borders.Fill(2, $666666, $666666, $F1F1F1, $F1F1F1);
  Options(skncCheckBox, sknsHotTrack).Body.Fill(1, $76ADFF, $2C7DF7, clNone, clNone);
  Options(skncCheckBox, sknsHotTrack).Borders.Fill(2, $214A87, $214A87, $8FBCFD, $8FBCFD);
  Options(skncCheckBox, sknsChecked).Body.Fill(1, $76ADFF, $2C7DF7, clNone, clNone);
  Options(skncCheckBox, sknsChecked).Borders.Fill(2, $214A87, $214A87, $8FBCFD, $8FBCFD);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Body.Fill(1, $76ADFF, $2C7DF7, clNone, clNone);
  Options(skncCheckBox, sknsCheckedAndHotTrack).Borders.Fill(2, $214A87, $214A87, $8FBCFD, $8FBCFD);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, $778B97, $778B97, $E6E6E6, $E6E6E6);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $E6E6E6, $E6E6E6);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, $1E4B8F, $1E4B8F, $1975FF, $1975FF);

  Options(skncEditButton, sknsHotTrack).Body.Fill(1, $76ADFF, $2C7DF7, clNone, clNone);
  Options(skncEditButton, sknsHotTrack).Borders.Fill(0, $1975FF, $1975FF, $8FBCFD, $8FBCFD);
  Options(skncEditButton, sknsPushed).Body.Fill(1, $3787FF, $0A66F0, clNone, clNone);
  Options(skncEditButton, sknsPushed).Borders.Fill(0, $1975FF, $1975FF, $589AFC, $589AFC);

  //---- Tabs ----//
  Options(skncTab, sknsNormal).Body.Fill(0, $DBDFE4, clNone, clNone, clNone);
  Options(skncTab, sknsNormal).Borders.Fill(2, $B8C2CA, $B8C2CA, $E1E4E8, $E1E4E8);
  Options(skncTab, sknsDisabled).Body.Fill(0, $DBDFE4, clNone, clNone, clNone);
  Options(skncTab, sknsDisabled).Borders.Fill(2, $B8C2CA, $B8C2CA, $E1E4E8, $E1E4E8);

  Options(skncTab, sknsHotTrack).Body.Fill(1, clWhite, $E7EBEF, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(2, $677883, $677883, clWhite, $DCE0E4);
  Options(skncTab, sknsChecked).Body.Fill(1, clWhite, $E7EBEF, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(2, $677883, $677883, clWhite, $DCE0E4);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(1, clWhite, $E7EBEF, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $677883, $677883, clWhite, $DCE0E4);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, $677883, $677883, clWhite, $DCE0E4);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(5, $9CC3FF, $60A0FF, $1672FD, $63A1FF);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);


  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(5, clWhite, $EDF0F3, $E2E7EC, $E7EBEF);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(5, $9CC3FF, $60A0FF, $1672FD, $63A1FF);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(2, $D0D4D8, $D0D4D8, $606A73, $606A73);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(0, $E7EBEF, clNone, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $F3F5F7, $8F9CAA, clNone, $D4D8DB);
  Options(skncHeader, sknsHotTrack).Body.Fill(0, $EEF2F6, clNone, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $F3F5F7, $8F9CAA, clNone, $D4D8DB);
end;

procedure TSpTBXHumanSkin.PaintBackground(ACanvas: TCanvas; ARect: TRect;
  Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background,
  Borders, Vertical: Boolean; ForceRectBorders: TAnchors);
begin
  inherited;
  // Override the Tab painting
  if (Component = skncTab) and (State in [sknsHotTrack, sknsPushed, sknsChecked, sknsCheckedAndHotTrack]) then begin
    SpDrawLine(ACanvas, ARect.Left + 2, ARect.Top, ARect.Right - 2, ARect.Top, $1E4B8F);
    SpDrawLine(ACanvas, ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Top + 1, $1975FF);
    SpDrawLine(ACanvas, ARect.Left, ARect.Top + 2, ARect.Right, ARect.Top + 2, $1975FF);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXLeopardSkin }

procedure TSpTBXLeopardSkin.FillOptions;
begin
  SkinName := 'Leopard';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(1, $CFCFCF, $999797, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, $EAE4DD, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $999797, $999797, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(1, $C0C0C0, $999797, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, $EAE4DD, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $999797, $999797, clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(1, $C2C5C3, $999797, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(1, $C2C5C3, $999797, clNone, clNone);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, $C2C5C3, $999797, clNone, clNone);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $646262, $CECECE, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $646262, $CECECE, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $646262, $CECECE, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(1, clWhite, $A8A8A8, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(2, $848484, $848484, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, $343434, $696969, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(2, $272727, $272727, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).TextColor := clWhite;
  Options(skncToolbarItem, sknsChecked).Body.Fill(1, $343434, $696969, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(2, $272727, $272727, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).TextColor := clWhite;
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(1, $343434, $696969, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $272727, $272727, $343434, $343434);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).TextColor := clWhite;
  Options(skncToolbarItem, sknsDisabled).TextColor := $89736D;

  Options(skncMenuBarItem, sknsHotTrack).Body.Fill(0, $D87439, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsHotTrack).TextColor := clWhite;
  Options(skncMenuBarItem, sknsPushed).Body.Fill(0, $D87439, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsPushed).TextColor := clWhite;
  Options(skncMenuBarItem, sknsChecked).Body.Fill(0, $D87439, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsChecked).TextColor := clWhite;
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).Body.Fill(0, $D87439, clNone, clNone, clNone);
  Options(skncMenuBarItem, sknsCheckedAndHotTrack).TextColor := clWhite;

  Options(skncMenuItem, sknsHotTrack).Body.Fill(0, $D87439, clNone, clNone, clNone);
  Options(skncMenuItem, sknsHotTrack).TextColor := clWhite;
  Options(skncMenuItem, sknsChecked).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncMenuItem, sknsChecked).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncMenuItem, sknsCheckedAndHotTrack).TextColor := clWhite;

  Options(skncButton, sknsNormal).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncButton, sknsNormal).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncButton, sknsDisabled).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncButton, sknsHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncButton, sknsPushed).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncButton, sknsPushed).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncButton, sknsChecked).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncButton, sknsChecked).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);

  Options(skncListItem, sknsChecked).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(30);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(30);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncButton, skncCheckBox);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $D0D0D0, $D0D0D0);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, $D99D51, $D99D51, $FFD3A7, $FFD3A7);

  Options(skncEditButton, sknsHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncEditButton, sknsHotTrack).Borders.Fill(0, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncEditButton, sknsPushed).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncEditButton, sknsPushed).Borders.Fill(0, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncEditButton, sknsChecked).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncEditButton, sknsChecked).Borders.Fill(0, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncEditButton, sknsCheckedAndHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncEditButton, sknsCheckedAndHotTrack).Borders.Fill(0, $DB9D79, $DB9D79, clNone, clNone);

  //---- Tabs ----//
  Options(skncTab, sknsNormal).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncTab, sknsNormal).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncTab, sknsDisabled).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncTab, sknsDisabled).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncTab, sknsHotTrack).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncTab, sknsHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncTab, sknsChecked).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $EAE4DD, clNone, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(5, $FFD6B7, $FFC689, $F3AF67, $FCFBBE);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(2, $DB9D79, $DB9D79, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(5, clWhite, $EFEFEF, $E3E3E3, clWhite);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $F5BA82, $DB9D79, clNone, clNone);
  Options(skncHeader, sknsPushed).Body.Fill(5, $FFE7D5, $FFD0A2, $F5BA82, $FDFCDA);
  Options(skncHeader, sknsPushed).Borders.Fill(0, $F5BA82, $DB9D79, clNone, clNone);
end;

procedure TSpTBXLeopardSkin.PaintBackground(ACanvas: TCanvas; ARect: TRect;
  Component: TSpTBXSkinComponentsType; State: TSpTBXSkinStatesType; Background,
  Borders, Vertical: Boolean; ForceRectBorders: TAnchors);
begin
  inherited;
  // Override the TabBackground painting
  if Component = skncTabBackground then begin
    SpDrawLine(ACanvas, ARect.Left, ARect.Top, ARect.Right, ARect.Top, $DB9D79);
    SpDrawLine(ACanvas, ARect.Left, ARect.Top + 1, ARect.Right, ARect.Top + 1, $FAEEAF);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXXitoSkin }

procedure TSpTBXXitoSkin.FillOptions;
begin
  SkinName := 'Xito';

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, $E0E0E0, $E0E0E0, clWhite, clWhite);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(5, clWhite, $F2F2F2, $EAEAEA, $EFEFEF);

  Options(skncPanel, sknsNormal).Body.Fill(0, $D8D8D8, clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $909090, $909090, $D0D0D0, $D0D0D0);

  Options(skncPopup, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(5, clWhite, $F2F2F2, $EAEAEA, $EFEFEF);

  Options(skncSplitter, sknsNormal).Body.Fill(5, clWhite, $F2F2F2, $EAEAEA, $EFEFEF);

  Options(skncToolbar, sknsNormal).Body.Fill(5, clWhite, $F2F2F2, $EAEAEA, $EFEFEF);
  Options(skncToolbar, sknsNormal).Borders.Fill(2, clWhite, $B0B0B0, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, $C0C0C0, $DDD9D2);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(1, $C0C0C0, clWhite, clNone, clNone);

  //---- Elements ----//
  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, $B0B0B0, clWhite, clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, $D0D0D0, clWhite, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(2, $A0A0A0, $A0A0A0, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(0, $C0C0C0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(2, $909090, $909090, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(0, $D8D8D8, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(2, $B8B8B8, $F8F8F8, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(0, $D0D0D0, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(2, $808080, $808080, clNone, clNone);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);

  Options(skncButton, sknsNormal).Body.Fill(5, clWhite, $DADADA, $D0D0D0, $E0E0E0);
  Options(skncButton, sknsNormal).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(5, clWhite, $DADADA, $D0D0D0, $E0E0E0);
  Options(skncButton, sknsDisabled).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsHotTrack).Body.Fill(5, clWhite, $DADADA, $DFDFDF, $E0E0E0);
  Options(skncButton, sknsHotTrack).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsPushed).Body.Fill(1, $D0D0D0, clWhite, clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsChecked).Body.Fill(1, $D0D0D0, clWhite, clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(1, $EFEFEF, clWhite, clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(2, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncListItem, sknsChecked).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncButton, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncButton, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Body.Reset;
  Options(skncCheckBox, sknsChecked).Body.Fill(5, clWhite, $DADADA, $D0D0D0, $E0E0E0);
  Options(skncCheckBox, sknsDisabled).Body.Reset;

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, $E0E0E0, $E0E0E0);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, $99A8AC, $99A8AC);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, $C0C0C0, $C0C0C0);

  CopyOptions(skncButton, skncEditButton);
  Options(skncEditButton, sknsNormal).Reset;
  Options(skncEditButton, sknsDisabled).Borders.SkinType := 0;
  Options(skncEditButton, sknsHotTrack).Borders.SkinType := 0;
  Options(skncEditButton, sknsPushed).Borders.SkinType := 0;
  Options(skncEditButton, sknsChecked).Borders.SkinType := 0;
  Options(skncEditButton, sknsCheckedAndHotTrack).Borders.SkinType := 0;

  //---- Tabs ----//
  CopyOptions(skncToolbarItem, skncTab);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(2, $B8B8B8, $F8F8F8, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, $D8D8D8, clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(2, $B8B8B8, $F8F8F8, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, $C0C0C0, clNone, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(2, $909090, $909090, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(2, $909090, $909090, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, $E0E0E0, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(2, $909090, $909090, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(0, $F0F0F0, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(2, $909090, $909090, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(5, clWhite, $DADADA, $D0D0D0, $E0E0E0);
  Options(skncHeader, sknsNormal).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(5, clWhite, $DADADA, $DFDFDF, $E0E0E0);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $E7E7E7, $C0C0C0, clNone, clNone);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOfficeXPSkin }

procedure TSpTBXOfficeXPSkin.FillOptions;
var
  ToolbarC, PopupC, HotTrackC, CheckedC, EditHotTrackC: TColor;
begin
  //---- Skin Properties ----//

  SkinName := 'Office XP';
  OfficeIcons := True;
  OfficeMenu := True;
  OfficeStatusBar := True;

  //---- Colors ----//

  ToolbarC := SpBlendColors(clWindow, clBtnFace, 165);
  PopupC := SpBlendColors(clBtnFace, clWindow, 143);

  HotTrackC := SpBlendColors(clHighlight, clWindow, 30);
  HotTrackC := SpLighten(HotTrackC, -15); //-7 original, -30 silver

  CheckedC := SpBlendColors(clBtnFace, clWindow, 50);
  CheckedC := SpBlendColors(clHighlight, CheckedC, 10);

  EditHotTrackC := clHighlight;

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, PopupC, clNone, clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncDockablePanelTitleBar, sknsNormal).Borders.Fill(0, PopupC, PopupC, clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(1, clBtnFace, clWhite, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, $808080, $808080, $B0D4DF, clWhite);

  Options(skncPopup, sknsNormal).Body.Fill(0, PopupC, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(0, ToolbarC, clNone, clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(0, ToolbarC, clNone, clNone, clNone);
  Options(skncToolbar, sknsNormal).Borders.Fill(2, clBtnFace, clBtnFace, clNone, clNone);

  CopyOptions(skncToolbar, skncMenuBar);

  Options(skncWindow, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncWindow, sknsNormal).Borders.Fill(0, $808080, $808080, clBtnShadow, clBtnShadow);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(0, clBtnShadow, clNone, clNone, clNone);
  Options(skncWindowTitleBar, sknsNormal).Borders.Fill(0, clBtnFace, clBtnFace, clNone, clNone);

  //---- Elements ----//
  Options(skncGutter, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);

  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, clBtnShadow, clWhite, clNone, clNone);//////

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, clBtnShadow, clWhite, clNone, clNone);//////

  Options(skncSeparator, sknsNormal).Body.Fill(0, clBtnShadow, clNone, clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(0, CheckedC, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);

  Options(skncOpenToolbarItem, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncOpenToolbarItem, sknsNormal).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);

  CopyOptions(skncToolbarItem, skncButton);
  Options(skncButton, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);
  Options(skncButton, sknsDisabled).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, clBtnFace, clBtnFace);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, clBtnShadow, clBtnShadow);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, EditHotTrackC, EditHotTrackC);

  CopyOptions(skncToolbarItem, skncEditButton);
  Options(skncEditButton, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncEditButton, sknsNormal).Borders.Fill(0, clWindow, clWindow, clNone, clNone);
  Options(skncEditButton, sknsPushed).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncEditButton, sknsPushed).Borders.Fill(0, EditHotTrackC, EditHotTrackC, clNone, clNone);
  Options(skncEditButton, sknsPushed).TextColor := clHighlightText;

  //---- Tabs ----//
  Options(skncTab, sknsHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, clBtnShadow, clBtnShadow, clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, CheckedC, clWhite, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, CheckedC, clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(0, HotTrackC, clWhite, clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(0, clHighlight, clHighlight, clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(0, clBtnFace, clNone, clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, clWindow, clBtnShadow, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(0, HotTrackC, clNone, clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, clBtnFace, clBtnShadow, clNone, clNone);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOffice2003Skin }

constructor TSpTBXOffice2003Skin.Create;
begin
  FDefaultColorScheme := lusUnknown;
  inherited;
end;

procedure TSpTBXOffice2003Skin.FillColors;
const
  Office2003Colors: array[lusBlue..lusGreen] of TSpTBXOffice2003Colors = (
    ($F5BE9E, $F9D9C3,      // 0, 1: Dock
     $FEECDD, $E2A981,      // 2, 3: Toolbar
     $CB8C6A, $F5BE9E,      // 4, 5: Toolbar borders
     $F6F6F6,               // 6: Popup
     $962D00,               // 7: Popup borders
     $800000,               // 8: Item borders
     $C9662A,               // 9: Window borders
     $8CD5FF, $55ADFF,      // 10, 11: Item Checked
     $4E91FE, $8ED3FF,      // 12, 13: Item Pushed
     $CCF4FF, $91D0FF,      // 14, 15: Item HotTrack
     $FFEFE3, $E7B593,      // 16, 17: OpenToolbarItem
     $91420D,               // 18: Button external border
     $FBD0B3, $CB8C6A,      // 19, 20: Button internal borders
     $FEECDD,               // 21: Grip/Separator soft color
     $E2A981),              // 22: Panel/CheckBox/Tabs/ProgressBar/TrackBar borders

    ($E5D7D7, $F7F3F3,      // 0, 1: Dock
     $FAF4F3, $B59799,      // 2, 3: Toolbar
     $8F6D6E, $E5D7D7,      // 4, 5: Toolbar borders
     $FFFAFD,               // 6: Popup
     $947C7C,               // 7: Popup borders
     $6F4B4B,               // 8: Item borders
     $99797A,               // 9: Window borders
     $8CD5FF, $55ADFF,      // 10, 11: Item Checked
     $4E91FE, $8ED3FF,      // 12, 13: Item Pushed
     $CCF4FF, $91D0FF,      // 14, 15: Item HotTrack
     $F1E9E8, $CDB9BA,      // 16, 17: OpenToolbarItem
     $988B8A,               // 18: Button external border
     clWhite, $B0A7A6,      // 19, 20: Button internal borders
     $FFFFFF,               // 21: Grip/Separator soft color
     $B59799),              // 22: Panel/CheckBox/Tabs/ProgressBar/TrackBar borders

    ($A7D9D9, $E4F2F0,      // 0, 1: Dock
     $DEF7F4, $91C6B7,      // 2, 3: Toolbar
     $588060, $A7D9D9,      // 4, 5: Toolbar borders
     $EEF4F4,               // 6: Popup
     $5E8D75,               // 7: Popup borders
     $385D3F,               // 8: Item borders
     $5E8674,               // 9: Window borders
     $8CD5FF, $55ADFF,      // 10, 11: Item Checked
     $4E91FE, $8ED3FF,      // 12, 13: Item Pushed
     $CCF4FF, $91D0FF,      // 14, 15: Item HotTrack
     $D5F0EC, $9FCEC2,      // 16, 17: OpenToolbarItem
     $5B8479,               // 18: Button external border
     $E3FFFA, $86ADAA,      // 19, 20: Button internal borders
     $DEF7F4,               // 21: Grip/Separator soft color
     $91C6B7)               // 22: Panel/CheckBox/Tabs/ProgressBar/TrackBar borders
  );
var
  Luna: TSpTBXLunaScheme;
begin
  if FDefaultColorScheme <> lusUnknown then
    Luna := FDefaultColorScheme
  else
    Luna := SpGetLunaScheme;

  if Luna <> lusUnknown then
    FColors := Office2003Colors[Luna]
  else begin
    // Use adapted colors
    // 0, 1: Dock
    FColors[0] := clBtnFace;
    FColors[1] := SpBlendColors(clBtnFace, clWindow, 35);
    // 2, 3: Toolbar
    FColors[2] := SpBlendColors(clBtnFace, clWindow, 20);
    FColors[3] := SpBlendColors(clBtnFace, clWindow, 95);
    // 4, 5: Toolbar borders
    FColors[4] := SpBlendColors(clBtnShadow, clWindow, 70);
    FColors[5] := SpBlendColors(clBtnFace, clWindow, 62);
    // 6: Popup
    FColors[6] := SpBlendColors(clBtnFace, clWindow, 15);
    // 7: Popup borders
    FColors[7] := SpBlendColors(clBtnFace, clBtnShadow, 20);
    // 8: Item borders
    FColors[8] := clHighlight;
    // 9: Window borders
    FColors[9] := SpBlendColors(clBtnText, clBtnShadow, 15);
    // 10, 11: Item Checked
    FColors[10] := SpBlendColors(clHighlight, SpBlendColors(clBtnFace, clWindow, 50), 10);
    FColors[11] := FColors[10];
    // 12, 13: Item Pushed
    FColors[12] := SpBlendColors(clHighlight, clWindow, 50);
    FColors[13] := FColors[12];
    // 14, 15: Item HotTrack
    FColors[14] := SpBlendColors(clHighlight, clWindow, 30);
    FColors[15] := FColors[14];
    // 16, 17: OpenToolbarItem
    FColors[16] := SpBlendColors(clBtnFace, clWindow, 16);
    FColors[17] := SpBlendColors(clBtnFace, clWindow, 42);
    // 18: Button external border
    FColors[18] := FColors[4];
    // 19, 20: Button internal borders
    FColors[19] := FColors[5];
    FColors[20] := SpBlendColors(clBtnShadow, clWindow, 40);
    // 21: Grip/Separator soft color
    FColors[21] := clWhite;
    // 22: Panel/CheckBox/Tabs/ProgressBar/TrackBar borders
    FColors[22] := SpBlendColors(clBtnShadow, clWindow, 70);
  end;
end;

procedure TSpTBXOffice2003Skin.FillOptions;
var
  IsUnknownLuna: Boolean;
begin
  //---- Skin Properties ----//

  SkinName := 'Office 2003';
  OfficeMenu := True;

  //---- Colors ----//

  FillColors;
  IsUnknownLuna := FColors[0] = clBtnFace;

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(2, FColors[0], FColors[1], clNone, clNone);

  Options(skncDockablePanel, sknsNormal).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, FColors[3], FColors[3], clNone, clNone);

  Options(skncDockablePanelTitleBar, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncDockablePanelTitleBar, sknsNormal).Borders.Fill(0, FColors[6], FColors[6], clNone, clNone);

  Options(skncPanel, sknsNormal).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(2, FColors[22], FColors[22], clNone, clNone);

  Options(skncPopup, sknsNormal).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, FColors[7], FColors[7], clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);

  Options(skncSplitter, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);

  Options(skncToolbar, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncToolbar, sknsNormal).Borders.Fill(2, FColors[5], FColors[4], clNone, clNone);

  Options(skncWindow, sknsNormal).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncWindow, sknsNormal).Borders.Fill(0, FColors[9], FColors[9], FColors[9], FColors[9]);

  Options(skncWindowTitleBar, sknsNormal).Body.Fill(0, FColors[9], clNone, clNone, clNone);
  if IsUnknownLuna then
    Options(skncWindowTitleBar, sknsNormal).TextColor := clBtnHighlight;

  //---- Elements ----//
  Options(skncGutter, sknsNormal).Body.Fill(2, FColors[2], FColors[3], clNone, clNone);

  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, FColors[4], FColors[21], clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, FColors[4], FColors[21], clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, FColors[4], FColors[21], clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(1, FColors[14], FColors[15], clNone, clNone);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(0, FColors[8], FColors[8], clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Body.Fill(1, FColors[12], FColors[13], clNone, clNone);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(0, FColors[8], FColors[8], clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Body.Fill(1, FColors[10], FColors[11], clNone, clNone);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(0, FColors[8], FColors[8], clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(1, FColors[12], FColors[13], clNone, clNone);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(0, FColors[8], FColors[8], clNone, clNone);

  Options(skncOpenToolbarItem, sknsNormal).Body.Fill(1, FColors[16], FColors[17], clNone, clNone);
  Options(skncOpenToolbarItem, sknsNormal).Borders.Fill(0, FColors[7], FColors[7], clNone, clNone);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);
  Options(skncMenuItem, sknsChecked).Borders.Fill(0, clNone, clNone, FColors[8], FColors[8]);
  Options(skncMenuItem, sknsCheckedAndHotTrack).Borders.Fill(0, clNone, clNone, FColors[8], FColors[8]);

  Options(skncButton, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);
  Options(skncButton, sknsDisabled).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);
  Options(skncButton, sknsHotTrack).Body.Fill(1, FColors[14], FColors[15], clNone, clNone);
  Options(skncButton, sknsHotTrack).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);
  Options(skncButton, sknsPushed).Body.Fill(1, FColors[12], FColors[13], clNone, clNone);
  Options(skncButton, sknsPushed).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);
  Options(skncButton, sknsChecked).Body.Fill(1, FColors[10], FColors[11], clNone, clNone);
  Options(skncButton, sknsChecked).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);
  Options(skncButton, sknsCheckedAndHotTrack).Body.Fill(1, SpLighten(FColors[12], 15), FColors[13], clNone, clNone);
  Options(skncButton, sknsCheckedAndHotTrack).Borders.Fill(1, FColors[18], FColors[18], FColors[19], FColors[20]);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(20);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(20);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-20);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-20);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, FColors[3], FColors[3]);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, clBtnShadow, clBtnShadow);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, FColors[8], FColors[8]);

  CopyOptions(skncToolbarItem, skncEditButton);
  Options(skncEditButton, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncEditButton, sknsNormal).Borders.Fill(0, clWindow, clWindow, clNone, clNone);

  //---- Tabs ----//
  Options(skncTab, sknsHotTrack).Body.Fill(1, FColors[14], FColors[15], clNone, clNone);
  Options(skncTab, sknsHotTrack).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);
  Options(skncTab, sknsChecked).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Body.Fill(1, FColors[14], FColors[15], clNone, clNone);
  Options(skncTab, sknsCheckedAndHotTrack).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Body.Fill(0, FColors[6], clNone, clNone, clNone);
  Options(skncTabBackground, sknsNormal).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, FColors[6], clWhite, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(0, FColors[0], clNone, clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Body.Fill(0, FColors[0], clNone, clNone, clNone);
  Options(skncTrackBarButton, sknsNormal).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Body.Fill(1, FColors[12], FColors[13], clNone, clNone);
  Options(skncTrackBarButton, sknsPushed).Borders.Fill(0, FColors[22], FColors[22], clNone, clNone);

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(1, FColors[2], FColors[3], clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, FColors[19], FColors[20], clNone, clNone);
  Options(skncHeader, sknsHotTrack).Body.Fill(1, FColors[14], FColors[15], clNone, clNone);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, FColors[19], FColors[20], clNone, clNone);
end;

procedure TSpTBXOffice2003Skin.SetDefaultColorScheme(const Value: TSpTBXLunaScheme);
begin
  if FDefaultColorScheme <> Value then begin
    FDefaultColorScheme := Value;
    Reset;
    FillOptions;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOffice2007Skin }

procedure TSpTBXOffice2007Skin.FillOptions;
begin
  //---- Skin Properties ----//

  SkinName := 'Office 2007';
  OfficeMenu := True;

  //---- Colors ----//

  FillColors;

  //---- Single State ----//
  Options(skncDock, sknsNormal).Body.Fill(7, FColors[0], FColors[1], FColors[2], FColors[3]);

  Options(skncDockablePanel, sknsNormal).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncDockablePanel, sknsNormal).Borders.Fill(0, FColors[11], FColors[11], FColors[12], FColors[12]);

  Options(skncPanel, sknsNormal).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncPanel, sknsNormal).Borders.Fill(1, FColors[11], FColors[11], FColors[12], FColors[12]);

  Options(skncPopup, sknsNormal).Body.Fill(0, clWhite, clNone, clNone, clNone);
  Options(skncPopup, sknsNormal).Borders.Fill(0, $C0C0C0, $C0C0C0, clNone, clNone);

  Options(skncStatusBar, sknsNormal).Body.Fill(7, FColors[7], FColors[8], FColors[9], FColors[10]);
  Options(skncStatusBar, sknsNormal).TextColor := FColors[6];  // This overrides the Items text color

  CopyOptions(skncStatusBar, skncDockablePanelTitleBar);

  CopyOptions(skncDock, skncSplitter);

  Options(skncWindow, sknsNormal).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncWindow, sknsNormal).Borders.Fill(0, FColors[15], FColors[16], FColors[17], FColors[16]);

  CopyOptions(skncStatusBar, skncWindowTitleBar);

  //---- Elements ----//
  Options(skncGutter, sknsNormal).Body.Fill(0, $EEEEE9, clNone, clNone, clNone);
  Options(skncGutter, sknsNormal).Borders.Fill(0, $C5C5C5, clNone, clNone, clNone);

  Options(skncToolbarGrip, sknsNormal).Body.Fill(0, FColors[4], FColors[5], clNone, clNone);

  Options(skncStatusBarGrip, sknsNormal).Body.Fill(0, FColors[4], FColors[5], clNone, clNone);

  Options(skncSeparator, sknsNormal).Body.Fill(0, FColors[4], FColors[5], clNone, clNone);

  //---- Buttons ----//
  Options(skncToolbarItem, sknsHotTrack).Body.Fill(7, $DBFDFF, $95E7FF, $4CD7FF, $96E7FF);
  Options(skncToolbarItem, sknsHotTrack).Borders.Fill(1, $9BCFDD, $9BCFDD, $D2FBFF, $B3FAFF);
  Options(skncToolbarItem, sknsPushed).Body.Fill(7, $70BAFC, $59A5F3, $388FEF, $64CDFD);
  Options(skncToolbarItem, sknsPushed).Borders.Fill(1, $65818E, $AEBDC3, $66C1FA, $66C1FA);
  Options(skncToolbarItem, sknsChecked).Body.Fill(7, $A8D6FE, $61AFFC, $329BFB, $ACF0FF);
  Options(skncToolbarItem, sknsChecked).Borders.Fill(1, $65818E, $AEBDC3, $66C1FA, $66C1FA);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Body.Fill(7, $70BAFC, $59A5F3, $388FEF, $64CDFD);
  Options(skncToolbarItem, sknsCheckedAndHotTrack).Borders.Fill(1, $65818E, $AEBDC3, $66C1FA, $66C1FA);

  CopyOptions(skncToolbarItem, skncMenuBarItem);

  CopyOptions(skncToolbarItem, skncMenuItem);

  CopyOptions(skncToolbarItem, skncButton);
  Options(skncButton, sknsNormal).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncButton, sknsNormal).Borders.Fill(1, FColors[11], FColors[11], FColors[12], FColors[12]);
  Options(skncButton, sknsDisabled).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncButton, sknsDisabled).Borders.Fill(1, FColors[11], FColors[11], FColors[12], FColors[12]);

  Options(skncListItem, sknsChecked).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsCheckedAndHotTrack).Assign(Options(skncToolbarItem, sknsHotTrack));
  Options(skncListItem, sknsHotTrack).Body.Lighten(10);
  Options(skncListItem, sknsHotTrack).Borders.Lighten(10);
  Options(skncListItem, sknsCheckedAndHotTrack).Body.Lighten(-10);
  Options(skncListItem, sknsCheckedAndHotTrack).Borders.Lighten(-10);

  CopyOptions(skncToolbarItem, skncCheckBox);
  Options(skncCheckBox, sknsNormal).Borders.Fill(1, FColors[11], FColors[11], clNone, clNone);
  Options(skncCheckBox, sknsDisabled).Borders.Fill(1, FColors[11], FColors[11], clNone, clNone);

  CopyOptions(skncCheckBox, skncRadioButton);

  //---- Editors ----//
  Options(skncEditFrame, sknsNormal).Borders.Fill(1, clNone, clNone, FColors[4], FColors[4]);
  Options(skncEditFrame, sknsDisabled).Borders.Fill(1, clNone, clNone, FColors[4], FColors[4]);
  Options(skncEditFrame, sknsHotTrack).Borders.Fill(1, clNone, clNone, FColors[4], FColors[4]);

  CopyOptions(skncToolbarItem, skncEditButton);

  //---- Tabs ----//
  Options(skncTab, sknsHotTrack).Assign(Options(skncButton, sknsHotTrack));
  Options(skncTab, sknsChecked).Body.Fill(0, FColors[13], clNone, clNone, clNone);
  Options(skncTab, sknsChecked).Borders.Fill(1, FColors[11], FColors[11], FColors[12], FColors[12]);
  Options(skncTab, sknsCheckedAndHotTrack).Assign(Options(skncButton, sknsHotTrack));

  // TabBackground: Only Normal state is used
  Options(skncTabBackground, sknsNormal).Assign(Options(skncButton, sknsNormal));

  //---- ProgressBar ----//
  // ProgressBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  Options(skncProgressBar, sknsNormal).Body.Fill(0, FColors[5], clWhite, clNone, clNone);
  Options(skncProgressBar, sknsNormal).Borders.Fill(1, FColors[2], FColors[2], clNone, clNone);
  Options(skncProgressBar, sknsHotTrack).Body.Fill(7, $DBFDFF, $95E7FF, $4CD7FF, $96E7FF);
  Options(skncProgressBar, sknsHotTrack).Borders.Fill(1, FColors[2], FColors[2], clNone, clNone);

  //---- TrackBar ----//
  // TrackBar: Only Normal and HotTrack states are used
  // HotTrack represents the selection
  CopyOptions(skncProgressBar, skncTrackBar);

  // TrackBarButton: Only Normal and Pushed states are used
  Options(skncTrackBarButton, sknsNormal).Assign(Options(skncToolbarItem, sknsChecked));
  Options(skncTrackBarButton, sknsPushed).Assign(Options(skncToolbarItem, sknsPushed));

  //---- Header ----//
  Options(skncHeader, sknsNormal).Body.Fill(1, FColors[13], FColors[14], clNone, clNone);
  Options(skncHeader, sknsNormal).Borders.Fill(0, FColors[11], FColors[11], FColors[12], FColors[12]);
  Options(skncHeader, sknsHotTrack).Body.Fill(7, $DBFDFF, $95E7FF, $4CD7FF, $96E7FF);
  Options(skncHeader, sknsHotTrack).Borders.Fill(0, $7E99A9, $7E99A9, $D2FBFF, $D2FBFF);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOffice2007BlueSkin }

procedure TSpTBXOffice2007BlueSkin.FillColors;
const
  ColorConst: TSpTBXOffice2007Colors = (
     $F4E6DB, $F0DECF, $EDD9C9, $FFF2E7, // 0, 1, 2, 3: Dock
     $D6AE90, $FFFFFF,                   // 4, 5: Separator
     clNone,                             // 6: StatusBar Text Color
     $F9E6D7, $F7D7BF, $F5D0B4, $F7E5D6, // 7, 8, 9, 10: StatusBar
     $EDC4A5, $FEF6ED,                   // 11, 12: Button Borders
     $FEF9F6, $FBE7D9,                   // 13, 14: Button Normal
     $825A3B, $E1C6B1, $F7D9C2           // 15, 16, 17: Window Borders
  );
begin
  FColors := ColorConst;
  SkinName := 'Office 2007 Blue';
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOffice2007BlackSkin }

procedure TSpTBXOffice2007BlackSkin.FillColors;
const
  ColorConst: TSpTBXOffice2007Colors = (
     $DDD8D5, $CFC6C1, $C5BBB4, $EAEAEA, // 0, 1, 2, 3: Dock
     $989898, $CCCCCC,                   // 4, 5: Separator
     clWhite,                            // 6: StatusBar Text Color
     $524743, $463E3B, $30302F, $454545, // 7, 8, 9, 10: StatusBar
     $B4B0AE, $DBD4D0,                   // 11, 12: Button Borders
     $E5E0DD, $CFC6C1,                   // 13, 14: Button Normal
     $2F2F2F, $4D4D4D, $666666           // 15, 16, 17: Window Borders
  );
begin
  FColors := ColorConst;
  SkinName := 'Office 2007 Black';
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXOffice2007SilverSkin }

procedure TSpTBXOffice2007SilverSkin.FillColors;
const
  ColorConst: TSpTBXOffice2007Colors = (
     $DDD8D5, $CFC6C1, $C5BBB4, $EAEAEA, // 0, 1, 2, 3: Dock
     $AAA6A2, $FFFFFF,                   // 4, 5: Separator
     clNone,                             // 6: StatusBar Text Color
     $EBE8E7, $C2B7B1, $B6AFA9, $CEC9C7, // 7, 8, 9, 10: StatusBar
     $B4B0AE, $DBD4D0,                   // 11, 12: Button Borders
     $E5E0DD, $CFC6C1,                   // 13, 14: Button Normal
     $989898, $BABABB, $DEDDDE           // 15, 16, 17: Window Borders
  );
begin
  FColors := ColorConst;
  SkinName := 'Office 2007 Silver';
end;

initialization
  SkinManager.SkinsList.AddSkin('Aluminum', TSpTBXAluminumSkin);
  SkinManager.SkinsList.AddSkin('Athen', TSpTBXAthenSkin);
  SkinManager.SkinsList.AddSkin('Dream', TSpTBXDreamSkin);
  SkinManager.SkinsList.AddSkin('Eos', TSpTBXEosSkin);
  SkinManager.SkinsList.AddSkin('Human', TSpTBXHumanSkin);
  SkinManager.SkinsList.AddSkin('Leopard', TSpTBXLeopardSkin);
  SkinManager.SkinsList.AddSkin('Xito', TSpTBXXitoSkin);
  { Office }
  SkinManager.SkinsList.AddSkin('Office XP', TSpTBXOfficeXPSkin);
  SkinManager.SkinsList.AddSkin('Office 2003', TSpTBXOffice2003Skin);
  SkinManager.SkinsList.AddSkin('Office 2007 Blue', TSpTBXOffice2007BlueSkin);
  SkinManager.SkinsList.AddSkin('Office 2007 Black', TSpTBXOffice2007BlackSkin);
  SkinManager.SkinsList.AddSkin('Office 2007 Silver', TSpTBXOffice2007SilverSkin);

end.
