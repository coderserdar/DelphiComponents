{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_theme;

interface
{$I psc_defines.inc}

uses
  SyncObjs,
  Winapi.Windows,
  classes,
  Winapi.Messages,
  Forms,
  Graphics,

  myla_system,
  myla_interfaces,
  
  psc_wrapper,
  psc_procs,
  psc_const;

{------------------------}

procedure PSCAdjustContrast(var AColor1: Integer; AColor2: Integer; Threshold: Single);

function PSCColorDistance(AColor1, AColor2: TPSCColor): Real;
function PSCBlendColor(Alpha: Real; Color1, Color2: TPSCColor): TPSCColor;
function PSCCreateTheme_3D(AUserData:Cardinal): IPSCThemeLib;
function PSCCreateTheme_Word2000(AUserData:Cardinal): IPSCThemeLib;
function PSCCreateTheme_WordXP(AUserData:Cardinal): IPSCThemeLib;
function PSCCreateTheme_WindowsXP(AUserData:Cardinal): IPSCThemeLib;
function PSCCreateTheme_Flat(AUserData:Cardinal): IPSCThemeLib;

type
  TPSCCustomTheme = class(TPSCEvents)
  private
  protected
    procedure DrawThemeBackground(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );virtual;abstract;
    procedure DrawThemeText(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect;
      const ACaption: WideString;
      AFlags: Integer
      );virtual;abstract;
    procedure GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );virtual;abstract;
    function GetThemeBoolData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): LongBool;virtual;abstract;

    function GetThemeColorData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): TPSCColor;virtual;abstract;

    function SupportsThemeClass(AClass: TPSCThemeClass): Boolean;virtual;abstract;
  public
  end;

  TPSCDirection = (dirUp, dirDown, dirLeft, dirRight, dirNone);

  TPSCDrawThemeClass= procedure(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      ) of object;

  TPSCThemeLib3d = class(TPSCCustomTheme, IPSCThemeLib)
  private
    FFlat:Boolean;
    FHotTrack:Boolean;
    FDoubleBorder: Boolean;

    FDrawProcs: array[TPSCThemeClass] of TPSCDrawThemeClass;
    procedure DrawEdit(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawComboBox(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawSpin(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawEditBtn(
      const ACanvas:IPSCCanvas;
      const ARect: TPSCRect;
      AState: Integer;
      AArrowKind: TPSCArrowKind;
      ANotInflateDir: TPSCDirection
      );

  public
    constructor Create;
    procedure DrawThemeBackground(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );override;
    procedure DrawThemeText(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect;
      const ACaption: WideString;
      AFlags: Integer
      );override;
    procedure GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );override;
    function GetThemeBoolData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
      ): LongBool;override;
    function GetThemeColorData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): TPSCColor;override;

    function SupportsThemeClass(AClass: TPSCThemeClass): Boolean;override;

    property Flat: Boolean read FFlat Write FFlat;
    property HotTrack: Boolean read FHotTrack Write FHotTrack;
    property DoubleBorder: Boolean read FDoubleBorder write FDoubleBorder;
  end;

  TPSCThemeLibFlat = class(TPSCThemeLib3d, IPSCThemeLib)
  public
    constructor Create;
  end;

  TPSCThemeLibWord2000 = class(TPSCThemeLib3d, IPSCThemeLib)
  public
    constructor Create;

    function GetThemeBoolData(AClass: TPSCThemeClass;
      APart, AState, APropID: Integer): LongBool;override;
  end;

  TPSCThemeLibWordXP = class(TPSCCustomTheme, IPSCThemeLib)
  private
    FHotBtnBkColor: TPSCColor;
    FHotBorderColor: TPSCColor;
    FPressedBkColor: TPSCColor;
    FDisabledBorderColor: TPSCColor;
    FDrawProcs: array[TPSCThemeClass] of TPSCDrawThemeClass;
    function ProcessMsg(var Message: TMessage): Boolean;
    procedure SetContrast(var AColor1: TPSCColor; AColor2: TPSCColor; Threshold: Integer);
    procedure AdjustColors;
    procedure GetBtnColor(
      AState: Integer;
      var ABkColor, ABorderColor: TPSCColor
      );
    procedure DrawEdit(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawComboBox(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
    procedure DrawSpin(
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );
  protected
    procedure DrawArrow(
      AArrowKind: TPSCArrowKind;
      const ACanvas: IPSCCanvas;
      const ARect: TPSCRect;
      AEnabled: Boolean
      );
    property HotBtnBkColor: TPSCColor Read FHotBtnBkColor;
    property HotBorderColor: TPSCColor Read FHotBorderColor;
    property PressedBkColor: TPSCColor Read FPressedBkColor;
    property DisabledBorderColor: TPSCColor Read FDisabledBorderColor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DrawThemeBackground(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );override;
    procedure DrawThemeText(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect;
      const ACaption: WideString;
      AFlags: Integer
      );override;

    procedure GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );override;

    function GetThemeBoolData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
      ): LongBool;override;

    function GetThemeColorData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): TPSCColor;override;

    function SupportsThemeClass(AClass: TPSCThemeClass): Boolean;override;

  end;

  TPSCOpenThemeData = function(
    hwnd: HWND;
    pszClassList: LPCWSTR
    ): THandle; stdcall;

  TPSCCloseThemeData = function(
    hTheme: THandle
    ): HRESULT; stdcall;

  TPSCSetWindowTheme = function(
    hwnd: HWND;
    pszSubAppName: LPCWSTR;
    pszSubIdList: LPCWSTR
    ): HRESULT; stdcall;

  TPSCDrawThemeBackground = function(
    hTheme: THandle;
    hdc: HDC;
    iPartId, iStateId: Integer;
    const pRect: TPSCRect;
    pClipRect: PRECT
    ): HRESULT; stdcall;

  TPSCDrawThemeText = function(
    hTheme: THandle;
    hdc: HDC;
    iPartId, iStateId: Integer;
    pszText: LPCWSTR;
    iCharCount: Integer;
    dwTextFlags, dwTextFlags2: DWORD;
    const pRect: TPSCRect
    ): HRESULT; stdcall;

  TPSCSetThemeAppProperties = procedure(
    dwFlags: DWORD
    ); stdcall;

  TPSCGetWindowTheme = function(
    hwnd: HWND
    ): THandle; stdcall;

  TPSCDrawThemeEdge = function(
    hTheme: THandle;
    hdc: HDC;
    iPartId, iStateId: Integer;
    const pDestRect: TRect;
    uEdge, uFlags: UINT;
    pContentRect: PRECT
    ): HRESULT; stdcall;

  TPSCGetThemeInt = function(
    hTheme: THandle;
    iPartId, iStateId, iPropId: Integer;
    var piVal: Integer
    ): HRESULT; stdcall;

  TPScGetThemeBool = function(
    hTheme: THandle;
    iPartId, iStateId, iPropId: Integer;
    var pfVal: BOOL
    ): HRESULT; stdcall;

  TPSCGetThemeColor = function(
    hTheme: THandle;
    iPartId, iStateId, iPropId: Integer;
    var pColor: COLORREF
    ): HRESULT; stdcall;

  TPSCIsThemeActive = function: Bool; stdcall;

  TPSCThemeLibWindowsXP = class(TPSCCustomTheme, IPSCThemeLib)
  private
    FTheme3d:IPSCThemeLib;
    FThemeLib: THandle;
    FLock: TCriticalSection;
    FThemeLibObjects: array[TPSCThemeClass] of THandle;

    _OpenThemeData: TPSCOpenThemeData;
    _CloseThemeData: TPSCCloseThemeData;
    _SetWindowTheme: TPSCSetWindowTheme;
    _DrawThemeBackground: TPSCDrawThemeBackground;
    _DrawThemeText: TPSCDrawThemeText;
    _SetThemeAppProperties: TPSCSetThemeAppProperties;
    _GetWindowTheme: TPSCGetWindowTheme;
    _DrawThemeEdge: TPSCDrawThemeEdge;
    _GetThemeInt: TPSCGetThemeInt;
    _GetThemeBool: TPSCGetThemeBool;
    _GetThemeColor: TPSCGetThemeColor;
    _IsThemeActive: TPSCIsThemeActive;

    function ProcessMsg(var Message: TMessage): Boolean;
    procedure RefreshThemeData;
    function InitThemeLib: Boolean;
    procedure FreeThemeLib;
    function GetThemeObject(AClass: TPSCThemeClass): THandle;
  public
    procedure DrawThemeBackground(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect
      );override;
    procedure DrawThemeText(
      AClass: TPSCThemeClass;
      const ACanvas: IPSCCanvas;
      APart: Integer;
      AState: Integer;
      const ARect: TPSCRect;
      const ACaption: WideString;
      AFlags: Integer
      );override;

    procedure GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );override;

    function GetThemeBoolData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
      ): LongBool;override;

    function GetThemeColorData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer
    ): TPSCColor;override;

    function SupportsThemeClass(AClass: TPSCThemeClass): Boolean;override;

    constructor Create;
    destructor Destroy; override;
  end;

  TPSCThemeLib3dClass         = class of TPSCThemeLib3d;
  TPSCThemeLibWord2000Class   = class of TPSCThemeLibWord2000;
  TPSCThemeLibFlatClass       = class of TPSCThemeLibFlat;
  TPSCThemeLibWindowsXpClass  = class of TPSCThemeLibWindowsXp;
  TPSCThemeLibWordXPClass     = class of TPSCThemeLibWordXP;

var
  CPSCUsedThemeLib3d:TPSCThemeLib3dClass               = TPSCThemeLib3d;
  CPSCUsedThemeLibWord2000:TPSCThemeLibWord2000Class   = TPSCThemeLibWord2000;
  CPSCUsedThemeLibFlat:TPSCThemeLibFlatClass           = TPSCThemeLibFlat;
  CPSCUsedThemeLibWindowsXp:TPSCThemeLibWindowsXpClass = TPSCThemeLibWindowsXp;
  CPSCUsedThemeLibWordXP:TPSCThemeLibWordXPClass       = TPSCThemeLibWordXP;

{------------------------}

implementation

{------------------------}

const
  WM_THEMECHANGED = $031A;

  PSCThemeClassesNames: array[TPSCThemeClass] of WideString  = (
    'window', 'button', 'rebar', 'toolbar',
    'status', 'listview', 'header', 'progress',
    'tab', 'trackbar', 'tooltip', 'treeview',
    'spin', 'scrollbar', 'edit', 'combobox',
    'taskbar', 'taskband', 'startpanel', 'explorerbar'
  );
  SPSCThemeLibDll = 'uxtheme.dll';

const
  STAP_ALLOW_NONCLIENT   = (1 shl 0);
  STAP_ALLOW_CONTROLS    = (1 shl 1);
  STAP_ALLOW_WEBCONTENT  = (1 shl 2);

var
  GlobalThemeLib: IPSCThemeLib;

{-----------------------------------------}

constructor TPSCThemeLibWindowsXP.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  InitThemeLib;
  Application.HookMainWindow(ProcessMsg);
  FTheme3d:=PSCCreateTheme_3D(0);
end;

{-----------------------------------------}

destructor TPSCThemeLibWindowsXP.Destroy;
var
  MyIndex: TPSCThemeClass;
begin
  Application.UnhookMainWindow(ProcessMsg);
  for MyIndex:= Low(TPSCThemeClass) to High(TPSCThemeClass) do
    if(FThemeLibObjects[MyIndex] <> 0)then
      _CloseThemeData(FThemeLibObjects[MyIndex]);
  FreeThemeLib;
  FLock.Free;
  inherited;
end;

{-----------------------------------------}

procedure TPSCThemeLibWindowsXP.DrawThemeBackground(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas; APart: Integer; AState: Integer;
  const ARect: TPSCRect);
begin
  If (not _IsThemeActive) or (_DrawThemeBackground(
    GetThemeObject(AClass),
    PSCGetWindowsHandle(ACanvas),
    APart,
    AState,
    ARect,
    Nil)<>S_OK)
  then
    FTheme3d.DrawThemeBackground(AClass,ACanvas,APart,AState,ARect);
end;

{-----------------------------------------}

procedure TPSCThemeLibWindowsXP.DrawThemeText(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas;
  APart: Integer; AState: Integer; const ARect: TPSCRect;
  const ACaption: WideString; AFlags: Integer);
begin
  If (not _IsThemeActive) or
    (_DrawThemeText(
    GetThemeObject(AClass),
    PSCGetWindowsHandle(ACanvas),
    APart,
    AState,
    PWideChar(ACaption),
    Length(ACaption),
    AFlags,
    0,
    ARect)<>S_OK)
  then
    FTheme3d.DrawThemeText(AClass,ACanvas,APart,AState,ARect,ACaption,AFlags);
end;

{-----------------------------------------}

procedure TPSCThemeLibWindowsXP.FreeThemeLib;
begin
  FLock.Enter;
  try
    if(FThemeLib <> 0)then
    begin
      FreeLibrary(FThemeLib);
      _OpenThemeData := nil;
      _CloseThemeData := nil;
      _SetWindowTheme := nil;
      _DrawThemeBackground := nil;
      _DrawThemeText := nil;
      _SetThemeAppProperties := nil;
      _GetWindowTheme := nil;
      _DrawThemeEdge:= nil;
      _GetThemeInt:= nil;
      _GetThemeBool:= nil;
      _GetThemeColor:= nil;
      _IsThemeActive:= nil;
    end;
  finally
    FLock.Leave;
  end;
end;

{------------------------}

function TPSCThemeLibWindowsXP.GetThemeBoolData(AClass: TPSCThemeClass; APart,
  AState, APropID: Integer): LongBool;
begin
  if (APropID>TMT_PSCINTERNAL) and (APropID<TMT_PSCINTERNAL_MAX) then
    case APropID of
      TMT_SEPARATEBUTTONSFROMEDIT,
      TMT_SEPARATESPINBUTTONS:
        Result:= True;
      TMT_FOCUSEDEDITWHENDROPPED,
      TMT_NOHOTWHENSIBLINGFOCUSED,
      TMT_PRESSEDBTNWHENDROPPED,
      TMT_NARROWCOMBOBTN:
        Result:= False;
    else
      Result:=False;
    end
  else
    If (not _IsThemeActive) or
      (_GetThemeBool(GetThemeObject(AClass), APart, AState, APropID, Result)<>S_OK)
    then
      Result:=FTheme3d.GetThemeBoolData(AClass,APart,AState, APropID);
end;

{------------------------}

function TPSCThemeLibWindowsXP.GetThemeColorData(AClass: TPSCThemeClass;
  APart, AState, APropID: Integer): TPSCColor;
begin
  If (not _IsThemeActive) or
    (_GetThemeColor(GetThemeObject(AClass), APart, AState, APropID,
    Cardinal(Result))<>S_OK)
  then
    Result:=FTheme3d.GetThemeColorData(AClass,APart, AState, APropID);
end;

{------------------------}

procedure TPSCThemeLibWindowsXP.GetThemeIntData(
      AClass: TPSCThemeClass;
      APart, AState: Integer;
      APropID: Integer;
      var AValue: Integer
      );
begin
  If (not _IsThemeActive) or
    (_GetThemeInt(GetThemeObject(AClass), APart, AState, APropID, AValue)<>S_OK)
  then
    FTheme3d.GetThemeIntData(AClass,APart, AState,APropID,AValue);
end;

{------------------------}

function TPSCThemeLibWindowsXP.GetThemeObject(AClass: TPSCThemeClass): THandle;
begin
  if(FThemeLibObjects[AClass]=0)then
    FThemeLibObjects[AClass]:= _OpenThemeData(0, PWideChar(PSCThemeClassesNames[AClass]));
  Result:= FThemeLibObjects[AClass];
end;

{------------------------}

function TPSCThemeLibWindowsXp.InitThemeLib: Boolean;
begin
  FLock.Enter;
  try
    FThemeLib := LoadLibrary(SPSCThemeLibDll);
    if(FThemeLib<>0)then
    begin
      _OpenThemeData := GetProcAddress(FThemeLib, 'OpenThemeData');
      _CloseThemeData := GetProcAddress(FThemeLib, 'CloseThemeData');
      _SetWindowTheme := GetProcAddress(FThemeLib, 'SetWindowTheme');
      _DrawThemeBackground := GetProcAddress(FThemeLib, 'DrawThemeBackground');
      _DrawThemeText := GetProcAddress(FThemeLib, 'DrawThemeText');
      _SetThemeAppProperties := GetProcAddress(FThemeLib, 'SetThemeAppProperties');
      _GetWindowTheme:= GetProcAddress(FThemeLib, 'GetWindowTheme');
      _DrawThemeEdge:= GetProcAddress(FThemeLib, 'DrawThemeEdge');
      _GetThemeInt:= GetProcAddress(FThemeLib, 'GetThemeInt');
      _GetThemeBool:= GetProcAddress(FThemeLib, 'GetThemeBool');
      _GetThemeColor:= GetProcAddress(FThemeLib, 'GetThemeColor');
      _IsThemeActive:= GetProcAddress(FThemeLib, 'IsThemeActive');
    end;
    Result := FThemeLib > 0;
  finally
    FLock.Leave;
  end;
end;

{------------------------}

function PSCCreateTheme_WindowsXP(AUserData:Cardinal): IPSCThemeLib;
begin
  If PSCWindowsXPOrHigher then
  begin
    if(GlobalThemeLib=Nil)then
      GlobalThemeLib:= TPSCThemeLibWindowsXp.Create;
    Result:= GlobalThemeLib;
  end
  else
    Result:= PSCCreateTheme_3D(0);
end;

{------------------------}

function PSCCreateTheme_WordXP(AUserData:Cardinal): IPSCThemeLib;
begin
  Result:= TPSCThemeLibWordXP.Create;
end;

{------------------------}

function PSCCreateTheme_3D(AUserData:Cardinal): IPSCThemeLib;
begin
  Result:= TPSCThemeLib3d.Create;
end;

{------------------------}

function PSCCreateTheme_Word2000(AUserData:Cardinal): IPSCThemeLib;
begin
  Result:= TPSCThemeLibWord2000.Create;
end;

{------------------------}

function PSCCreateTheme_Flat(AUserData:Cardinal): IPSCThemeLib;
begin
  Result:= TPSCThemeLibFlat.Create;
end;

{------------------------}

function TPSCThemeLibWindowsXP.ProcessMsg(var Message: TMessage): Boolean;
begin
  if(Message.Msg = WM_THEMECHANGED)then
    RefreshThemeData;
  Result:= False;
end;

{------------------------}

procedure TPSCThemeLibWindowsXP.RefreshThemeData;
var
  MyIndex: TPSCThemeClass;
begin
  for MyIndex:= Low(TPSCThemeClass) to High(TPSCThemeClass) do
    if(FThemeLibObjects[MyIndex] <> 0)then
    begin
      _CloseThemeData(FThemeLibObjects[MyIndex]);
      FThemeLibObjects[MyIndex]:= _OpenThemeData(0, PWideChar(PSCThemeClassesNames[MyIndex]));
    end;
end;

{------------------------}

function TPSCThemeLibWindowsXP.SupportsThemeClass(AClass: TPSCThemeClass): Boolean;
begin
  Result:= True;
end;

{------------------------}

constructor TPSCThemeLibWordXP.Create;
begin
  inherited Create;
  FDrawProcs[tcEdit]:= DrawEdit;
  FDrawProcs[tcSpin]:= DrawSpin;
  FDrawProcs[tcComboBox]:= DrawComboBox;
  AdjustColors;
  Application.HookMainWindow(ProcessMsg);
end;

{------------------------}

destructor TPSCThemeLibWordXP.Destroy;
begin
  Application.UnhookMainWindow(ProcessMsg);
  inherited;
end;

{------------------------}

const
  WeightR: real = 0.764706;
  WeightG: real = 1.52941;
  WeightB: real = 0.254902;

{------------------------}

function PSCBlendColor(Alpha: Real; Color1, Color2: TPSCColor): TPSCColor;
var
  R, G, B: Integer;
begin
  R:= Round(GetRValue(Color1)*(Alpha) + GetRValue(Color2)*(1-Alpha));
  G:= Round(GetGValue(Color1)*(Alpha) + GetGValue(Color2)*(1-Alpha));
  B:= Round(GetBValue(Color1)*(Alpha) + GetBValue(Color2)*(1-Alpha));
  Result:= RGB(R, G, B);
end;

{------------------------}

function PSCColorDistance(AColor1, AColor2: TPSCColor): Real;
var
  dR, dG, dB: Integer;
begin
  dR := GetRValue(AColor1) - GetRValue(AColor2);
  Result := Sqr(dR * WeightR);
  dG := GetGValue(AColor1) - GetGValue(AColor2);
  Result := Result + Sqr(dG * WeightG);
  dB := GetBValue(AColor1) - GetBValue(AColor2);
  Result := Result + Sqr(dB * WeightB);
  Result := Sqrt(Result);
end;

{------------------------}

function PSCGetAdjustedThreshold(BkgndIntensity, Threshold: Single): Single;
begin
  if BkgndIntensity < 220 then Result := (2 - BkgndIntensity / 220) * Threshold
   else Result := Threshold;
end;

{------------------------}

function PSCIsContrastEnough(
  AColor, ABkgndColor: Integer;
  DoAdjustThreshold: Boolean;
  Threshold: Single
  ): Boolean;
begin
  if DoAdjustThreshold then
    Threshold := PSCGetAdjustedThreshold(PSCColorDistance(ABkgndColor, $000000), Threshold);
  Result := (PSCColorDistance(ABkgndColor, AColor) > Threshold);
end;

{------------------------}

procedure PSCAdjustContrast(var AColor1: Integer; AColor2: Integer; Threshold: Single);
var
  x, y, z: Real;
  r, g, b: Real;
  RR, GG, BB: Integer;
  ColorIntens1, ColorIntens2: Real;
  s, q, w: Real;
  MyInvert: Boolean;
begin
  ColorIntens1 := PSCColorDistance(AColor1, 0);
  ColorIntens2 := PSCColorDistance(AColor2, 0);
  Threshold    := PSCGetAdjustedThreshold(ColorIntens2, Threshold);

  if ColorIntens1 > ColorIntens2 then
    MyInvert := ColorIntens2 < 442 - Threshold
  else
    MyInvert := ColorIntens2 < Threshold;

  x:= GetRValue(AColor2)*WeightR;
  y:= GetGValue(AColor2)*WeightG;
  z:= GetBValue(AColor2)*WeightB;

  r:= GetRValue(AColor1)*WeightR;
  g:= GetGValue(AColor1)*WeightG;
  b:= GetBValue(AColor1)*WeightB;

  if(MyInvert)then
  begin
    r:= 195 - r;
    g:= 390 - g;
    b:= 65 - b;
    x:= 195 - x;
    y:= 390 - y;
    z:= 65 - z;
  end;

  s:= Sqrt(Sqr(b) + Sqr(g) + Sqr(r));
  if(s < 0.01)then
    s:= 0.01;
  q:= (r * x + g * y + b * z) / S;
  x:= Q / S * r - x;
  y:= Q / S * g - y;
  z:= Q / S * b - z;
  w:=  Sqrt(Sqr(Threshold) - Sqr(x) - Sqr(y) - Sqr(z));
  r:= (q - w) * r / s;
  g:= (q - w) * g / s;
  b:= (q - w) * b / s;

  if MyInvert then
  begin
    r:= 195 - r;
    g:= 390 - g;
    b:=  65 - b;
  end;

  if(r < 0)then
    r:= 0
  else
  if(r > 195)then
    r:= 195;

  if(g < 0)then
    g:= 0
  else if(g > 390)then
    g:= 390;

  if(b < 0)then
    b:= 0
  else if(b >  65)then
    b:=  65;

  RR:= Trunc(r * (1 / WeightR) + 0.5);
  GG:= Trunc(g * (1 / WeightG) + 0.5);
  BB:= Trunc(b * (1 / WeightB) + 0.5);

  if(RR > $FF)then
    RR:= 255
  else if(RR < 0)then
    RR:= 0;

  if(GG > 255)then
    GG:= 255
  else if(GG < 0)then
    GG:= 0;

  if(BB > 255)then
    BB:= 255
  else if(BB < 0)then
    BB:= 0;

  AColor1 := RGB(RR, GG, BB);
end;

{------------------------}

procedure TPSCThemeLibWordXP.SetContrast(var AColor1: TPSCColor;
  AColor2: TPSCColor; Threshold: Integer);
var
  t: Real;
begin
  t := Threshold;
  if not PSCIsContrastEnough(AColor1, AColor2, True, t) then
    PSCAdjustContrast(Integer(AColor1), AColor2, t);
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawThemeBackground(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas; APart, AState: Integer; const ARect: TPSCRect);
begin
  if Assigned(FDrawProcs[AClass]) then
    FDrawProcs[AClass](ACanvas, APart, AState, ARect);
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawThemeText(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas; APart, AState: Integer; const ARect: TPSCRect;
  const ACaption: WideString; AFlags: Integer);
begin

end;

{------------------------}

procedure TPSCThemeLibWordXP.GetThemeIntData(AClass: TPSCThemeClass; APart,
  AState, APropID: Integer; var AValue: Integer);
begin
  case APropID of
    TMT_BORDERSIZE: AValue:= 1;
  end;
end;

{------------------------}

function TPSCThemeLibWordXP.SupportsThemeClass(
  AClass: TPSCThemeClass): Boolean;
begin
  if(AClass in [tcComboBox, tcEdit, tcSpin])then
    Result:= True
  else
    Result:= False;
end;

{------------------------}

procedure TPSCThemeLibWordXP.AdjustColors;
var
  TempColor: TPSCColor;
begin
  TempColor:= PSCBlendColor(0.165, GetSysColor(COLOR_WINDOW), GetSysColor(COLOR_BTNFACE));
  FHotBtnBkColor:= PSCBlendColor(0.3, GetSysColor(COLOR_HIGHLIGHT), GetSysColor(COLOR_WINDOW));
  SetContrast(FHotBtnBkColor, TempColor, 50);
  FHotBorderColor:= GetSysColor(COLOR_HIGHLIGHT);
  SetContrast(FHotBorderColor, TempColor, 100);
  FPressedBkColor:= PSCBlendColor(0.5, GetSysColor(COLOR_HIGHLIGHT), GetSysColor(COLOR_WINDOW));
  FDisabledBorderColor:= PSCBlendColor(0.9, GetSysColor(COLOR_BTNSHADOW), GetSysColor(COLOR_WINDOW));
end;

{------------------------}

function TPSCThemeLibWordXP.ProcessMsg(var Message: TMessage): Boolean;
begin
  if(Message.Msg = WM_SYSCOLORCHANGE)then
    AdjustColors;
  Result:= False;
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawComboBox(const ACanvas: IPSCCanvas; APart,
  AState: Integer; const ARect: TPSCRect);
var
  MyBkColor: TPSCColor;
  MyBorderColor: TPSCColor;
  MyRect:TPSCRect;
begin
  GetBtnColor(AState, MyBkColor, MyBorderColor);
  with ACanvas do
  begin
    Pen.Color:=clRed;
    Brush.Color:= MyBkColor;
    FillRect(ARect);
    MyRect:=ARect;
    PSCInflateRect(MyRect, 1, 1);
    Brush.Color:= MyBorderColor;
    FrameRect(MyRect);
    DrawArrow(ArrowDown, ACanvas, MyRect, AState <> COMBOBOX_STATE_DISABLED);
  end;
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawEdit(const ACanvas: IPSCCanvas; APart,
  AState: Integer; const ARect: TPSCRect);
var
  MyBkColor: TPSCColor;
  MyBorderColor: TPSCColor;
begin
  MyBkColor:= clPSCWindow;
  MyBorderColor:= MyBkColor;
  if(APart = EDIT_PART_EDITTEXT)then
    if(AState in [EDIT_STATE_HOT, EDIT_STATE_FOCUSED])then
      MyBorderColor:= FHotBorderColor
    else if(AState = EDIT_STATE_DISABLED)then
    begin
      MyBkColor:= clPSCBtnFace;
      MyBorderColor:= FDisabledBorderColor;
    end;
  with ACanvas do
  begin
    Brush.Color:= MyBkColor;
    ACanvas.FillRect(ARect);
    ACanvas.Brush.Color:= MyBorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawSpin(
  const ACanvas: IPSCCanvas;
  APart, AState: Integer;
  const ARect: TPSCRect);
var
  MyArrKind: TPSCArrowKind;
  MyBkColor, MyBorderColor: TPSCColor;
  MyRect: TPSCRect;
begin
  MyArrKind:= ArrowDown;
  MyRect:= ARect;
  PSCInflateRect(MyRect, 1, 1);
  if(APart = SPIN_PART_UP)then
    MyArrKind:= ArrowUp
  else if(APart = SPIN_PART_DOWN)then
  begin
    MyArrKind:= ArrowDown;
    Inc(MyRect.Top);
  end;

  GetBtnColor(AState, MyBkColor, MyBOrderColor);
  with ACanvas do
  begin
    Brush.Color:= MyBkColor;
    ACanvas.FillRect(ARect);
    ACanvas.Brush.Color:= MyBorderColor;
    ACanvas.FrameRect(MyRect);
    DrawArrow(MyArrKind, ACanvas, ARect, AState <> SPIN_STATE_DISABLED);
  end;
end;

{------------------------}

procedure TPSCThemeLibWordXP.GetBtnColor(AState: Integer; var ABkColor, ABorderColor: TPSCColor);
begin
  ABkColor:= clPSCBTNFACE;
  ABorderColor:= clPSCWINDOW;
  if(AState=PUSHBUTTON_STATE_DISABLED)then
  begin
    ABorderColor:= FDisabledBorderColor;
  end
  else if(AState = PUSHBUTTON_STATE_PRESSED)then
  begin
    ABkColor:= FPressedBkColor;
    ABorderColor:= FHotBorderColor;
  end
  else if(AState = PUSHBUTTON_STATE_HOT)then
  begin
    ABkColor:= FHotBtnBkColor;
    ABorderColor:= FHotBorderColor;
  end;
end;

{------------------------}

procedure TPSCThemeLibWordXP.DrawArrow(AArrowKind: TPSCArrowKind;
  const ACanvas: IPSCCanvas; const ARect: TPSCRect; AEnabled: Boolean);
var
  MySize: Word;
  Width: Integer;
  Hight: Integer;
  MyPoints: array[0..2] of TPSCPoint;
begin
  with ARect do
  begin
    Hight:= Bottom-Top-4;
    Width:= Right-Left-4;
    if(Hight > Width)then
      MySize:= Width div 2
    else
      MySize:= Hight div 2;
    MySize:= MySize and $1E;
    if(ArrowDown = AArrowKind)then
    begin
      MyPoints[0].X:= 2 + Left + (Width - MySize)div 2;
      MyPoints[0].Y:= 2 + Top + (Hight - MySize div 2)div 2;
      MyPoints[1].X:= MyPoints[0].X + MySize;
      MyPoints[1].Y:= MyPoints[0].Y;
      MyPoints[2].X:= MyPoints[0].X + MySize div 2;
      MyPoints[2].Y:= MyPoints[0].Y + MySize div 2;
    end
    else
    begin
      MyPoints[0].X:= 2 + Left + (Width - MySize)div 2;
      MyPoints[0].Y:= 2 + Top + (Hight - MySize div 2 + MySize)div 2;
      MyPoints[1].X:= MyPoints[0].X + MySize div 2;
      MyPoints[1].Y:= MyPoints[0].Y - MySize div 2;
      MyPoints[2].X:= MyPoints[0].X + MySize;
      MyPoints[2].Y:= MyPoints[0].Y;
    end;

    if(AEnabled)then
      ACanvas.Brush.Color:= RGB(0, 0, 0)
    else
      ACanvas.Brush.Color:= FDisabledBorderColor;
    ACanvas.Pen.Color:= ACanvas.Brush.Color;
    ACanvas.Polygon(MyPoints);
  end;
end;

{------------------------}

constructor TPSCThemeLib3d.Create;
begin
  inherited Create;
  FDrawProcs[tcEdit]:= DrawEdit;
  FDrawProcs[tcSpin]:= DrawSpin;
  FDrawProcs[tcComboBox]:= DrawComboBox;
end;

{------------------------}

procedure TPSCThemeLib3d.DrawEditBtn(const ACanvas: IPSCCanvas;
  const ARect: TPSCRect; AState: Integer; AArrowKind: TPSCArrowKind;
  ANotInflateDir: TPSCDirection);
var
  R: TPSCRect;
  MyPressed: Boolean;
begin
  R:=ARect;
  MyPressed:= AState = PUSHBUTTON_STATE_PRESSED;
  ACanvas.Brush.Color:=clPSCBtnFace;
  ACanvas.Pen.Color:= clPSCWindow;
  ACanvas.Brush.Style:=BS_Solid;
  ACanvas.Pen.Style:=PS_Solid;

  if(Flat)then
    if(not DoubleBorder)then
    begin
      PSCInflateRect(R, 1, 1);
      if(ANotInflateDir = dirUp)then
      begin
        if(AState <> PUSHBUTTON_STATE_HOT)then
        begin
          ACanvas.MoveTo(R.Left, R.Top+1);
          ACanvas.LineTo(R.Right, R.Top+1);
          Inc(R.Top);
        end;
        Inc(R.Top);
      end
      else if(ANotInflateDir = dirDown)then
      begin
        Dec(R.Bottom);
      end;
    end
    else if(AState in [PUSHBUTTON_STATE_NORMAL, PUSHBUTTON_STATE_DISABLED])then
      ACanvas.Brush.Color:=clPSCWindow;
  ACanvas.FillRect(R);

  R:= ARect;
  if(MyPressed)then
    PSCOffsetRect(R,1,1);
  PSCDrawArrow(ACanvas, AArrowKind, R, AState <> PUSHBUTTON_STATE_DISABLED);

  R:=ARect;

  if Flat then
  begin
    if MyPressed then
      PSCDrawEdge(ACanvas,R,BDR_SUNKENOUTER,BF_RECT or BF_ADJUST)
    else
      if((not HotTrack) or (AState = PUSHBUTTON_STATE_HOT))then
        PSCDrawEdge(ACanvas,R,BDR_RAISEDINNER,BF_RECT or BF_ADJUST);
  end
  else
    begin
      If MyPressed then
        PSCDrawEdge(ACanvas,R,EDGE_RAISED,BF_RECT or BF_ADJUST or BF_FLAT)
      else
        PSCDrawEdge(ACanvas,R,EDGE_RAISED,BF_RECT or BF_ADJUST);
    end;
end;

{------------------------}

procedure TPSCThemeLib3d.DrawComboBox(const ACanvas: IPSCCanvas; APart,
  AState: Integer; const ARect: TPSCRect);
begin
  DrawEditBtn(ACanvas, ARect, AState, ArrowDown, dirNone);
end;

{------------------------}

procedure TPSCThemeLib3d.DrawEdit(const ACanvas: IPSCCanvas; APart, AState: Integer;
  const ARect: TPSCRect);
var
  MyRect: TPSCRect;
  MyD: Integer;
begin
  ACanvas.Brush.Color:= clPSCWindow;
  if(APart = EDIT_PART_EDITTEXT)then
    if(Flat)then
      begin
        if (not HotTrack) or (AState in [EDIT_STATE_HOT, EDIT_STATE_FOCUSED]) then
          begin
            if(DoubleBorder)then
              begin
                MyRect:=ARect;
                PSCDrawEdge(ACanvas, MyRect, BDR_SUNKEN, BF_RECT)
              end
            else
            begin
              MyRect:=ARect;
              PSCDrawEdge(ACanvas, MyRect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
              ACanvas.Pen.Color:=clPSCBtnFace;
              ACanvas.Rectangle(MyRect);
            end;
          end
        else
          begin
            if(DoubleBorder)then
              begin
                ACanvas.Pen.Color:= clPSCBtnFace;
                MyRect:= ARect;
                PSCInflateRect(MyRect, -1, -1);
                ACanvas.Rectangle(MyRect);
                MyRect:= ARect;
                PSCDrawEdge(ACanvas, MyRect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST)
              end
            else
              begin
                ACanvas.Pen.Color:=clPSCWindow;
                ACanvas.Rectangle(ARect);
              end;
          end;
      end
    else
      begin
        MyRect:= ARect;
        PSCDrawEdge(ACanvas, MyRect, BDR_SUNKEN, BF_RECT);
        MyRect:= ARect;
        GetThemeIntData(tcEdit, APart, AState, TMT_BORDERSIZE, MyD);
        PSCInflateRect(MyRect, -MyD, -MyD);
        ACanvas.Brush.Color:= GetThemeColorData(tcEdit, APart, AState, TMT_FILLCOLOR);
        ACanvas.FillRect(MyRect);
      end;
end;

{------------------------}

procedure TPSCThemeLib3d.DrawSpin(const ACanvas: IPSCCanvas; APart, AState: Integer;
  const ARect: TPSCRect);
var
  MyArrKind: TPSCArrowKind;
  MyNotInflateDir: TPSCDirection;
begin
  if(APart = SPIN_PART_UP)then
  begin
    MyArrKind:= ArrowUp;
    MyNotInflateDir:= dirDown;
  end
  else
  begin
    MyArrKind:= ArrowDown;
    MyNotInflateDir:= dirUp;
  end;
  DrawEditBtn(ACanvas, ARect, AState, MyArrKind, MyNotInflateDir);
end;

{------------------------}

procedure TPSCThemeLib3d.DrawThemeBackground(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas; APart, AState: Integer; const ARect: TPSCRect);
begin
  if Assigned(FDrawProcs[AClass]) then
    FDrawProcs[AClass](ACanvas, APart, AState, ARect);
end;

{------------------------}

procedure TPSCThemeLib3d.DrawThemeText(AClass: TPSCThemeClass;
  const ACanvas: IPSCCanvas; APart, AState: Integer; const ARect: TPSCRect;
  const ACaption: WideString; AFlags: Integer);
begin

end;

{------------------------}

procedure TPSCThemeLib3d.GetThemeIntData(AClass: TPSCThemeClass; APart,
  AState, APropID: Integer; var AValue: Integer);
begin
  case APropID of
    TMT_BORDERSIZE:
      AValue:= GetSystemMetrics(SM_CXEDGE)
  end;
end;

{------------------------}

function TPSCThemeLib3d.SupportsThemeClass(AClass: TPSCThemeClass): Boolean;
begin
  if(AClass in [tcComboBox, tcEdit, tcSpin])then
    Result:= True
  else
    Result:= False;
end;

{------------------------}

constructor TPSCThemeLibFlat.Create;
begin
  inherited;
  Flat:=True;
end;

{------------------------}

constructor TPSCThemeLibWord2000.Create;
begin
  inherited;
  Flat:=True;
  HotTrack:=True;
end;

{------------------------}

function TPSCThemeLibWord2000.GetThemeBoolData(AClass: TPSCThemeClass;
  APart, AState, APropID: Integer): LongBool;
begin
  case APropID of
    TMT_SEPARATEBUTTONSFROMEDIT,
    TMT_SEPARATESPINBUTTONS:
      Result:= False;
    TMT_FOCUSEDEDITWHENDROPPED,
    TMT_NOHOTWHENSIBLINGFOCUSED,
    TMT_PRESSEDBTNWHENDROPPED,
    TMT_NARROWCOMBOBTN:
      Result:= True;
    else
      Result:= False;
  end;
end;

{------------------------}

function TPSCThemeLibWordXP.GetThemeBoolData(AClass: TPSCThemeClass;
  APart, AState, APropID: Integer): LongBool;
begin
  case APropID of
    TMT_SEPARATEBUTTONSFROMEDIT,
    TMT_SEPARATESPINBUTTONS:
      Result:= False;
    TMT_FOCUSEDEDITWHENDROPPED,
    TMT_NOHOTWHENSIBLINGFOCUSED,
    TMT_PRESSEDBTNWHENDROPPED,
    TMT_NARROWCOMBOBTN:
      Result:= True;
    else
      Result:= False;
  end;
end;

{------------------------}

function TPSCThemeLib3d.GetThemeBoolData(AClass: TPSCThemeClass; APart,
  AState, APropID: Integer): LongBool;
begin
  case APropID of
    TMT_SEPARATEBUTTONSFROMEDIT,
    TMT_SEPARATESPINBUTTONS,
    TMT_FOCUSEDEDITWHENDROPPED,
    TMT_NOHOTWHENSIBLINGFOCUSED,
    TMT_PRESSEDBTNWHENDROPPED:
      Result:= False;
    TMT_NARROWCOMBOBTN:
      Result:= Flat and HotTrack;
    else
      Result:= False;
  end;
end;

{------------------------}

function TPSCThemeLib3d.GetThemeColorData(AClass: TPSCThemeClass; APart,
  AState, APropID: Integer): TPSCColor;
begin
  Result:= 0;
  case APropID of
    TMT_FILLCOLOR:
      begin
        if(AClass = tcEdit)then
          if(AState = EDIT_STATE_DISABLED)then
            Result:= clPSCWindow
          else
            Result:= clPSCWindow;
      end;
    TMT_TEXTCOLOR:
      begin
        if(AClass = tcEdit)then
          if(AState = EDIT_STATE_DISABLED)then
            Result:= clPSCGrayText
          else
            Result:= clPSCWindowText;
      end;
    TMT_BORDERCOLOR:
      begin
        if(AClass = tcEdit)then
          Result:= clPSCBtnFace;
      end
  end;
end;

{------------------------}

function TPSCThemeLibWordXP.GetThemeColorData(AClass: TPSCThemeClass;
  APart, AState, APropID: Integer): TPSCColor;
begin
  Result:= 0;
  case APropID of
    TMT_FILLCOLOR:
      begin
        if(AClass = tcEdit)then
          if(AState = EDIT_STATE_DISABLED)then
            Result:= clPSCBtnFace
          else
            Result:= clPSCWindow;
      end;
    TMT_TEXTCOLOR:
      begin
        if(AClass = tcEdit)then
          if(AState = EDIT_STATE_DISABLED)then
            Result:= clPSCGrayText
          else
            Result:= clPSCWindowText;
      end;
    TMT_BORDERCOLOR:
      begin
        if(AClass = tcEdit)then
          if(AState = EDIT_STATE_DISABLED)then
            Result:= FDisabledBorderColor
          else
          if AState in [EDIT_STATE_HOT, EDIT_STATE_FOCUSED] then
            Result:= FHotBorderColor
          else
            Result:= clPSCWindow;
      end;
  end;
end;

{------------------------}

end.


